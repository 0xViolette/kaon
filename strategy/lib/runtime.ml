open Language

type t =
  { steps : (unit -> unit) array
  ; input_ids : int array
  ; graph_id : int
  }

let compile (g : Graph.t) =
  (* Array of all signals that are part of G *)
  let signals = Array.of_list g.ctx.registry in
  Array.stable_sort (fun (Any l) (Any r) -> compare l.rank r.rank) signals;
  (* ids of all input signals *)
  let input_ids =
    Array.fold_left
      (fun ids (Any s) ->
         match s.def with
         | Input -> s.id :: ids
         | _ -> ids)
      []
      signals
    |> Array.of_list
  in
  (* wire each knot to the loop that owns it *)
  Array.iter
    (fun (Any s) ->
       match s.def with
       | Rec (knot, _) -> knot.knot_target <- Some s
       | _ -> ())
    signals;
  let step (Any s) =
    match s.def with
    | Undefined ->
      s.prev <- None;
      s.cur <- None;
      None
    | Const v ->
      s.prev <- Some v;
      s.cur <- Some v;
      None
    | IsPending src ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur
          <- (match src.cur with
              | Some _ -> Some false
              | None -> Some true))
    | Input -> None
    | Tick ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur
          <- (match s.cur with
              | Some n -> Some (n + 1)
              | None -> Some 1))
    | Pre src ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur <- src.prev)
    | Knot ->
      (match s.knot_target with
       | None -> failwith "compile: dangling knot (not created by loop)"
       | Some target ->
         Some
           (fun () ->
             s.prev <- s.cur;
             s.cur <- target.cur))
    | Unary (op, src) ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur
          <- (match src.cur with
              | None -> None
              | Some x -> Some (eval_unary op x)))
    | Binary (op, l, r) ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur
          <- (match l.cur, r.cur with
              | None, _ | _, None -> None
              | Some l, Some r -> Some (eval_binary op l r)))
    | Cond (pred, the, els) ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur
          <- (match pred.cur with
              | None -> None
              | Some true -> the.cur
              | Some false -> els.cur))
    | Rec (_, result) ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur <- result.cur)
    | Map (f, src) ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur
          <- (match src.cur with
              | Some x -> Some (f x)
              | None -> None))
  in
  let steps =
    List.rev
      (Array.fold_left
         (fun acc s ->
            match step s with
            | Some fn -> fn :: acc
            | None -> acc)
         []
         signals)
  in
  { steps = Array.of_list steps; input_ids; graph_id = g.id }
;;

type set = Set : 'a signal * 'a -> set

let step r setters =
  (* validate first: a failing step leaves the runtime state untouched *)
  List.iter
    (fun (Set (s, _)) ->
       if s.graph_id <> r.graph_id
       then failwith "step: signal belongs to a different graph";
       match s.def with
       | Input -> ()
       | _ -> failwith "step: cannot set non input node")
    setters;
  if
    Array.exists
      (fun id -> not (List.exists (fun (Set (s, _)) -> s.id = id) setters))
      r.input_ids
  then failwith "step: all inputs must be set";
  List.iter
    (fun (Set (s, v)) ->
       s.prev <- s.cur;
       s.cur <- Some v)
    setters;
  Array.iter (fun f -> f ()) r.steps
;;

let value r (s : 'a signal) : 'a option =
  if s.graph_id <> r.graph_id then failwith "value: signal belongs to a different graph";
  s.cur
;;
