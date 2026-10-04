open Language

type t =
  { steps : (unit -> unit) array
  ; input_ids : int array
  ; graph_id : int
  }

let compile (g : Graph.t) =
  (* Array of all signals that are part of G *)
  let signals = Array.of_list g.ctx.registry in
  Array.stable_sort
    (fun (Any l) (Any r) -> compare l.rank r.rank)
    signals;
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
              | None -> Some 0))
    | Pre src ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur <- src.prev)
    | Knot ->
      (match s.knot_target with
       | None ->
         failwith "compile: dangling knot (not created by loop)"
       | Some target ->
         Some
           (fun () ->
             s.prev <- s.cur;
             s.cur <- target.cur))
    | Cond (c, t, e) ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur
          <- (match c.cur with
              | Some true -> t.cur
              | Some false -> e.cur
              | _ -> None))
    | Rec (_, result) ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur <- result.cur)
    | Fmap (f, src) ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur
          <- (match src.cur with
              | Some x -> Some (f x)
              | None -> None))
    | Reader f ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur <- f ())
    | Lift2 (f, a, b) ->
      Some
        (fun () ->
          s.prev <- s.cur;
          s.cur
          <- (match a.cur, b.cur with
              | Some x, Some y -> Some (f x y)
              | _ -> None))
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
      (fun id ->
         not (List.exists (fun (Set (s, _)) -> s.id = id) setters))
      r.input_ids
  then failwith "step: all inputs must be set";
  if Array.length r.input_ids <> List.length setters
  then failwith "step: an input is set more than once"
  else
    List.iter
      (fun (Set (s, v)) ->
         s.prev <- s.cur;
         s.cur <- Some v)
      setters;
  Array.iter (fun f -> f ()) r.steps
;;

let value r (s : 'a signal) : 'a option =
  if s.graph_id <> r.graph_id
  then failwith "value: signal belongs to a different graph";
  s.cur
;;
