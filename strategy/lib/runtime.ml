open Language

type t =
  { steps : (unit -> unit) array
  ; cur : Obj.t array
  ; prev : Obj.t array
  ; input_ids : int array
  ; size : int
  ; graph_id : int
  }

let get id r =
  if id < r.size then Obj.magic r.cur.(id) else failwith "get: node with id doesn't exist"
;;

let compile (g : Graph.t) =
  (* Array of all signals that are part of G *)
  let signals = Array.of_list g.builder.registry in
  Array.stable_sort (fun (Any l) (Any r) -> compare l.rank r.rank) signals;
  (* size of the graph = number of signals present in the graph *)
  let size = Array.length signals in
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
  (* knot_target maps a knot id to the id of its loop signal *)
  let knot_target = Array.make size (-1) in
  Array.iter
    (fun (Any s) ->
       match s.def with
       | Loop (_, knot, _) -> knot_target.(knot.id) <- s.id
       | _ -> ())
    signals;
  (* this is the runtime instantiation for G which will be returned *)
  let t =
    { steps = [||]
    ; cur = Array.make size (Obj.repr None)
    ; prev = Array.make size (Obj.repr None)
    ; input_ids
    ; size
    ; graph_id = g.id
    }
  in
  let step (Any s) =
    match s.def with
    | Const v ->
      t.prev.(s.id) <- Obj.repr (Some v);
      t.cur.(s.id) <- Obj.repr (Some v);
      None
    | Input -> None
    | Pre src ->
      Some
        (fun () ->
          t.prev.(s.id) <- t.cur.(s.id);
          t.cur.(s.id) <- t.prev.(src.id))
    | Knot ->
      let target = knot_target.(s.id) in
      if target < 0 then failwith "compile: dangling knot (not created by loop)";
      Some
        (fun () ->
          t.prev.(s.id) <- t.cur.(s.id);
          t.cur.(s.id) <- t.cur.(target))
    | Unary (op, src) ->
      Some
        (fun () ->
          t.prev.(s.id) <- t.cur.(s.id);
          t.cur.(s.id)
          <- (match get src.id t with
              | Some x -> Obj.repr (Some (eval_unary op x))
              | _ -> Obj.repr None))
    | Binary (op, l, r) ->
      Some
        (fun () ->
          t.prev.(s.id) <- t.cur.(s.id);
          t.cur.(s.id)
          <- (match get l.id t, get r.id t with
              | Some l, Some r -> Obj.repr (Some (eval_binary op l r))
              | _ -> Obj.repr None))
    | Loop (init, _, result) ->
      let index = ref 0 in
      (match init with
       | Nodes sources ->
         Some
           (fun () ->
             t.prev.(s.id) <- t.cur.(s.id);
             if !index < Array.length sources
             then (
               t.cur.(s.id) <- t.cur.(sources.(!index).id);
               incr index)
             else t.cur.(s.id) <- t.cur.(result.id))
       | Node (source, len) ->
         Some
           (fun () ->
             t.prev.(s.id) <- t.cur.(s.id);
             if !index < len
             then (
               t.cur.(s.id) <- t.cur.(source.id);
               incr index)
             else t.cur.(s.id) <- t.cur.(result.id)))
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
  { t with steps = Array.of_list steps }
;;

type set = Set : 'a signal * 'a -> set

let tick r setters =
  (* validate first: a failing tick leaves the runtime state untouched *)
  List.iter
    (fun (Set (s, _)) ->
       if s.graph_id <> r.graph_id
       then failwith "tick: signal belongs to a different graph";
       match s.def with
       | Input -> ()
       | _ -> failwith "tick: cannot set non input node")
    setters;
  if
    Array.exists
      (fun id -> not (List.exists (fun (Set (s, _)) -> s.id = id) setters))
      r.input_ids
  then failwith "tick: all inputs must be set";
  List.iter
    (fun (Set (s, v)) ->
       r.prev.(s.id) <- r.cur.(s.id);
       r.cur.(s.id) <- Obj.repr (Some v))
    setters;
  Array.iter (fun step -> step ()) r.steps
;;

let value r (s : 'a signal) : 'a option =
  if s.graph_id <> r.graph_id then failwith "value: signal belongs to a different graph";
  get s.id r
;;
