open Language

(* Per-node machine op; signal state lives in flat arrays indexed by signal
   id (ids are dense per graph). Cells hold the node's ['a option] erased to
   Obj.t — representation-safe, as an option is always immediate [None] or a
   [Some] block. *)
type instr =
  | ITick of int (* dst slot *)
  | IPrev of int * int (* curs.(d) <- prevs.(s): Pre, Knot *)
  | ICopy of int * int (* curs.(d) <- curs.(s): Rec *)
  | IFmap of int * (Obj.t -> Obj.t) * int
  | ILift2 of int * (Obj.t -> Obj.t -> Obj.t) * int * int

type t =
  { instrs : instr array
  ; mutable curs : Obj.t array
  ; mutable prevs : Obj.t array
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
  let curs = Array.make g.ctx.counter (Obj.repr None) in
  let prevs = Array.make g.ctx.counter (Obj.repr None) in
  let instrs =
    List.rev
      (Array.fold_left
         (fun acc (Any s) ->
            match s.def with
            | Undefined -> acc
            | Const v ->
              (* no instruction: both buffers agree, so swaps keep it constant *)
              curs.(s.id) <- Obj.repr (Some v);
              prevs.(s.id) <- Obj.repr (Some v);
              acc
            | Input -> acc
            | Tick -> ITick s.id :: acc
            | Pre src -> IPrev (s.id, src.id) :: acc
            | Knot ->
              (match s.knot_target with
               | None ->
                 failwith "compile: dangling knot (not created by loop)"
               | Some target -> IPrev (s.id, target.id) :: acc)
            | Rec (_, result) -> ICopy (s.id, result.id) :: acc
            | Fmap (f, src) ->
              IFmap (s.id, (Obj.magic f : Obj.t -> Obj.t), src.id) :: acc
            | Lift2 (f, a, b) ->
              ILift2
                (s.id, (Obj.magic f : Obj.t -> Obj.t -> Obj.t), a.id, b.id)
              :: acc)
         []
         signals)
    |> Array.of_list
  in
  { instrs; curs; prevs; input_ids; graph_id = g.id }
;;

let exec r instr =
  match instr with
  | ITick d ->
    r.curs.(d)
    <- (match (Obj.obj r.prevs.(d) : int option) with
        | Some n -> Obj.repr (Some (n + 1))
        | None -> Obj.repr (Some 0))
  | IPrev (d, s) -> r.curs.(d) <- r.prevs.(s)
  | ICopy (d, s) -> r.curs.(d) <- r.curs.(s)
  | IFmap (d, f, a) -> r.curs.(d) <- f r.curs.(a)
  | ILift2 (d, f, a, b) -> r.curs.(d) <- f r.curs.(a) r.curs.(b)
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
  else (
    (* rotate buffers: every node's last-bar cur becomes its prev *)
    let c = r.curs in
    r.curs <- r.prevs;
    r.prevs <- c;
    List.iter
      (fun (Set (s, v)) -> r.curs.(s.id) <- Obj.repr (Some v))
      setters;
    for i = 0 to Array.length r.instrs - 1 do
      exec r r.instrs.(i)
    done)
;;

let value r (s : 'a signal) : 'a option =
  if s.graph_id <> r.graph_id
  then failwith "value: signal belongs to a different graph";
  Obj.obj r.curs.(s.id)
;;
