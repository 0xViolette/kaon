open Language

(* Per-node machine op; signal state lives in flat arrays indexed by signal
   id (ids are dense per kind). Float signals live in unboxed float arrays
   with [nan] as the pending marker; effect signals live in [Obj.t] arrays
   holding the node's ['a option] erased to [Obj.t] — representation-safe,
   as an option is always immediate [None] or a [Some] block. *)
type src =
  | F of int (* float slot *)
  | E of int (* effect slot *)

type instr =
  | ITick of int (* dst slot *)
  | IPrev of
      int * int (* float_curs.(d) <- float_prevs.(s): Pre, Knot *)
  | IPrevE of int * int (* effect_curs.(d) <- effect_curs.(s): Pre *)
  | ICopy of int * int (* float_curs.(d) <- float_curs.(s): Rec *)
  | IUnbox of
      int * int (* float_curs.(d) <- effect_curs.(s); None -> nan *)
  | IFmap of int * (float -> float) * int
  | ILift2 of int * (float -> float -> float) * int * int
  | IEffect of int * (Obj.t -> Obj.t) * src
  | IEffect2 of int * (Obj.t -> Obj.t -> Obj.t) * src * src
  | ICond of int * int * int * int

type t =
  { instrs : instr array
  ; mutable float_curs : float array
  ; mutable float_prevs : float array
  ; mutable effect_curs : Obj.t array
  ; mutable effect_prevs : Obj.t array
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
       | Rec (knot, _) -> knot.knot_target <- Some (Obj.magic s)
       | _ -> ())
    signals;
  (* a float-typed operand may still be an effect-kind node (e.g. one built
     by [read]/[gmap]); route it through a coercion slot that maps [None] to
     [nan]. One slot per source, coerced before its first consumer. *)
  let extra_floats = ref 0 in
  let coercions = Hashtbl.create 8 in
  let consts = ref [] in
  let float_slot (s : float signal) =
    match s.kind with
    | Float -> s.id, []
    | Effect ->
      (match Hashtbl.find_opt coercions s.id with
       | Some slot -> slot, []
       | None ->
         let slot = g.ctx.float_counter + !extra_floats in
         incr extra_floats;
         Hashtbl.add coercions s.id slot;
         slot, [ IUnbox (slot, s.id) ])
  in
  let src_of : type a. a signal -> src =
    fun s ->
    match s.kind with
    | Float -> F s.id
    | Effect -> E s.id
  in
  let instrs =
    List.rev
      (Array.fold_left
         (fun acc (Any s) ->
            match s.def with
            | Undefined -> acc
            | Const v ->
              (* no instruction: both buffers agree, so swaps keep it constant *)
              consts := (s.id, v) :: !consts;
              acc
            | Input -> acc
            | Tick -> ITick s.id :: acc
            | Pre src ->
              (match s.kind with
               | Float -> IPrev (s.id, src.id) :: acc
               | Effect -> IPrevE (s.id, src.id) :: acc)
            | Knot ->
              (match s.knot_target with
               | None ->
                 failwith
                   "compile: dangling knot (not created by loop)"
               | Some target -> IPrev (s.id, target.id) :: acc)
            | Rec (_, result) ->
              (match result.kind with
               | Float -> ICopy (s.id, result.id) :: acc
               | Effect -> IUnbox (s.id, result.id) :: acc)
            | Fmap (f, src) ->
              let src, coerce = float_slot src in
              IFmap (s.id, f, src) :: (coerce @ acc)
            | Lift2 (f, a, b) ->
              let a, coerce_a = float_slot a in
              let b, coerce_b = float_slot b in
              ILift2 (s.id, f, a, b) :: (coerce_a @ coerce_b @ acc)
            | Effect (f, src) ->
              IEffect
                (s.id, (fun o -> Obj.repr (f (Obj.obj o))), src_of src)
              :: acc
            | Effect2 (f, a, b) ->
              IEffect2
                ( s.id
                , (fun oa ob ->
                    Obj.repr (f (Obj.obj oa) (Obj.obj ob)))
                , src_of a
                , src_of b )
              :: acc
            | Cond (c, t, e) ->
              let c, coerce_c = float_slot c in
              let t, coerce_t = float_slot t in
              let e, coerce_e = float_slot e in
              ICond (s.id, c, t, e)
              :: (coerce_c @ coerce_t @ coerce_e @ acc))
         []
         signals)
    |> Array.of_list
  in
  let float_size = g.ctx.float_counter + !extra_floats in
  let float_curs = Array.make float_size nan in
  let float_prevs = Array.make float_size nan in
  List.iter
    (fun (id, v) ->
       float_curs.(id) <- v;
       float_prevs.(id) <- v)
    !consts;
  let effect_curs = Array.make g.ctx.effect_counter (Obj.repr None) in
  let effect_prevs =
    Array.make g.ctx.effect_counter (Obj.repr None)
  in
  { instrs
  ; float_curs
  ; float_prevs
  ; effect_curs
  ; effect_prevs
  ; input_ids
  ; graph_id = g.id
  }
;;

let read_src r = function
  | F i ->
    let v = r.float_curs.(i) in
    Obj.repr (if Float.is_nan v then None else Some v)
  | E i -> r.effect_curs.(i)
;;

let exec r instr =
  match instr with
  | ITick d ->
    r.float_curs.(d)
    <- (match r.float_prevs.(d) with
        | n when Float.is_nan n -> 0.
        | n -> n +. 1.)
  | IPrev (d, s) -> r.float_curs.(d) <- r.float_prevs.(s)
  | IPrevE (d, s) -> r.effect_curs.(d) <- r.effect_prevs.(s)
  | ICopy (d, s) -> r.float_curs.(d) <- r.float_curs.(s)
  | IUnbox (d, s) ->
    r.float_curs.(d)
    <- (match (Obj.obj r.effect_curs.(s) : float option) with
        | Some v -> v
        | None -> nan)
  | IFmap (d, f, a) -> r.float_curs.(d) <- f r.float_curs.(a)
  | ILift2 (d, f, a, b) ->
    r.float_curs.(d) <- f r.float_curs.(a) r.float_curs.(b)
  | IEffect (d, f, s) -> r.effect_curs.(d) <- f (read_src r s)
  | IEffect2 (d, f, a, b) ->
    r.effect_curs.(d) <- f (read_src r a) (read_src r b)
  | ICond (d, c, t, e) ->
    r.float_curs.(d)
    <- (let c = r.float_curs.(c) in
        if Float.is_nan c
        then nan
        else if c = 0.
        then r.float_curs.(e)
        else r.float_curs.(t))
;;

type set = Set : float signal * float -> set

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
    let fc = r.float_curs in
    let ec = r.effect_curs in
    r.float_curs <- r.float_prevs;
    r.float_prevs <- fc;
    r.effect_curs <- r.effect_prevs;
    r.effect_prevs <- ec;
    List.iter (fun (Set (s, v)) -> r.float_curs.(s.id) <- v) setters;
    for i = 0 to Array.length r.instrs - 1 do
      exec r r.instrs.(i)
    done)
;;

let value : type a. t -> a signal -> a option =
  fun r s ->
  if s.graph_id <> r.graph_id
  then failwith "value: signal belongs to a different graph";
  match s.kind with
  | Effect -> Obj.obj r.effect_curs.(s.id)
  | Float ->
    let v = r.float_curs.(s.id) in
    if Float.is_nan v then None else Some v
;;
