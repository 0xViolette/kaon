open Language

type t =
  { mutable float_counter : int
  ; mutable effect_counter : int
  ; mutable registry : any_signal list
    (* ; cache : (signal_key, any_signal) Hashtbl.t *)
  }

let create () =
  { float_counter = 0
  ; effect_counter = 0
  ; registry = [] (*cache = Hashtbl.create 16*)
  }
;;

(* a node may only reference signals of its own graph; checked before the
   cache so a foreign reference cannot hash-cons into a local signal *)
let check_operands : type a. int -> a def -> unit =
  fun graph_id def ->
  let check s =
    if s.graph_id <> graph_id
    then failwith "make: operand belongs to a different graph"
  in
  match def with
  | Undefined | Const _ | Input | Knot | Tick -> ()
  | Pre src -> check src
  | Rec (knot, result) ->
    check knot;
    check result
  | Fmap (_, src) -> check src
  | Effect (_, src) -> check src
  | Effect2 (_, a, b) ->
    check a;
    check b
  | Lift2 (_, a, b) ->
    check a;
    check b
  | Cond (c, t, e) ->
    check c;
    check t;
    check e
;;

let rec rank_of : type a. a def -> int = function
  | Undefined | Const _ | Input | Tick | Knot -> 0
  | Pre s -> 1 + s.rank
  | Fmap (_, s) -> 1 + s.rank
  | Lift2 (_, l, r) -> 1 + Int.max l.rank r.rank
  | Effect2 (_, l, r) -> 1 + Int.max l.rank r.rank
  | Rec (_, result) -> 1 + result.rank
  | Cond (c, t, e) -> 1 + max c.rank (max t.rank e.rank)
  | Effect (_, s) -> 1 + s.rank
;;

let make t graph_id =
  fun (type a)
    (def : a def)
    (kind : a Language.signal_kind)
    : a signal ->
  check_operands graph_id def;
  let fresh ?key () =
    let new_id =
      match kind with
      | Effect ->
        t.effect_counter <- t.effect_counter + 1;
        t.effect_counter - 1
      | Float ->
        t.float_counter <- t.float_counter + 1;
        t.float_counter - 1
    in
    let signal =
      { id = new_id
      ; kind
      ; graph_id
      ; rank = rank_of def
      ; def
      ; knot_target = None
      ; pre_child = None
      }
    in
    t.registry <- Any signal :: t.registry;
    (* (match key with
     | Some key -> Hashtbl.replace t.cache key (Any signal)
     | None -> ()); *)
    signal
  in
  match def with
  | Pre src ->
    (match src.pre_child with
     | Some signal -> signal
     | None ->
       let signal = fresh () in
       src.pre_child <- Some signal;
       signal)
  | _ -> fresh ()
;;
