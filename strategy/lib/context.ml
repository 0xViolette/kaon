open Language

type t =
  { mutable counter : int
  ; mutable registry : any_signal list
    (* ; cache : (signal_key, any_signal) Hashtbl.t *)
  }

let create () =
  { counter = 0; registry = [] (*cache = Hashtbl.create 16*) }
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
  | Lift2 (_, a, b) ->
    check a;
    check b
;;

let rec rank_of : type a. a def -> int = function
  | Undefined | Const _ | Input | Tick | Knot -> 0
  | Pre s -> 1 + s.rank
  | Fmap (_, s) -> 1 + s.rank
  | Lift2 (_, l, r) -> 1 + Int.max l.rank r.rank
  | Rec (_, result) -> 1 + result.rank
;;

let make t graph_id def =
  check_operands graph_id def;
  let fresh ?key () =
    let signal =
      { id = t.counter
      ; graph_id
      ; rank = rank_of def
      ; def
      ; cur = None
      ; prev = None
      ; knot_target = None
      ; pre_child = None
      }
    in
    t.counter <- t.counter + 1;
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
