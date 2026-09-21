open Language

type t =
  { mutable counter : int
  ; mutable registry : any_signal list
  ; cache : (signal_key, any_signal) Hashtbl.t
  }

let create () = { counter = 0; registry = []; cache = Hashtbl.create 16 }

(* a cached signal is reused only when its own definition proves it has the
   requested type: matching the operator against the stored definition in a
   single branch forces both sides to agree on the result type *)
let cast_unary : type a b. (a, b) unary -> any_signal -> b signal option =
  fun op (Any s) ->
  match op, s.def with
  | Neg, Unary (Neg, _) -> Some s
  | Abs, Unary (Abs, _) -> Some s
  | Sqrt, Unary (Sqrt, _) -> Some s
  | Not, Unary (Not, _) -> Some s
  | _ -> None
;;

let cast_binary : type a b c. (a, b, c) binary -> any_signal -> c signal option =
  fun op (Any s) ->
  match op, s.def with
  | Add, Binary (Add, _, _) -> Some s
  | Sub, Binary (Sub, _, _) -> Some s
  | Mul, Binary (Mul, _, _) -> Some s
  | Div, Binary (Div, _, _) -> Some s
  | Eq, Binary (Eq, _, _) -> Some s
  | Neq, Binary (Neq, _, _) -> Some s
  | Lt, Binary (Lt, _, _) -> Some s
  | Le, Binary (Le, _, _) -> Some s
  | Gt, Binary (Gt, _, _) -> Some s
  | Ge, Binary (Ge, _, _) -> Some s
  | Min, Binary (Min, _, _) -> Some s
  | Max, Binary (Max, _, _) -> Some s
  | And, Binary (And, _, _) -> Some s
  | Or, Binary (Or, _, _) -> Some s
  | _ -> None
;;

(* a node may only reference signals of its own graph; checked before the
   cache so a foreign reference cannot hash-cons into a local signal *)
let check_operands : type a. int -> a def -> unit =
  fun graph_id def ->
  let check s =
    if s.graph_id <> graph_id then failwith "make: operand belongs to a different graph"
  in
  match def with
  | Undefined | Const _ | Input | Knot | Tick -> ()
  | Pre src -> check src
  | Unary (_, src) -> check src
  | IsPending src -> check src
  | Binary (_, l, r) ->
    check l;
    check r
  | Cond (pred, t, e) ->
    check pred;
    check t;
    check e
  | Rec (knot, result) ->
    check knot;
    check result
  | Map (_, src) -> check src
;;

let make t graph_id rank def =
  check_operands graph_id def;
  let fresh ?key () =
    let signal =
      { id = t.counter
      ; graph_id
      ; rank
      ; def
      ; cur = None
      ; prev = None
      ; knot_target = None
      ; pre_child = None
      }
    in
    t.counter <- t.counter + 1;
    t.registry <- Any signal :: t.registry;
    (match key with
     | Some key -> Hashtbl.replace t.cache key (Any signal)
     | None -> ());
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
  | Unary (op, src) ->
    let key = KUnary (op, src.id) in
    (match Hashtbl.find_opt t.cache key with
     | Some any ->
       (match cast_unary op any with
        | Some signal -> signal
        | None -> fresh ~key ())
     | None -> fresh ~key ())
  | Binary (op, l, r) ->
    let key = KBinary (op, l.id, r.id) in
    (match Hashtbl.find_opt t.cache key with
     | Some any ->
       (match cast_binary op any with
        | Some signal -> signal
        | None -> fresh ~key ())
     | None -> fresh ~key ())
  | _ -> fresh ()
;;
