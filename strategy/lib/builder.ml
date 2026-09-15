open Language

type t =
  { mutable counter : int
  ; mutable registry : any_signal list
  ; cache : (signal_key, any_signal) Hashtbl.t
  }

let create () = { counter = 0; registry = []; cache = Hashtbl.create 16 }

let unary_tag : type a b. (a, b) unary -> int = function
  | Int_neg -> 0
  | Float_neg -> 1
;;

let binary_tag : type a b c. (a, b, c) binary -> int = function
  | Int_add -> 0
  | Int_sub -> 1
  | Int_mul -> 2
  | Int_div -> 3
  | Float_add -> 4
  | Float_sub -> 5
  | Float_mul -> 6
  | Float_div -> 7
  | Int_lt -> 8
  | Float_lt -> 9
;;

let key_of_def : type a. a def -> signal_key option = function
  | Pre src -> Some (KPre src.id)
  | Unary (op, src) -> Some (KUnary (unary_tag op, src.id))
  | Binary (op, l, r) -> Some (KBinary (binary_tag op, l.id, r.id))
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
  | Const _ | Input | Knot -> ()
  | Pre src -> check src
  | Unary (_, src) -> check src
  | Binary (_, l, r) ->
    check l;
    check r
  | Loop (init, knot, result) ->
    check knot;
    check result;
    (match init with
     | Nodes sources -> Array.iter check sources
     | Node (source, _) -> check source)
;;

let make t graph_id rank def =
  check_operands graph_id def;
  match key_of_def def with
  | Some key ->
    (match Hashtbl.find_opt t.cache key with
     | Some (Any signal) -> Obj.magic signal
     | None ->
       let signal = { id = t.counter; graph_id; rank; def } in
       t.counter <- t.counter + 1;
       t.registry <- Any signal :: t.registry;
       Hashtbl.replace t.cache key (Any signal);
       signal)
  | None ->
    let signal = { id = t.counter; graph_id; rank; def } in
    t.counter <- t.counter + 1;
    t.registry <- Any signal :: t.registry;
    signal
;;
