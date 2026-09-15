type t =
  { builder : Builder.t
  ; mutable compiled : bool
  ; id : int
  }

let fresh_token =
  let c = ref 0 in
  fun () ->
    let token = !c in
    incr c;
    token
;;

let create () = { builder = Builder.create (); compiled = false; id = fresh_token () }

open Language

let make g rank def =
  if not g.compiled
  then Builder.make g.builder g.id rank def
  else failwith "Cannot mess with a compiled graph"
;;

let const g v = make g 0 (Const v)
let input g = make g 0 Input

(* constant folding: an operation on constants is itself a constant *)
let unary : type a b. t -> (a, b) unary -> a signal -> b signal =
  fun g op a ->
  match a.def with
  | Const x -> const g (eval_unary op x)
  | _ -> make g (a.rank + 1) (Unary (op, a))
;;

let binary : type a b c. t -> (a, b, c) binary -> a signal -> b signal -> c signal =
  fun g op a b ->
  match a.def, b.def with
  | Const x, Const y -> const g (eval_binary op x y)
  | _ -> make g (1 + max a.rank b.rank) (Binary (op, a, b))
;;

let int_add g a b = binary g Int_add a b
let int_sub g a b = binary g Int_sub a b
let int_mul g a b = binary g Int_mul a b
let int_div g a b = binary g Int_div a b
let int_lt g a b = binary g Int_lt a b
let float_add g a b = binary g Float_add a b
let float_sub g a b = binary g Float_sub a b
let float_mul g a b = binary g Float_mul a b
let float_div g a b = binary g Float_div a b
let float_lt g a b = binary g Float_lt a b
let int_neg g a = unary g Int_neg a
let float_neg g a = unary g Float_neg a

let pre g a =
  match a.def with
  | Const _ -> a
  | _ -> make g (a.rank + 1) (Pre a)
;;

let rec delay g n node =
  match n with
  | 0 -> node
  | n when n > 0 -> delay g (n - 1) (pre g node)
  | _ -> invalid_arg "delay: cannot be negative"
;;

let rec window g length node =
  if length < 0 then invalid_arg "window: lookback cannot be negative";
  if length = 0 then [] else node :: window g (length - 1) (pre g node)
;;

let loop g ~(init : 'a init) (body : 'a Language.signal -> 'a Language.signal)
  : 'a Language.signal
  =
  (match init with
   | Nodes nodes ->
     if Array.length nodes = 0 then invalid_arg "loop: init nodes cannot be empty"
   | Node (_, len) -> if len <= 0 then invalid_arg "loop: init length must be positive");
  let initial_rank =
    match init with
    | Nodes nodes -> Array.fold_left (fun max_rank node -> max max_rank node.rank) 0 nodes
    | Node (node, _) -> node.rank
  in
  let knot = make g 0 Knot in
  let result = body knot in
  make g (1 + max result.rank initial_rank) (Loop (init, knot, result))
;;
