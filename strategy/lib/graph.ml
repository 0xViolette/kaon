type t =
  { ctx : Context.t
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

let create () = { ctx = Context.create (); compiled = false; id = fresh_token () }

open Language

let make g rank def =
  if not g.compiled
  then Context.make g.ctx g.id rank def
  else failwith "Cannot mess with a compiled graph"
;;

let const g v = make g 0 (Const v)
let input g = make g 0 Input

(* constant folding: an operation on constants is itself a constant *)
let unary =
  fun op g a ->
  match a.def with
  | Const x -> const g (eval_unary op x)
  | _ -> make g (a.rank + 1) (Unary (op, a))
;;

let binary =
  fun op g a b ->
  match a.def, b.def with
  | Const x, Const y -> const g (eval_binary op x y)
  | _ -> make g (1 + max a.rank b.rank) (Binary (op, a, b))
;;

(*unary operations*)
let neg = unary Neg
let abs = unary Abs
let sqrt = unary Sqrt

(*binary operations*)
let add = binary Add
let sub = binary Sub
let mul = binary Mul
let div = binary Div
let eq g a b = binary Eq g a b
let neq g a b = binary Neq g a b
let lt = binary Lt
let le = binary Le
let gt = binary Gt
let ge = binary Ge
let min = binary Min
let max = binary Max
let and_ = binary And
let or_ = binary Or

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

let cond g p t e = make g (1 + Int.max p.rank (Int.max t.rank e.rank)) (Cond (p, t, e))

let loop g ~(init : 'a init) (body : 'a Language.signal -> 'a Language.signal)
  : 'a Language.signal
  =
  (match init with
   | Nodes nodes ->
     if Array.length nodes = 0 then invalid_arg "loop: init nodes cannot be empty"
   | Node (_, len) -> if len <= 0 then invalid_arg "loop: init length must be positive");
  let initial_rank =
    match init with
    | Nodes nodes ->
      Array.fold_left (fun max_rank node -> Int.max max_rank node.rank) 0 nodes
    | Node (node, _) -> node.rank
  in
  let knot = make g 0 Knot in
  let result = body knot in
  make g (1 + Int.max result.rank initial_rank) (Loop (init, knot, result))
;;
