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

let undefined g = make g 0 Undefined
let const g v = make g 0 (Const v)
let input g = make g 0 Input
let tick g = make g 0 Tick

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
let not = unary Not
let floor = unary Floor
let ceil = unary Ceil

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

let cond g p t e = make g (1 + (Int.max p.rank @@ Int.max t.rank e.rank)) (Cond (p, t, e))
let is_pending g s = make g (1 + s.rank) (IsPending s)

(* [loop ~init body]: on bar i < length(init) the loop copies init.(i)
   *verbatim* (Some or None — exactly what the source holds that bar);
   from bar length(init) on it takes the value of [body knot], where [knot]
   holds the loop's value from the previous bar. init = [||] starts the
   body on bar 0. A None handed to the recursion is usually permanent:
   seed with values that are valid at the bar they are consumed. *)
let recurrence g (body : 'a Language.signal -> 'a Language.signal) : 'a Language.signal =
  let knot = make g 0 Knot in
  let result = body knot in
  make g (1 + result.rank) (Rec (knot, result))
;;

let map g f a = make g (1 + a.rank) (Language.Map (f, a))
