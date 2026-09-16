(*
A signal can only be float or bool [numerical stream or logical stream]
*)
type (_, _) unary =
  | Neg : (float, float) unary
  | Abs : (float, float) unary
  | Sqrt : (float, float) unary

type (_, _, _) binary =
  | Add : (float, float, float) binary
  | Sub : (float, float, float) binary
  | Mul : (float, float, float) binary
  | Div : (float, float, float) binary
  | Eq : ('a, 'a, bool) binary
  | Neq : ('a, 'a, bool) binary
  | Lt : (float, float, bool) binary
  | Le : (float, float, bool) binary
  | Gt : (float, float, bool) binary
  | Ge : (float, float, bool) binary
  | Min : (float, float, float) binary
  | Max : (float, float, float) binary
  | And : (bool, bool, bool) binary
  | Or : (bool, bool, bool) binary

type 'a signal =
  { id : int
  ; graph_id : int
  ; rank : int
  ; def : 'a def
  }

and _ def =
  | Const : 'a -> 'a def
  | Input : 'a def
  | Pre : 'a signal -> 'a def
  | Unary : ('a, 'b) unary * 'a signal -> 'b def
  | Binary : ('a, 'b, 'c) binary * 'a signal * 'b signal -> 'c def
  | Cond : bool signal * 'a signal * 'a signal -> 'a def
  | Knot : 'a def
  | Loop : 'a init * 'a signal * 'a signal -> 'a def

and 'a init =
  | Nodes of 'a signal array
  | Node of 'a signal * int

type signal_key =
  | KPre of int
  | KUnary of int * int
  | KBinary of int * int * int

type any_signal = Any : 'a signal -> any_signal [@@unboxed]

let eval_unary : type a b. (a, b) unary -> a -> b =
  fun op x ->
  match op with
  | Neg -> Float.neg x
  | Abs -> Float.abs x
  | Sqrt -> Float.sqrt x
;;

let eval_binary : type a b c. (a, b, c) binary -> a -> b -> c =
  fun op x y ->
  match op with
  | Add -> Float.add x y
  | Sub -> Float.sub x y
  | Mul -> Float.mul x y
  | Div -> Float.div x y
  | Eq -> x = y
  | Neq -> x <> y
  | Lt -> x < y
  | Le -> x <= y
  | Gt -> x > y
  | Ge -> x >= y
  | Min -> Float.min x y
  | Max -> Float.max x y
  | And -> Bool.( && ) x y
  | Or -> Bool.( || ) x y
;;
