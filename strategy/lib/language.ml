(*
A signal can only be float or bool [numerical stream or logical stream]
*)

type (_, _, _) binary =
  (*------------arithmetic-------------*)
  | Add : (float, float, float) binary
  | Sub : (float, float, float) binary
  | Mul : (float, float, float) binary
  | Div : (float, float, float) binary
  (*-----------equivalence-------------*)
  | Eq : ('a, 'a, bool) binary
  | Neq : ('a, 'a, bool) binary
  (*-----------relational--------------*)
  | Lt : (float, float, bool) binary
  | Le : (float, float, bool) binary
  | Gt : (float, float, bool) binary
  | Ge : (float, float, bool) binary
  (*-----------extremum----------------*)
  | Min : (float, float, float) binary
  | Max : (float, float, float) binary
  (*-----------logical-----------------*)
  | And : (bool, bool, bool) binary
  | Or : (bool, bool, bool) binary

type 'a signal =
  { id : int
  ; graph_id : int
  ; rank : int
  ; def : 'a def
  ; (* per-bar runtime state, kept on the node itself so it stays typed *)
    mutable cur : 'a option
  ; mutable prev : 'a option
  ; (* a knot is wired to the loop that owns it when the graph is compiled *)
    mutable knot_target : 'a signal option
  ; (* hash-cons slot: a node has at most one [Pre] child *)
    mutable pre_child : 'a signal option
  }

and (_, _) unary =
  | Neg : (float, float) unary
  | Abs : (float, float) unary
  | Sqrt : (float, float) unary
  | Not : (bool, bool) unary
  | Floor : (float, float) unary
  | Ceil : (float, float) unary

and _ def =
  | Undefined : 'a def
  | Const : 'a -> 'a def
  | IsPending : 'a signal -> bool def
  | Tick : int def
  | Input : 'a def
  | Pre : 'a signal -> 'a def
  | Unary : ('a, 'b) unary * 'a signal -> 'b def
  | Binary : ('a, 'b, 'c) binary * 'a signal * 'b signal -> 'c def
  | Knot : 'a def
  | Rec : 'a signal * 'a signal -> 'a def
  | Fmap : ('a option -> 'b option) * 'a signal -> 'b def
  | Lift2 : ('a option -> 'b option -> 'c option) * 'a signal * 'b signal -> 'c def

type signal_key =
  | KUnary : ('a, 'b) unary * int -> signal_key
  | KBinary : ('a, 'b, 'c) binary * int * int -> signal_key

type any_signal = Any : 'a signal -> any_signal [@@unboxed]

let eval_unary : type a b. (a, b) unary -> a -> b =
  fun op x ->
  match op with
  | Neg -> Float.neg x
  | Abs -> Float.abs x
  | Sqrt -> Float.sqrt x
  | Not -> Bool.not x
  | Floor -> Float.floor x
  | Ceil -> Float.ceil x
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
