type (_, _) unary =
  | Int_neg : (int, int) unary
  | Float_neg : (float, float) unary

type (_, _, _) binary =
  | Int_add : (int, int, int) binary
  | Int_sub : (int, int, int) binary
  | Int_mul : (int, int, int) binary
  | Int_div : (int, int, int) binary
  | Float_add : (float, float, float) binary
  | Float_sub : (float, float, float) binary
  | Float_mul : (float, float, float) binary
  | Float_div : (float, float, float) binary
  | Int_lt : (int, int, bool) binary
  | Float_lt : (float, float, bool) binary

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
  | Int_neg -> -x
  | Float_neg -> -.x
;;

let eval_binary : type a b c. (a, b, c) binary -> a -> b -> c =
  fun op x y ->
  match op with
  | Int_add -> x + y
  | Int_sub -> x - y
  | Int_mul -> x * y
  | Int_div -> x / y
  | Float_add -> x +. y
  | Float_sub -> x -. y
  | Float_mul -> x *. y
  | Float_div -> x /. y
  | Int_lt -> x < y
  | Float_lt -> x < y
;;
