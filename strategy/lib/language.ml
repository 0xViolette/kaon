type (_, _) unary =
  | Int_neg : (int, int) unary
  | Float_neg : (float, float) unary
  | Int_abs : (int, int) unary
  | Float_abs : (float, float) unary
  | Float_of_int : (int, float) unary
  | Float_sqrt : (float, float) unary

type (_, _, _) binary =
  | Int_add : (int, int, int) binary
  | Float_add : (float, float, float) binary
  | Int_sub : (int, int, int) binary
  | Float_sub : (float, float, float) binary
  | Int_mul : (int, int, int) binary
  | Float_mul : (float, float, float) binary
  | Int_div : (int, int, int) binary
  | Float_div : (float, float, float) binary
  | Int_lt : (int, int, bool) binary
  | Float_lt : (float, float, bool) binary
  | Int_le : (int, int, bool) binary
  | Float_le : (float, float, bool) binary
  | Int_gt : (int, int, bool) binary
  | Float_gt : (float, float, bool) binary
  | Int_ge : (int, int, bool) binary
  | Float_ge : (float, float, bool) binary
  | Int_min : (int, int, int) binary
  | Float_min : (float, float, float) binary
  | Int_max : (int, int, int) binary
  | Float_max : (float, float, float) binary

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
  | Int_neg -> Int.neg x
  | Float_neg -> Float.neg x
  | Int_abs -> Int.abs x
  | Float_abs -> Float.abs x
  | Float_of_int -> Float.of_int x
  | Float_sqrt -> Float.sqrt x
;;

let eval_binary : type a b c. (a, b, c) binary -> a -> b -> c =
  fun op x y ->
  match op with
  | Int_add -> Int.add x y
  | Float_add -> Float.add x y
  | Int_sub -> Int.sub x y
  | Float_sub -> Float.sub x y
  | Int_mul -> Int.mul x y
  | Float_mul -> Float.mul x y
  | Int_div -> Int.div x y
  | Float_div -> Float.div x y
  | Int_lt -> x < y
  | Float_lt -> x < y
  | Int_le -> x <= y
  | Float_le -> x <= y
  | Int_gt -> x > y
  | Float_gt -> x > y
  | Int_ge -> x >= y
  | Float_ge -> x >= y
  | Int_min -> Int.min x y
  | Float_min -> Float.min x y
  | Int_max -> Int.max x y
  | Float_max -> Float.max x y
;;
