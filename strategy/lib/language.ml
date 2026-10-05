(*
A signal can only be float or bool [numerical stream or logical stream]
*)

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

and _ def =
  | Undefined : 'a def
  | Const : 'a -> 'a def
  | Input : 'a def
  | Tick : int def
  | Pre : 'a signal -> 'a def
  | Knot : 'a def
  | Rec : 'a signal * 'a signal -> 'a def
  | Fmap : ('a option -> 'b option) * 'a signal -> 'b def
  | Lift2 :
      ('a option -> 'b option -> 'c option) * 'a signal * 'b signal
      -> 'c def

type any_signal = Any : 'a signal -> any_signal [@@unboxed]
