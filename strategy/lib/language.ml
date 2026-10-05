(*
A signal can only be float or bool [numerical stream or logical stream]
*)

type _ signal_kind =
  | Float : float signal_kind
  | Effect : 'a signal_kind

type 'a signal =
  { id : int
  ; graph_id : int
  ; rank : int
  ; kind : 'a signal_kind
  ; def : 'a def
  ; (* a knot is wired to the loop that owns it when the graph is compiled *)
    mutable knot_target : float signal option
  ; (* hash-cons slot: a node has at most one [Pre] child *)
    mutable pre_child : 'a signal option
  }

and _ def =
  | Undefined : float def
  | Const : float -> float def
  | Input : float def
  | Tick : float def
  | Pre : 'a signal -> 'a def
  | Cond : float signal * float signal * float signal -> float def
  | Knot : float def
  | Rec : float signal * float signal -> 'a def
  | Fmap : (float -> float) * float signal -> float def
  | Effect : ('a option -> 'b option) * 'a signal -> 'b def
  | Effect2 :
      ('a option -> 'b option -> 'c option) * 'a signal * 'b signal
      -> 'c def
  | Lift2 :
      (float -> float -> float) * float signal * float signal
      -> float def

type any_signal = Any : 'a signal -> any_signal [@@unboxed]
