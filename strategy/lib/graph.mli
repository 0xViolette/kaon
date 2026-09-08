type packed_node = Pack : 'a node -> packed_node

and 'a node =
  | Source of { id : int; mutable value : 'a }
  | Derived of {
      id : int;
      mutable version : int;
      mutable value : 'a;
      deps : packed_node array;
      compute : unit -> 'a;
    }

val read : 'a node -> 'a
val map : ('a -> 'b) -> 'a node -> 'b node
val map2 : ('a -> 'b -> 'c) -> 'a node -> 'b node -> 'c node
val create_source : 'a -> 'a node
val set : 'a node -> 'a -> unit
