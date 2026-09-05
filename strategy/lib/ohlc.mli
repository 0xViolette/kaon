type t = {
  timestamp : int64;
  open_ : float;
  high : float;
  low : float;
  close : float;
}

val message_size : int
val decode : bytes -> t
val encode : bytes -> t -> unit
val show : t -> unit
