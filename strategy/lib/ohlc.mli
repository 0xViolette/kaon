type bar = {
  timestamp : int64;
  open_ : float;
  high : float;
  low : float;
  close : float;
}

type series = {
  timestamp : int64 list;
  open_ : float list;
  high : float list;
  low : float list;
  close : float list;
}

val message_size : int
val decode : bytes -> bar
val encode : bytes -> bar -> unit
val show : bar -> unit
val add : bar -> series -> series
val really_read : In_channel.t -> bytes -> [> `Eof | `Ok | `Truncated of int ]
val next_bar : In_channel.t -> bytes -> bar option
