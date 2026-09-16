module type S = sig
  val const : 'a -> 'a Language.signal
  val input : unit -> 'a Language.signal
  val pre : 'a Language.signal -> 'a Language.signal
  val delay : int -> 'a Language.signal -> 'a Language.signal
  val window : int -> 'a Language.signal -> 'a Language.signal list

  val loop
    :  init:'a Language.init
    -> ('a Language.signal -> 'a Language.signal)
    -> 'a Language.signal

  val int_neg : int Language.signal -> int Language.signal
  val float_neg : float Language.signal -> float Language.signal
  val int_abs : int Language.signal -> int Language.signal
  val float_abs : float Language.signal -> float Language.signal
  val float_sqrt : float Language.signal -> float Language.signal
  val float_of_int : int Language.signal -> float Language.signal
  val int_add : int Language.signal -> int Language.signal -> int Language.signal
  val float_add : float Language.signal -> float Language.signal -> float Language.signal
  val int_sub : int Language.signal -> int Language.signal -> int Language.signal
  val float_sub : float Language.signal -> float Language.signal -> float Language.signal
  val int_mul : int Language.signal -> int Language.signal -> int Language.signal
  val float_mul : float Language.signal -> float Language.signal -> float Language.signal
  val int_div : int Language.signal -> int Language.signal -> int Language.signal
  val float_div : float Language.signal -> float Language.signal -> float Language.signal
  val int_lt : int Language.signal -> int Language.signal -> bool Language.signal
  val float_lt : float Language.signal -> float Language.signal -> bool Language.signal
  val int_le : int Language.signal -> int Language.signal -> bool Language.signal
  val float_le : float Language.signal -> float Language.signal -> bool Language.signal
  val int_gt : int Language.signal -> int Language.signal -> bool Language.signal
  val float_gt : float Language.signal -> float Language.signal -> bool Language.signal
  val int_ge : int Language.signal -> int Language.signal -> bool Language.signal
  val float_ge : float Language.signal -> float Language.signal -> bool Language.signal
  val int_min : int Language.signal -> int Language.signal -> int Language.signal
  val float_min : float Language.signal -> float Language.signal -> float Language.signal
  val int_max : int Language.signal -> int Language.signal -> int Language.signal
  val float_max : float Language.signal -> float Language.signal -> float Language.signal
  val and_ : bool Language.signal -> bool Language.signal -> bool Language.signal
  val or_ : bool Language.signal -> bool Language.signal -> bool Language.signal
  val ( +~ ) : int Language.signal -> int Language.signal -> int Language.signal
  val ( +.~ ) : float Language.signal -> float Language.signal -> float Language.signal
  val ( *~ ) : int Language.signal -> int Language.signal -> int Language.signal
  val ( *.~ ) : float Language.signal -> float Language.signal -> float Language.signal
  val ( -~ ) : int Language.signal -> int Language.signal -> int Language.signal
  val ( -.~ ) : float Language.signal -> float Language.signal -> float Language.signal
  val ( /~ ) : int Language.signal -> int Language.signal -> int Language.signal
  val ( /.~ ) : float Language.signal -> float Language.signal -> float Language.signal
  val ( <~ ) : int Language.signal -> int Language.signal -> bool Language.signal
  val ( <.~ ) : float Language.signal -> float Language.signal -> bool Language.signal
  val ( <=~ ) : int Language.signal -> int Language.signal -> bool Language.signal
  val ( <=.~ ) : float Language.signal -> float Language.signal -> bool Language.signal
  val ( >~ ) : int Language.signal -> int Language.signal -> bool Language.signal
  val ( >.~ ) : float Language.signal -> float Language.signal -> bool Language.signal
  val ( >=~ ) : int Language.signal -> int Language.signal -> bool Language.signal
  val ( >=.~ ) : float Language.signal -> float Language.signal -> bool Language.signal
  val ( &&~ ) : bool Language.signal -> bool Language.signal -> bool Language.signal
  val ( ||~ ) : bool Language.signal -> bool Language.signal -> bool Language.signal
  val compile : unit -> Runtime.t

  module Indicator : sig
    val sma : int -> float Language.signal -> float Language.signal
    val ema : int -> float Language.signal -> float Language.signal
    val crossover : float Language.signal -> float Language.signal -> bool Language.signal

    val crossunder
      :  float Language.signal
      -> float Language.signal
      -> bool Language.signal
  end
end
