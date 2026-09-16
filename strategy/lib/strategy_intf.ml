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

  val ( +~ ) : float Language.signal -> float Language.signal -> float Language.signal
  val ( +~. ) : float Language.signal -> float -> float Language.signal
  val ( +.~ ) : float -> float Language.signal -> float Language.signal
  val ( *~ ) : float Language.signal -> float Language.signal -> float Language.signal
  val ( *~. ) : float Language.signal -> float -> float Language.signal
  val ( *.~ ) : float -> float Language.signal -> float Language.signal
  val ( -~ ) : float Language.signal -> float Language.signal -> float Language.signal
  val ( -~. ) : float Language.signal -> float -> float Language.signal
  val ( -.~ ) : float -> float Language.signal -> float Language.signal
  val ( /~ ) : float Language.signal -> float Language.signal -> float Language.signal
  val ( /~. ) : float Language.signal -> float -> float Language.signal
  val ( /.~ ) : float -> float Language.signal -> float Language.signal
  val ( <~ ) : float Language.signal -> float Language.signal -> bool Language.signal
  val ( <=~ ) : float Language.signal -> float Language.signal -> bool Language.signal
  val ( >~ ) : float Language.signal -> float Language.signal -> bool Language.signal
  val ( >=~ ) : float Language.signal -> float Language.signal -> bool Language.signal
  val ( &&~ ) : bool Language.signal -> bool Language.signal -> bool Language.signal
  val ( ||~ ) : bool Language.signal -> bool Language.signal -> bool Language.signal
  val ( =~ ) : 'a Language.signal -> 'a Language.signal -> bool Language.signal
  val ( !=~ ) : 'a Language.signal -> 'a Language.signal -> bool Language.signal
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
