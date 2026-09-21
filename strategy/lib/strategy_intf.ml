module type S = sig
  type 'a signal
  type set = Set : 'a signal * 'a -> set

  val tick : int signal
  val compile : unit -> unit
  val step : set list -> unit
  val const : 'a -> 'a signal
  val undefined : unit -> 'a signal
  val value : 'a signal -> 'a option
  val input : unit -> 'a signal
  val pre : 'a signal -> 'a signal
  val delay : int -> 'a signal -> 'a signal
  val window : int -> 'a signal -> 'a signal list
  val recurrence : ('a signal -> 'a signal) -> 'a signal
  val ( +~ ) : float signal -> float signal -> float signal
  val ( +~. ) : float signal -> float -> float signal
  val ( +.~ ) : float -> float signal -> float signal
  val ( *~ ) : float signal -> float signal -> float signal
  val ( *~. ) : float signal -> float -> float signal
  val ( *.~ ) : float -> float signal -> float signal
  val ( -~ ) : float signal -> float signal -> float signal
  val ( -~. ) : float signal -> float -> float signal
  val ( -.~ ) : float -> float signal -> float signal
  val ( /~ ) : float signal -> float signal -> float signal
  val ( /~. ) : float signal -> float -> float signal
  val ( /.~ ) : float -> float signal -> float signal
  val ( <~ ) : float signal -> float signal -> bool signal
  val ( <=~ ) : float signal -> float signal -> bool signal
  val ( >~ ) : float signal -> float signal -> bool signal
  val ( >=~ ) : float signal -> float signal -> bool signal
  val ( &&~ ) : bool signal -> bool signal -> bool signal
  val ( ||~ ) : bool signal -> bool signal -> bool signal
  val ( =~ ) : 'a signal -> 'a signal -> bool signal
  val ( !=~ ) : 'a signal -> 'a signal -> bool signal
  val cond : bool signal -> 'a signal -> 'a signal -> 'a signal
  val abs : float signal -> float signal
  val not : bool signal -> bool signal
  val sqrt : float signal -> float signal
  val floor : float signal -> float signal
  val ceil : float signal -> float signal
  val is_pending : float signal -> bool signal
  val map : ('a -> 'b) -> 'a signal -> 'b signal

  module Indicator : sig
    val max : float signal -> float signal -> float signal
    val min : float signal -> float signal -> float signal
    val sma : int -> float signal -> float signal
    val ema : int -> float signal -> float signal
    val crossover : float signal -> float signal -> bool signal
    val highest : int -> float signal -> float signal
    val lowest : int -> float signal -> float signal
    val bars_since : bool signal -> float signal
    val crossunder : float signal -> float signal -> bool signal
  end
end
