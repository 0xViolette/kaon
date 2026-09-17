(* OHLC protocol
- Encoding: binary
- Byte order: little-endian
- Message size: 40 bytes
- Floating-point representation: IEEE 754 binary64 (`float64`)


+--------+------+---------+-----------+
| Offset | Size | Type    | Field     |
+--------+------+---------+-----------+
| 0      | 8    | uint64  | timestamp |
| 8      | 8    | float64 | open      |
| 16     | 8    | float64 | high      |
| 24     | 8    | float64 | low       |
| 32     | 8    | float64 | close     |
+--------+------+---------+-----------+
*)

type bar_tick =
  { timestamp : int64
  ; open_ : float
  ; high : float
  ; low : float
  ; close : float
  }

let message_size = 40

let decode bytes : bar_tick =
  let bytes_to_float offset = Int64.float_of_bits (Bytes.get_int64_le bytes offset) in
  { timestamp = Bytes.get_int64_le bytes 0
  ; open_ = bytes_to_float 8
  ; high = bytes_to_float 16
  ; low = bytes_to_float 24
  ; close = bytes_to_float 32
  }
;;

let encode buf (bar : bar_tick) =
  let set_float64 off v = Bytes.set_int64_le buf off (Int64.bits_of_float v) in
  Bytes.set_int64_le buf 0 bar.timestamp;
  set_float64 8 bar.open_;
  set_float64 16 bar.high;
  set_float64 24 bar.low;
  set_float64 32 bar.close
;;

let show (bar : bar_tick) =
  Printf.printf
    "timestamp: %Ld | open: %f | high: %f | low: %f | close: %f\n"
    bar.timestamp
    bar.open_
    bar.high
    bar.low
    bar.close
;;

let really_read ic buf =
  let len = Bytes.length buf in
  let rec fill off =
    if off = len
    then `Ok
    else (
      match In_channel.input ic buf off (len - off) with
      | 0 -> if off = 0 then `Eof else `Truncated off
      | n -> fill (off + n))
  in
  fill 0
;;

let next_bar ic buf =
  match really_read ic buf with
  | `Ok -> Some (decode buf)
  | `Eof -> None
  | `Truncated n -> Utils.die "truncated record: got %d bytes, expected %d" n message_size
;;

(* module type S = sig *)
(*   module S' : Strategy.S *)
(**)
(*   val open_ : float Language.signal *)
(*   val high : float Language.signal *)
(*   val low : float Language.signal *)
(*   val close : float Language.signal *)
(* end *)

module Make () = struct
  module S' = Strategy.Make ()
  include S'

  let open_ : float Language.signal = S'.input ()
  let high : float Language.signal = S'.input ()
  let low : float Language.signal = S'.input ()
  let close : float Language.signal = S'.input ()

  module Indicator = struct
    include S'.Indicator

    let tr =
      let open S' in
      let prev_close = pre close in
      max (high -~ low) (max (abs (high -~ prev_close)) (abs (low -~ prev_close)))
    ;;

    let atr n alpha = ema n alpha tr
  end

  let backtest r (f : unit -> unit) =
    Utils.refuse_tty ();
    let ic = In_channel.stdin in
    let buf = Bytes.create message_size in
    let rec loop n =
      match next_bar ic buf with
      | Some bar ->
        Runtime.(
          tick
            r
            [ Set (open_, bar.open_)
            ; Set (high, bar.high)
            ; Set (low, bar.low)
            ; Set (close, bar.close)
            ]);
        f ();
        loop (n + 1)
      | None -> prerr_endline "End of Stream"
    in
    loop 1
  ;;
end
