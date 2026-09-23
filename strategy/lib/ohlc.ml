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

  let broker = Broker.create ()

  include S'

  type side =
    | Buy
    | Sell

  type order_kind =
    | Market
    | Limit of float signal
    | Stop of float signal
    | StopLimit of float signal * float signal

  let open_ : float signal = S'.input ()
  let high : float signal = S'.input ()
  let low : float signal = S'.input ()
  let close : float signal = S'.input ()
  let market_position : float signal = S'.input ()
  let bar_index = tick
  let netprofit = const 0.0

  let entry side qty order_kind cond : unit =
    match order_kind with
    | Market ->
      let _ =
        lift2
          (fun cond qty ->
             match cond, qty with
             | Some true, Some qty ->
               Broker.place_order broker side Market qty;
               None
             | _ -> None)
          cond
          qty
      in
      ()
    | Limit limit ->
      let _ =
        lift3
          (fun cond qty limit ->
             match cond, qty, limit with
             | Some true, Some qty, Some limit ->
               Broker.place_order broker side (Limit limit) qty;
               None
             | _ -> None)
          cond
          qty
          limit
      in
      ()
    | Stop stop ->
      let _ =
        lift3
          (fun cond qty stop ->
             match cond, qty, stop with
             | Some true, Some qty, Some stop ->
               Broker.place_order broker side (Stop stop) qty;
               None
             | _ -> None)
          cond
          qty
          stop
      in
      ()
    | StopLimit (stop, limit) ->
      let _ =
        lift4
          (fun cond qty stop limit ->
             match cond, qty, stop, limit with
             | Some true, Some qty, Some stop, Some limit ->
               Broker.place_order broker side (StopLimit { stop; limit }) qty;
               None
             | _ -> None)
          cond
          qty
          stop
          limit
      in
      ()
  ;;

  module Indicator = struct
    include S'.Indicator

    let tr =
      let prev_close = pre close in
      max (high -~ low) (max (abs (high -~ prev_close)) (abs (low -~ prev_close)))
    ;;

    let atr n =
      let alpha = 1. /. float_of_int n in
      recurrence (fun p ->
        cond (is_pending p) (sma n tr) ((tr *~. alpha) +~ (p *~. (1. -. alpha))))
    ;;
  end

  (* let backtest (f : unit -> unit) = *)
  (*   Utils.refuse_tty (); *)
  (*   let ic = In_channel.stdin in *)
  (*   let buf = Bytes.create message_size in *)
  (*   let rec loop n = *)
  (*     match next_bar ic buf with *)
  (*     | Some bar -> *)
  (*       step *)
  (*         [ Set (open_, bar.open_) *)
  (*         ; Set (high, bar.high) *)
  (*         ; Set (low, bar.low) *)
  (*         ; Set (close, bar.close) *)
  (*         ; Set (market_position, float_of_int (Broker.market_position broker)) *)
  (*         ]; *)
  (*       f (); *)
  (*       loop (n + 1) *)
  (*     | None -> prerr_endline "End of Stream" *)
  (*   in *)
  (*   loop 1 *)

  let backtest (f : unit -> unit) =
    let rec loop n =
      match Broker.step broker with
      | true ->
        step
          [ Set (open_, broker.open_)
          ; Set (high, broker.high)
          ; Set (low, broker.low)
          ; Set (close, broker.close)
          ; Set (market_position, float_of_int (Broker.market_position broker))
          ];
        f ();
        loop (n + 1)
      | false ->
        prerr_endline "End of stream";
        flush_all ()
    in
    loop 1
  ;;
end
