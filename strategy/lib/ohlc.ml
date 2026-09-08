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

let message_size = 40

let decode bytes : bar =
  let bytes_to_float offset =
    Int64.float_of_bits (Bytes.get_int64_le bytes offset)
  in
  {
    timestamp = Bytes.get_int64_le bytes 0;
    open_ = bytes_to_float 8;
    high = bytes_to_float 16;
    low = bytes_to_float 24;
    close = bytes_to_float 32;
  }

let encode buf (bar : bar) =
  let set_float64 off v = Bytes.set_int64_le buf off (Int64.bits_of_float v) in
  Bytes.set_int64_le buf 0 bar.timestamp;
  set_float64 8 bar.open_;
  set_float64 16 bar.high;
  set_float64 24 bar.low;
  set_float64 32 bar.close

let show (bar : bar) =
  Printf.printf "timestamp: %Ld | open: %f | high: %f | low: %f | close: %f\n"
    bar.timestamp bar.open_ bar.high bar.low bar.close

let add (bar : bar) (bars : series) =
  {
    timestamp = bar.timestamp :: bars.timestamp;
    open_ = bar.open_ :: bars.open_;
    high = bar.high :: bars.high;
    low = bar.low :: bars.low;
    close = bar.close :: bars.close;
  }

let really_read ic buf =
  let len = Bytes.length buf in
  let rec fill off =
    if off = len then `Ok
    else
      match In_channel.input ic buf off (len - off) with
      | 0 -> if off = 0 then `Eof else `Truncated off
      | n -> fill (off + n)
  in
  fill 0

let next_bar ic buf =
  match really_read ic buf with
  | `Ok -> Some (decode buf)
  | `Eof -> None
  | `Truncated n ->
      Utils.die "truncated record: got %d bytes, expected %d" n message_size
