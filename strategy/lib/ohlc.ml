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

type t = {
  timestamp : int64;
  open_ : float;
  high : float;
  low : float;
  close : float;
}

let message_size = 40

let decode bytes =
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

let encode buf r =
  let set_float64 off v = Bytes.set_int64_le buf off (Int64.bits_of_float v) in
  Bytes.set_int64_le buf 0 r.timestamp;
  set_float64 8 r.open_;
  set_float64 16 r.high;
  set_float64 24 r.low;
  set_float64 32 r.close

let show x =
  Printf.printf "timestamp: %Ld | open: %f | high: %f | low: %f | close: %f\n"
    x.timestamp x.open_ x.high x.low x.close
