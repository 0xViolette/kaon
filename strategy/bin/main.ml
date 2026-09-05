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

module Protocol = struct
  let message_size = 40
  let timestamp_offset = 0
  let open_offset = 8
  let high_offset = 16
  let low_offset = 24
  let close_offset = 32
end

type ohlc = { t : int64; o : float; h : float; l : float; c : float }

let ohlc_from_bytes bytes =
  match
    List.map
      (fun off -> Bytes.get_int64_le bytes off |> Int64.float_of_bits)
      [
        Protocol.open_offset;
        Protocol.high_offset;
        Protocol.low_offset;
        Protocol.close_offset;
      ]
  with
  | [ o; h; l; c ] ->
      { t = Bytes.get_int64_le bytes Protocol.timestamp_offset; o; h; l; c }
  | _ -> failwith "Unexpected layout"

let ic = stdin;;

if In_channel.isatty ic then
  Printf.eprintf
    "error: Refusing to read binary from terminal\n\
     hint: Redirect from a file (< ohlc.bin) or pipe from a producer (... |).\n"
;;

let ohlc_bytes = Bytes.create Protocol.message_size in
match In_channel.really_input ic ohlc_bytes 0 Protocol.message_size with
| Some () ->
    ohlc_from_bytes ohlc_bytes |> fun x ->
    Printf.printf "timestamp: %Li | open: %f | high: %f | low: %f | close: %f"
      x.t x.o x.h x.l x.c
| None -> prerr_endline "error: unexpected EOF while reading OHLC data"
