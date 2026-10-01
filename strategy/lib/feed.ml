module C = Codec.Ohlc

let stdin_bars () : C.bar Seq.t =
  Utils.refuse_tty ();
  let buf = Bytes.create C.message_size in
  let rec next () =
    match Utils.read_exactly In_channel.stdin buf with
    | `Ok -> Seq.Cons (C.decode buf, next)
    | `Eof -> Seq.Nil
    | `Truncated (recieved, expected) ->
      Utils.die
        "truncated record: received %d bytes, expected %d"
        recieved
        expected
  in
  next
;;
