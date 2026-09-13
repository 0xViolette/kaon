open Lib

let refuse_tty () =
  if In_channel.isatty In_channel.stdin
  then
    Utils.die
      "refusing to read binary from terminal\n\
       hint: redirect from a file (< ohlc.bin) or pipe from a producer\n"
;;

let () =
  refuse_tty ();
  let ic = In_channel.stdin in
  let buf = Bytes.create Ohlc.message_size in
  let rec loop n =
    match Ohlc.next_bar ic buf with
    | Some _bar -> loop (n + 1)
    | None -> prerr_endline "End of Stream"
  in
  loop 1
;;
