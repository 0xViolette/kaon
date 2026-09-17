let die fmt =
  Printf.ksprintf
    (fun msg ->
       prerr_endline ("error: " ^ msg);
       exit 1)
    fmt
;;

let refuse_tty () =
  if In_channel.isatty In_channel.stdin
  then
    die
      "refusing to read binary from terminal\n\
       hint: redirect from a file (< ohlc.bin) or pipe from a producer\n"
;;
