let die fmt =
  Printf.ksprintf
    (fun msg ->
      prerr_endline ("error: " ^ msg);
      exit 1)
    fmt

let refuse_tty () =
  if In_channel.isatty In_channel.stdin then
    die
      "refusing to read binary from terminal\n\
       hint: redirect from a file (< ohlc.bin) or pipe from a producer\n"

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

let next ic buf =
  match really_read ic buf with
  | `Ok -> Some (Ohlc.decode buf)
  | `Eof -> None
  | `Truncated n ->
      die "truncated record: got %d bytes, expected %d" n Ohlc.message_size

let () =
  refuse_tty ();
  let ic = In_channel.stdin in
  let buf = Bytes.create Ohlc.message_size in
  let rec loop n =
    match next ic buf with
    | Some x ->
        Ohlc.show x;
        loop (n + 1)
    | None -> print_endline "Done"
  in
  loop 1
