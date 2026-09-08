let die fmt =
  Printf.ksprintf
    (fun msg ->
      prerr_endline ("error: " ^ msg);
      exit 1)
    fmt
