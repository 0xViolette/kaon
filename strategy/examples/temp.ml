open Lib

let string_float a = a |> Option.map string_of_float |> Option.value ~default:"None"

module Strat1 = Strategy.Make ()

let f1 close =
  let open Strat1 in
  let sma5 = List.fold_left ( +.~ ) (const 0.) (window 5 close) /.~ const 5.0 in
  let ema =
    let multiplier = const (1. /. 5.) in
    loop
      ~init:(Node (sma5, 5))
      (fun p -> (multiplier *.~ close) +.~ (p *.~ (const 1.0 -.~ multiplier)))
  in
  sma5, ema
;;

let close = Strat1.input ()
let sma5, ema = f1 close
let r = Strat1.compile ()

let () =
  Printf.printf "size: %d\n" r.size;
  for i = 1 to 20 do
    Runtime.(tick r [ Set (close, float_of_int i) ]);
    Printf.printf
      "\nclose=%s\nsma5=%s\nema=%s\n"
      (string_float (Runtime.value r close))
      (string_float (Runtime.value r sma5))
      (string_float (Runtime.value r ema))
  done
;;
