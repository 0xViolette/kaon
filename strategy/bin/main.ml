open Lib

let pp_float = function
  | None -> "None"
  | Some x -> Printf.sprintf "%.2f" x
;;

module OhlcStrat = Ohlc.Make ()

let strat () =
  let open OhlcStrat in
  let sma5 = Indicator.sma 5 close in
  sma5
;;

let observable = strat ()
let r = OhlcStrat.compile ();;

OhlcStrat.backtest r (fun () ->
  Printf.printf
    "close: %s | sma5: %s\n"
    (pp_float (Runtime.value r OhlcStrat.close))
    (pp_float (Runtime.value r observable)))
