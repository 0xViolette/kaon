(* open Lib *)
(**)
(* let pp_float = function *)
(*   | None -> "None" *)
(*   | Some x -> Printf.sprintf "%.2f" x *)
(* ;; *)
(**)
(* module OhlcStrat = Ohlc.Make () *)
(**)
(* let strat () = *)
(*   let open OhlcStrat in *)
(*   let sma5 = Indicator.sma 5 close in *)
(*   sma5 *)
(* ;; *)
(**)
(* let observable = strat ();; *)
(**)
(* OhlcStrat.compile (); *)
(* OhlcStrat.backtest (fun () -> *)
(*   Printf.printf *)
(*     "close: %s | sma5: %s\n" *)
(*     (pp_float (OhlcStrat.value OhlcStrat.close)) *)
(*     (pp_float (OhlcStrat.value observable))) *)

open Lib

let pp_float = function
  | None -> "None"
  | Some x -> Printf.sprintf "%.2f" x
;;

let pp_int = function
  | None -> "None"
  | Some x -> Printf.sprintf "%d" x
;;

module Papa = Ohlc.Make ()

let strat () =
  let open Papa in
  (* let echannel = 10 in *)
  (* let risk = 0.01 in *)
  (* let equity = 3000000. in *)
  (* let leverage = 2.0 in *)
  (* let atrgunak = 2. in *)
  (* let ma = 65 in *)
  let atrbars = 20 in
  (* let le = Indicator.highest echannel high +~. 0.05 in *)
  (* let se = Indicator.lowest echannel low -~. 0.05 in *)
  let atr1 = Indicator.sma atrbars Indicator.tr in
  let sma = Indicator.sma 20 atr1 in
  let atr = Indicator.ema 20 atr1 in
  (* let mov = Indicator.ema ma close in *)
  (* let movbuy = mov *~. 1.001 in *)
  (* let movsell = mov *~. 0.999 in *)
  (* let pbl = se *~. 1.035 in *)
  (* let pbs = le *~. 0.965 in *)
  (* let netequity = (equity +.~ netprofit) *~. leverage in *)
  (* let netequity1 = (equity *. leverage) +.~ netprofit in *)
  sma, atr1, atr, bar_index
;;

let sma, atr1, atr, bar_index = strat ();;

Papa.compile ();
Papa.backtest (fun () ->
  Printf.printf
    "bar_index: %s | sma20: %s | atr1: %s | atr: %s\n"
    (pp_int (Papa.value bar_index))
    (pp_float (Papa.value sma))
    (pp_float (Papa.value atr1))
    (pp_float (Papa.value atr)))
