open Lib
module Papa = Ohlc.Make ()

let strat () =
  let open Papa in
  let bigpointvalue = 50.0 in
  let echannel = 10 in
  let risk = 0.01 in
  let equity = 3000000. in
  let leverage = 2.0 in
  let atrgunak = 2. in
  let ma = 65 in
  let atrbars = 20 in
  let le = Indicator.highest echannel high +~. 0.05 in
  let se = Indicator.lowest echannel low -~. 0.05 in
  let atr1 = Indicator.sma atrbars Indicator.tr in
  let atr = Indicator.ema 20 atr1 in
  let mov = Indicator.ema ma close in
  let movbuy = mov *~. 1.001 in
  let movsell = mov *~. 0.999 in
  let pbl = se *~. 1.035 in
  let pbs = le *~. 0.965 in
  let netequity = (equity +.~ netprofit) *~. leverage in
  let netequity1 = (equity *. leverage) +.~ netprofit in
  let qty1 = netequity *~. risk /~ (atrgunak *.~ atr *~. bigpointvalue) in
  let con1 = floor qty1 in
  let qty2 = netequity /~ (le *~. bigpointvalue) in
  let con2 = floor qty2 in
  let qty3 = netequity /~ (se *~. bigpointvalue) in
  let con3 = floor qty3 in
  let con4 = const 2.0 in
  [ "close", close
  ; "le", le
  ; "se", se
  ; "atr1", atr1
  ; "atr", atr
  ; "mov", mov
  ; "movbuy", movbuy
  ; "movsell", movsell
  ; "pbl", pbl
  ; "pbs", pbs
  ; "netequity", netequity
  ; "netequity1", netequity1
  ; "qty1", qty1
  ; "con1", con1
  ; "qty2", qty2
  ; "con2", con2
  ; "qty3", qty3
  ; "con3", con3
  ; "con4", con4
  ]
;;

let signals = strat ()

let () =
  let start_time = Sys.time () in
  Papa.compile ();
  Papa.backtest (fun () ->
    (* Force evaluations if Papa.value is lazy, without printing *)
    List.iter (fun (_, s) -> ignore (Papa.value s)) signals);
  let end_time = Sys.time () in
  Printf.printf "Execution time: %.4f seconds\n" (end_time -. start_time)
;;
