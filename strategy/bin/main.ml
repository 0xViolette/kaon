open Lib
module Papa = Ohlc.Make ()

let strat () =
  let open Papa in
  let echannel = 10 in
  let risk = 0.01 in
  let leverage = 2.0 in
  let atrgunak = 2. in
  let ma = 65 in
  let atrbars = 20 in
  let le = Indicator.highest echannel high +~. 0.05 in
  let se = Indicator.lowest echannel low -~. 0.05 in
  let atr1 = Indicator.sma atrbars Indicator.tr in
  let atr = Indicator.ema 20 atr1 in
  let mov = Indicator.ema ma close in
  (* let movbuy = mov *~. 1.001 in *)
  (* let movsell = mov *~. 0.999 in *)
  (* let pbl = se *~. 1.035 in *)
  (* let pbs = le *~. 0.965 in *)
  let netequity = (initial_balance +~ net_pnl) *~. leverage in
  (* let netequity1 = (initial_balance *~. leverage) +~ net_pnl in *)
  let qty1 = netequity *~. risk /~ (atrgunak *.~ atr *~. lot_size) in
  let con1 = floor qty1 in
  let qty2 = netequity /~ (le *~. lot_size) in
  let con2 = floor qty2 in
  let qty3 = netequity /~ (se *~. lot_size) in
  let con3 = floor qty3 in
  let con4 = const 2.0 in
  let finalcon = Indicator.min con1 (Indicator.min con2 con3) in
  let tradecon = Indicator.max finalcon con4 in
  entry
    "LE"
    Buy
    (tradecon +~ abs net_qty)
    (Stop le)
    (close >~ mov &&~ (market_position <~. 1.0));
  entry
    "SE"
    Sell
    (tradecon +~ abs net_qty)
    (Stop se)
    (close <~ mov &&~ (market_position >~. -1.0))
;;

let signals = strat ()

let () =
  let start_time = Sys.time () in
  Papa.compile ();
  Papa.backtest (fun () -> ());
  let end_time = Sys.time () in
  Printf.printf "Execution time: %.4f seconds\n" (end_time -. start_time)
;;
