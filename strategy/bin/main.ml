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
  let entered_long = market_position >~. 0. &&~ (pre market_position <=~. 0.) in
  let entered_short = market_position <~. 0. &&~ (pre market_position >=~. 0.) in
  let atr1 = Indicator.sma atrbars Indicator.tr in
  let atr = Indicator.ema 20 atr1 in
  let mov = Indicator.ema ma close in
  let netequity = (initial_balance +~ net_pnl) *~. leverage in
  let qty1 = netequity *~. risk /~ (atrgunak *.~ atr *~. lot_size) in
  let con1 = floor qty1 in
  let qty2 = netequity /~ (le *~. lot_size) in
  let con2 = floor qty2 in
  let qty3 = netequity /~ (se *~. lot_size) in
  let con3 = floor qty3 in
  let con4 = const 2.0 in
  let finalcon = Indicator.min con1 (Indicator.min con2 con3) in
  let tradecon = Indicator.max finalcon con4 in
  let atr_at_entry =
    Indicator.min (const 50.0) (value_when (entered_long ||~ entered_short) atr)
  in
  let lx2n = Position.entry_price -~ (atrgunak *.~ atr_at_entry) in
  let sx2n = Position.entry_price +~ (atrgunak *.~ atr_at_entry) in
  place_order
    "LE"
    Buy
    (tradecon +~ abs net_qty)
    (Stop le)
    (close >~ mov &&~ (market_position <~. 1.0));
  place_order
    "LE stoploss"
    Sell
    (abs net_qty)
    (Stop lx2n)
    (Position.entry_price >~ se &&~ (market_position >~. 0.));
  place_order
    "SE"
    Sell
    (tradecon +~ abs net_qty)
    (Stop se)
    (close <~ mov &&~ (market_position >~. -1.0));
  place_order
    "SE stoploss"
    Buy
    (abs net_qty)
    (Stop sx2n)
    (Position.entry_price <~ le &&~ (market_position <~. 0.))
;;

let signals = strat ()

let () =
  let start_time = Sys.time () in
  Papa.compile ();
  Papa.backtest (fun () -> ());
  let end_time = Sys.time () in
  Printf.printf "Execution time: %.4f seconds\n" (end_time -. start_time)
;;
