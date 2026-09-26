module Make () = struct
  module S' = Strategy.Make ()

  let broker = Broker.create ~initial_balance:3000000. ()

  include S'

  type side =
    | Buy
    | Sell

  type order_kind =
    | Market
    | Limit of float signal
    | Stop of float signal

  let open_ : float signal = S'.input ()
  let high : float signal = S'.input ()
  let low : float signal = S'.input ()
  let close : float signal = S'.input ()
  let market_position : float signal = S'.input ()
  let net_pnl : float signal = S'.input ()
  let initial_balance : float signal = S'.input ()
  let net_qty : float signal = S'.input ()
  let lot_size = broker.lot_size
  let bar_index = tick

  let entry id side qty order_kind cond : unit =
    match order_kind with
    | Market ->
      let _ =
        lift2
          (fun cond qty ->
             match cond, qty with
             | Some true, Some qty ->
               Broker.place_order broker id side Market qty;
               None
             | _ -> None)
          cond
          qty
      in
      ()
    | Limit limit ->
      let _ =
        lift3
          (fun cond qty limit ->
             match cond, qty, limit with
             | Some true, Some qty, Some limit ->
               Broker.place_order broker id side (Limit limit) qty;
               None
             | _ -> None)
          cond
          qty
          limit
      in
      ()
    | Stop stop ->
      let _ =
        lift3
          (fun cond qty stop ->
             match cond, qty, stop with
             | Some true, Some qty, Some stop ->
               Broker.place_order broker id side (Stop stop) qty;
               None
             | _ -> None)
          cond
          qty
          stop
      in
      ()
  ;;

  module Indicator = struct
    include S'.Indicator

    let tr =
      let prev_close = pre close in
      max (high -~ low) (max (abs (high -~ prev_close)) (abs (low -~ prev_close)))
    ;;

    let atr n =
      let alpha = 1. /. float_of_int n in
      recurrence (fun p ->
        cond (is_pending p) (sma n tr) ((tr *~. alpha) +~ (p *~. (1. -. alpha))))
    ;;
  end

  let backtest (f : unit -> unit) =
    let rec loop n =
      match Broker.step broker with
      | true ->
        step
          [ Set (open_, broker.open_)
          ; Set (high, broker.high)
          ; Set (low, broker.low)
          ; Set (close, broker.close)
          ; Set (market_position, float_of_int broker.market_position)
          ; Set (initial_balance, broker.initial_balance)
          ; Set (net_pnl, broker.net_pnl)
          ; Set (net_qty, broker.net_qty)
          ];
        f ();
        loop (n + 1)
      | false ->
        prerr_endline "End of stream";
        flush_all ()
    in
    loop 1
  ;;
end
