module Make () = struct
  module S' = Strategy.Make ()

  let broker = Broker.create ~initial_balance:3000000. ()

  (* Orders are declared as intents, not fixed quantities: an exit covers
     whatever position is live when it fills, and an entry first covers the
     opposite position and then establishes its target.  After each fill,
     every pending order is re-aligned with the live position (the same
     reconciliation TradeStation does internally), so sibling orders can
     never double-fill the same contracts. *)
  let exits : (string * bool) list ref = ref []
  let entries : (string * bool * float ref) list ref = ref []

  let () =
    broker.on_fill
    <- (fun _ ->
         let net = broker.net_qty in
         List.iter
           (fun (id, exits_long) ->
              if (exits_long && net <= 0.) || ((not exits_long) && net >= 0.)
              then Broker.cancel_order broker id
              else Broker.modify_order broker id ~qty:(Float.abs net) ())
           !exits;
         List.iter
           (fun (id, is_buy, target) ->
              let needed = if is_buy then !target -. net else !target +. net in
              if needed <= 0.
              then Broker.cancel_order broker id
              else Broker.modify_order broker id ~qty:needed ())
           !entries)
  ;;

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

  module Position = struct
    let entry_price = fmap (fun _ -> Broker.entry_price broker) (undefined ())
    let avg_entry_price = fmap (fun _ -> Broker.average_entry_price broker) (undefined ())
  end

  let place_order ?(on_place = ignore) id side qty order_kind cond : unit =
    match order_kind with
    | Market ->
      let _ =
        lift2
          (fun cond qty ->
             match cond, qty with
             | Some true, Some qty ->
               Broker.place_order broker id side Market qty;
               on_place qty;
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
               on_place qty;
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
               on_place qty;
               None
             | _ -> None)
          cond
          qty
          stop
      in
      ()
  ;;

  (* order-management helpers over the dumb venue: an entry carries its
     target position (plus whatever opposite position it must cover), an
     exit covers the whole live position; quantities re-align on every fill *)
  let enter_long id target kind cond =
    let placed = ref 0. in
    entries := (id, true, placed) :: !entries;
    place_order
      ~on_place:(fun qty -> placed := qty -. Float.max 0. (-.broker.net_qty))
      id
      Buy
      (target +~ Indicator.max (const 0.) (const 0. -~ net_qty))
      kind
      (cond &&~ (market_position <~. 1.0))
  ;;

  let enter_short id target kind cond =
    let placed = ref 0. in
    entries := (id, false, placed) :: !entries;
    place_order
      ~on_place:(fun qty -> placed := qty -. Float.max 0. broker.net_qty)
      id
      Sell
      (target +~ Indicator.max (const 0.) net_qty)
      kind
      (cond &&~ (market_position >~. -1.0))
  ;;

  let exit_long id kind cond =
    exits := (id, true) :: !exits;
    place_order id Sell (abs net_qty) kind (cond &&~ (market_position >~. 0.))
  ;;

  let exit_short id kind cond =
    exits := (id, false) :: !exits;
    place_order id Buy (abs net_qty) kind (cond &&~ (market_position <~. 0.))
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
