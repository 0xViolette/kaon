module Make () = struct
  module S' = Strategy.Make ()

  let broker = Broker.create ~lot_size:50. ~initial_balance:3_000_000.
  let ledger = broker.Broker.ledger

  include S'

  type order_kind =
    | Market
    | Limit of float signal
    | Stop of float signal

  let open_ : float signal = S'.input ()
  let high : float signal = S'.input ()
  let low : float signal = S'.input ()
  let close : float signal = S'.input ()
  let lot_size = ledger.lot_size
  (* let bar_index = tick *)

  module Account = struct
    let net_pnl : float signal = S'.input ()
    let initial_balance : float signal = S'.input ()
  end

  module Position = struct
    let net_qty : float signal = S'.input ()
    let side : float signal = S'.input ()

    let entry_price =
      fmap (fun _ -> Ledger.entry_price ledger) (undefined ())
    ;;

    let avg_entry_price =
      fmap (fun _ -> Ledger.avg_entry_price ledger) (undefined ())
    ;;
  end

  module Indicator = struct
    include Indicator
    open Ops

    let tr =
      let prev_close = pre close in
      max
        (high - low)
        (max (abs (high - prev_close)) (abs (low - prev_close)))
    ;;

    let atr n =
      let alpha = 1. /. float_of_int n in
      recurrence (fun p ->
        cond
          (is_pending p)
          (sma n tr)
          ((tr * !alpha) + (p * !(1. -. alpha))))
    ;;
  end

  let kind_signal = function
    | Market -> Ops.(!Order.Market)
    | Limit p -> fmap (Option.map (fun p -> Order.Limit p)) p
    | Stop p -> fmap (Option.map (fun p -> Order.Stop p)) p
  ;;

  let order tag intent target kind when_ : Oms.desired signal =
    lift3
      (fun cond target kind ->
         match cond, target, kind with
         | Some true, Some target, Some kind ->
           Some { Oms.tag; intent = intent target; kind }
         | _ -> None)
      when_
      target
      (kind_signal kind)
  ;;

  let enter_long tag target kind when_ =
    order tag (fun t -> Oms.Go_long t) target kind when_
  ;;

  let enter_short tag target kind when_ =
    order tag (fun t -> Oms.Go_short t) target kind when_
  ;;

  let exit_long tag kind when_ =
    order tag (fun _ -> Oms.Close_long) (const 0.) kind when_
  ;;

  let exit_short tag kind when_ =
    order tag (fun _ -> Oms.Close_short) (const 0.) kind when_
  ;;

  type bar_report =
    { bar : Codec.Ohlc.bar
    ; fills : Order.fill list
    ; placed : Oms.desired list
    }

  let backtest
        ?(probes : (string * float signal) list = [])
        ?(on_bar = fun (_ : Report.t) -> ())
        (strategy : Oms.desired signal list)
    =
    compile ();
    let rec loop (resting : Oms.desired list) bars =
      match bars () with
      | Seq.Nil -> prerr_endline "End of stream"
      | Seq.Cons (bar, rest) ->
        Broker.start_bar broker bar;
        let fills = Oms.settle broker resting in
        Broker.end_bar broker;
        step
          [ Set (open_, bar.open_)
          ; Set (high, bar.high)
          ; Set (low, bar.low)
          ; Set (close, bar.close)
          ; Set (Position.side, Ledger.side ledger)
          ; Set (Position.net_qty, Ledger.net_lots ledger)
          ; Set (Account.initial_balance, ledger.initial_balance)
          ; Set (Account.net_pnl, ledger.realized)
          ];
        let submitted =
          Oms.submit broker (List.filter_map value strategy)
        in
        let is_new (d : Oms.desired) =
          Bool.not
            (List.exists
               (fun (r : Oms.desired) -> String.equal r.tag d.tag)
               resting)
        in
        on_bar
          { Report.bar
          ; fills
          ; placed = List.filter is_new submitted
          ; probes = List.map (fun (name, s) -> name, value s) probes
          };
        loop submitted rest
    in
    loop [] (Feed.stdin_bars ())
  ;;
end
