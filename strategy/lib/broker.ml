(* OHLC protocol
- Encoding: binary
- Byte order: little-endian
- Message size: 40 bytes
- Floating-point representation: IEEE 754 binary64 (`float64`)


+--------+------+---------+-----------+
| Offset | Size | Type    | Field     |
+--------+------+---------+-----------+
| 0      | 8    | uint64  | timestamp |
| 8      | 8    | float64 | open      |
| 16     | 8    | float64 | high      |
| 24     | 8    | float64 | low       |
| 32     | 8    | float64 | close     |
+--------+------+---------+-----------+
*)
open Order
module C = Codec.Ohlc

module Account = struct
  type t =
    { initial_balance : float
    ; mutable cash : float
    ; mutable realised_pnl : float
    }

  let create initial_balance =
    { initial_balance; cash = initial_balance; realised_pnl = 0. }
  ;;

  let update_cash t delta_cash = t.cash <- t.cash +. delta_cash

  let update_realised_pnl t delta_pnl =
    t.realised_pnl <- t.realised_pnl +. delta_pnl
  ;;
end

module Position = struct
  type side =
    | Long
    | Short

  type t =
    { price : float
    ; lots : float
    ; side : side
    }

  let dir t =
    match t.side with
    | Long -> 1.
    | Short -> -1.
  ;;
end

module Positions = struct
  type t = Position.t list

  let create () : t = []

  let dir (t : t) =
    match t with
    | [] -> 0.
    | p :: _ -> Position.dir p
  ;;

  let net_lots (t : t) =
    List.fold_left (fun acc (p : Position.t) -> acc +. p.lots) 0. t
  ;;

  let entry_price (t : t) =
    match t with
    | lot :: _ -> Some lot.price
    | [] -> None
  ;;

  let avg_entry_price (t : t) =
    match t with
    | [] -> None
    | positions ->
      Some
        (List.fold_left
           (fun acc (p : Position.t) -> acc +. (p.price *. p.lots))
           0.
           positions
         /. net_lots t)
  ;;

  (** Returns the new lots and the PnL realized by closing against them.
  @param lot_size Size of the lot.
  @param position_list Current active positions.
  @param position The new position to close against.
  @return [new_positions, delta_pnl] representing the updated position list and the realized profit/loss. *)
  let rec add ~lot_size (t : t) (p : Position.t) =
    let rec consume t (p : Position.t) delta_pnl =
      if p.lots = 0.
      then t, delta_pnl
      else (
        match t with
        | [] -> [ p ], delta_pnl
        | pos :: _ when Position.dir pos = Position.dir p ->
          t @ [ p ], delta_pnl
        | pos :: rest ->
          let matched_lots = Float.min pos.lots p.lots in
          let delta_pnl =
            delta_pnl
            +. (Position.dir pos
                *. matched_lots
                *. lot_size
                *. (p.price -. pos.price))
          in
          let pos' = { pos with lots = pos.lots -. matched_lots } in
          let p' = { p with lots = p.lots -. matched_lots } in
          consume
            (if pos'.lots = 0. then rest else pos' :: rest)
            p'
            delta_pnl)
    in
    consume t p 0.
  ;;
end

type t =
  { lot_size : float
  ; account : Account.t
  ; mutable positions : Positions.t
  ; mutable bar : C.bar
  ; mutable clock : float
  ; mutable next_seq : int
  ; mutable orders : order list
  }

let create ~lot_size ~initial_balance =
  { lot_size
  ; account = Account.create initial_balance
  ; positions = Positions.create ()
  ; bar =
      { timestamp = 0L
      ; open_ = nan
      ; high = nan
      ; low = nan
      ; close = nan
      }
  ; clock = 0.
  ; next_seq = 0
  ; orders = []
  }
;;

let net_lots t = Positions.net_lots t.positions
let net_qty t = t.lot_size *. net_lots t
let net_dir t = Positions.dir t.positions
let qty_of_lots t lots = lots *. t.lot_size
let entry_price t = Positions.entry_price t.positions
let avg_entry_price t = Positions.avg_entry_price t.positions
let realised_pnl t = t.account.realised_pnl

let start_bar t bar =
  t.bar <- bar;
  t.clock <- 0.
;;

let end_bar t = t.orders <- []

let cancel t id =
  t.orders
  <- List.filter
       (fun (o : order) -> not (String.equal o.id id))
       t.orders
;;

let place t ~id side kind qty =
  cancel t id;
  t.orders <- t.orders @ [ { id; seq = t.next_seq; side; kind; qty } ];
  t.next_seq <- t.next_seq + 1
;;

let modify t id ?kind ?qty () =
  t.orders
  <- List.map
       (fun (o : order) ->
          if String.equal o.id id
          then
            { o with
              kind = Option.value kind ~default:o.kind
            ; qty = Option.value qty ~default:o.qty
            }
          else o)
       t.orders
;;

let bar_path (b : C.bar) =
  if b.high -. b.open_ <= b.open_ -. b.low then `OHLC else `OLHC
;;

(* when the open gaps through several stops at once they all fill at the
   open; sequence them by how far each level lies beyond the open along the
   incoming path (the level hit first on the way down/up fills first) *)
let trigger (b : C.bar) (o : order) =
  let dist p =
    match bar_path b with
    | `OHLC ->
      if p >= b.open_
      then p -. b.open_
      else b.high -. b.open_ +. (b.high -. p)
    | `OLHC ->
      if p <= b.open_
      then b.open_ -. p
      else b.open_ -. b.low +. (p -. b.low)
  in
  let at_open gap = Some (b.open_, 0., gap) in
  let reached p =
    if b.low <= p && p <= b.high then Some (p, dist p, 0.) else None
  in
  match o.kind, o.side with
  | Market, _ -> Some (b.open_, 0., 0.)
  | Limit p, Buy | Stop p, Sell ->
    if b.open_ <= p then at_open (-.p) else reached p
  | Limit p, Sell | Stop p, Buy ->
    if b.open_ >= p then at_open p else reached p
;;

let next_fill (t : t) =
  let net_dir = net_dir t in
  let blocked (o : order) =
    match o.side with
    | Buy -> net_dir > 0.
    | Sell -> net_dir < 0.
  in
  let candidates =
    List.filter_map
      (fun (o : order) ->
         if blocked o
         then None
         else (
           match trigger t.bar o with
           | Some (price, time, gap) when time >= t.clock ->
             Some (time, gap, o, price)
           | _ -> None))
      t.orders
  in
  let earlier (t1, g1, (o1 : order), _) (t2, g2, (o2 : order), _) =
    compare (t1, g1, o1.seq) (t2, g2, o2.seq)
  in
  match List.sort earlier candidates with
  | [] -> None
  | (time, _, o, price) :: _ ->
    t.clock <- time;
    cancel t o.id;
    let side =
      match o.side with
      | Buy -> Position.Long
      | Sell -> Position.Short
    in
    let delta_cash =
      -.Position.dir { lots = o.qty; price; side }
      *. qty_of_lots t o.qty
      *. price
    in
    Account.update_cash t.account delta_cash;
    let updated_positions, delta_pnl =
      Positions.add
        ~lot_size:t.lot_size
        t.positions
        { lots = o.qty; price; side }
    in
    t.positions <- updated_positions;
    Account.update_realised_pnl t.account delta_pnl;
    Some { id = o.id; side = o.side; price; qty = o.qty }
;;
