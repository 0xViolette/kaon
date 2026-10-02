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

type t =
  { ledger : Ledger.t
  ; mutable bar : C.bar
  ; mutable clock : float
  ; mutable next_seq : int
  ; mutable orders : order list
  }

let create ~lot_size ~initial_balance =
  { ledger = Ledger.create ~lot_size ~initial_balance
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

let net_qty t = Ledger.net_lots t.ledger

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

(* when the open gaps through several stops at once they all fill at the
   open; sequence them by how far each level lies beyond the open along the
   incoming path (the level hit first on the way down/up fills first) *)
let trigger (b : C.bar) (o : order) =
  let high_first = b.high -. b.open_ <= b.open_ -. b.low in
  let dist p =
    if high_first
    then
      if p >= b.open_
      then p -. b.open_
      else b.high -. b.open_ +. (b.high -. p)
    else if p <= b.open_
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

let next_fill t =
  let net = net_qty t in
  let blocked (o : order) =
    match o.side with
    | Buy -> net > 0.
    | Sell -> net < 0.
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
    let signed_qty =
      match o.side with
      | Buy -> o.qty
      | Sell -> -.o.qty
    in
    Ledger.apply t.ledger ~lots:signed_qty ~price;
    Some { id = o.id; side = o.side; price; qty = o.qty }
;;
