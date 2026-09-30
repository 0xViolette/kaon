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

open Collections
module C = Codec.Ohlc

let next_bar ic buf =
  match Utils.read_exactly ic buf with
  | `Ok -> Some (C.decode buf)
  | `Eof -> None
  | `Truncated (recieved, expected) ->
    Utils.die "truncated record: recieved %d bytes, expected %d" recieved expected
;;

type side =
  | Buy
  | Sell

type order_kind =
  | Market
  | Limit of float
  | Stop of float

type order =
  { id : string
  ; side : side
  ; mutable kind : order_kind
  ; mutable qty : float
  }

type position =
  { price : float
  ; mutable qty : float
  }

type t =
  { ic : in_channel
  ; buf : bytes
  ; mutable market_position : int
  ; mutable net_qty : float
  ; lot_size : float
  ; mutable positions : position DList.t
  ; initial_balance : float
  ; mutable balance : float
  ; mutable net_pnl : float
  ; mutable timestamp : int64
  ; mutable open_ : float
  ; mutable high : float
  ; mutable low : float
  ; mutable close : float
  ; orders : order DList.t
  ; (* invoked with the order id, fill price and qty after each fill; lets the
       strategy layer react to fills the way a live algo reacts to order updates *)
    mutable on_fill : string -> float -> float -> unit
  }

let is_zero x =
  let epsilon = 1e-7 in
  Float.abs x < epsilon
;;

let sgn x = if x > 0. then 1 else if is_zero x then 0 else -1

let process_orders t =
  let add_position p =
    let rec loop (cur_node : position DList.node option) =
      match cur_node with
      | Some node ->
        let pos = node.value in
        if sgn pos.qty * sgn p.qty = -1
        then
          if abs_float pos.qty > abs_float p.qty
          then (
            pos.qty <- pos.qty +. p.qty;
            t.net_pnl <- t.net_pnl +. (p.qty *. t.lot_size *. (pos.price -. p.price));
            p.qty <- 0.)
          else (
            p.qty <- p.qty +. pos.qty;
            t.net_pnl <- t.net_pnl +. (pos.qty *. t.lot_size *. (p.price -. pos.price));
            pos.qty <- 0.);
        if is_zero pos.qty then DList.remove t.positions node;
        if not (is_zero p.qty) then loop node.next
      | None -> DList.append t.positions p
    in
    loop (DList.head t.positions)
  in
  let process_order (o : order) =
    let fill price =
      let new_position = { price; qty = o.qty } in
      (match o.side with
       | Buy ->
         t.balance <- t.balance -. (o.qty *. t.lot_size *. price);
         t.net_qty <- t.net_qty +. o.qty
       | Sell ->
         new_position.qty <- -1. *. new_position.qty;
         t.balance <- t.balance +. (o.qty *. t.lot_size *. price);
         t.net_qty <- t.net_qty -. o.qty);
      (match t.net_qty with
       | x when x > 0. -> t.market_position <- 1
       | x when x < 0. -> t.market_position <- -1
       | _ -> t.market_position <- 0);
      add_position new_position;
      Printf.eprintf "balance: %f\n" t.balance;
      Printf.eprintf "pnl: %f\n" t.net_pnl;
      Printf.eprintf "market position: %d\n" t.market_position;
      Printf.eprintf "position: %f\n\n" t.net_qty;
      Some price
    in
    match o.kind with
    | Market -> fill t.open_
    | Limit price ->
      (match o.side with
       | Buy ->
         if t.open_ <= price
         then fill t.open_
         else if t.low <= price
         then fill price
         else None
       | Sell ->
         if t.open_ >= price
         then fill t.open_
         else if t.high >= price
         then fill price
         else None)
    | Stop price ->
      (match o.side with
       | Buy ->
         if t.open_ >= price
         then fill t.open_
         else if t.high >= price
         then fill price
         else None
       | Sell ->
         if t.open_ <= price
         then fill t.open_
         else if t.low <= price
         then fill price
         else None)
  in
  let rec loop (cur_node : order DList.node option) =
    match cur_node with
    | Some node ->
      (match process_order node.value with
       | Some price ->
         DList.remove t.orders node;
         t.on_fill node.value.id price node.value.qty;
         (* on_fill may cancel or resize siblings, so restart from the head;
            unfilled orders re-check to the same result *)
         loop (DList.head t.orders)
       | None -> loop node.next)
    | None -> ()
  in
  loop (DList.head t.orders);
  (* one-bar duration, like EasyLanguage: an order that does not fill on the
     bar it is active for is cancelled; the strategy re-issues it next bar *)
  DList.clear t.orders
;;

let create ?(initial_balance = 0.) () =
  Utils.refuse_tty ();
  { ic = In_channel.stdin
  ; lot_size = 50.
  ; buf = Bytes.create C.message_size
  ; initial_balance
  ; balance = initial_balance
  ; net_pnl = 0.
  ; market_position = 0
  ; positions = DList.create ()
  ; net_qty = 0.0
  ; timestamp = Int64.of_int 0
  ; open_ = nan
  ; high = nan
  ; low = nan
  ; close = nan
  ; orders = DList.create ()
  ; on_fill = (fun _ _ _ -> ())
  }
;;

let place_order b id side kind qty =
  let rec loop (cur_node : order DList.node option) =
    match cur_node with
    | Some node -> if node.value.id = id then cur_node else loop node.next
    | None -> None
  in
  match loop (DList.head b.orders) with
  | Some node ->
    node.value.qty <- qty;
    node.value.kind <- kind
  | None ->
    let new_order = { id; side; kind; qty } in
    DList.append b.orders new_order
;;

let cancel_order b id =
  let rec loop (cur_node : order DList.node option) =
    match cur_node with
    | Some node ->
      if String.equal node.value.id id then DList.remove b.orders node else loop node.next
    | None -> ()
  in
  loop (DList.head b.orders)
;;

(* like the modify endpoint of a real broker: adjust a resting order in
   place, keeping its id and its position in the queue *)
let modify_order b id ?kind ?qty () =
  let rec loop (cur_node : order DList.node option) =
    match cur_node with
    | Some node ->
      if String.equal node.value.id id
      then (
        (match kind with
         | Some kind -> node.value.kind <- kind
         | None -> ());
        match qty with
        | Some qty -> node.value.qty <- qty
        | None -> ())
      else loop node.next
    | None -> ()
  in
  loop (DList.head b.orders)
;;

let entry_price b =
  match DList.head b.positions with
  | Some node -> Some node.value.price
  | None -> None
;;

let average_entry_price b =
  if DList.head b.positions = None
  then None
  else (
    let rec loop (cur_node : position DList.node option) (acc : float) =
      match cur_node with
      | Some node -> loop node.next (acc +. (node.value.price *. node.value.qty))
      | None -> acc
    in
    Some (loop (DList.head b.positions) 0. /. b.net_qty))
;;

let step (b : t) =
  match next_bar b.ic b.buf with
  | Some bar ->
    b.timestamp <- bar.timestamp;
    b.open_ <- bar.open_;
    b.high <- bar.high;
    b.low <- bar.low;
    b.close <- bar.close;
    process_orders b;
    true
  | None -> false
;;
