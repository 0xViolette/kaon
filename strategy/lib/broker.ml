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

let message_size = 40

type bar_tick =
  { timestamp : int64
  ; open_ : float
  ; high : float
  ; low : float
  ; close : float
  }

let decode bytes : bar_tick =
  let bytes_to_float offset = Int64.float_of_bits (Bytes.get_int64_le bytes offset) in
  { timestamp = Bytes.get_int64_le bytes 0
  ; open_ = bytes_to_float 8
  ; high = bytes_to_float 16
  ; low = bytes_to_float 24
  ; close = bytes_to_float 32
  }
;;

let really_read ic buf =
  let len = Bytes.length buf in
  let rec fill off =
    if off = len
    then `Ok
    else (
      match In_channel.input ic buf off (len - off) with
      | 0 -> if off = 0 then `Eof else `Truncated off
      | n -> fill (off + n))
  in
  fill 0
;;

let next_bar ic buf =
  match really_read ic buf with
  | `Ok -> Some (decode buf)
  | `Eof -> None
  | `Truncated n -> Utils.die "truncated record: got %d bytes, expected %d" n message_size
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

module DList = struct
  type 'a node =
    { value : 'a
    ; mutable prev : 'a node option
    ; mutable next : 'a node option
    }

  type 'a t =
    { mutable head : 'a node option
    ; mutable tail : 'a node option
    }

  let append t x =
    let new_node = { value = x; prev = None; next = None } in
    match t.tail with
    | Some tail ->
      new_node.prev <- Some tail;
      tail.next <- Some new_node;
      t.tail <- Some new_node
    | None ->
      t.head <- Some new_node;
      t.tail <- Some new_node
  ;;

  let remove t node =
    (match node.prev with
     | Some prev -> prev.next <- node.next
     | None -> t.head <- node.next);
    match node.next with
    | Some next -> next.prev <- node.prev
    | None -> t.tail <- node.prev
  ;;

  let create () = { head = None; tail = None }
end

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
    loop t.positions.head
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
      Printf.printf "balance: %f\n" t.balance;
      Printf.printf "pnl: %f\n" t.net_pnl;
      Printf.printf "market position: %d\n" t.market_position;
      Printf.printf "position: %f\n\n" t.net_qty;
      true
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
         else false
       | Sell ->
         if t.open_ >= price
         then fill t.open_
         else if t.high >= price
         then fill price
         else false)
    | Stop price ->
      (match o.side with
       | Buy ->
         if t.open_ >= price
         then fill t.open_
         else if t.high >= price
         then fill price
         else false
       | Sell ->
         if t.open_ <= price
         then fill t.open_
         else if t.low <= price
         then fill price
         else false)
  in
  let rec loop (cur_node : order DList.node option) =
    match cur_node with
    | Some node ->
      if process_order node.value then DList.remove t.orders node;
      loop node.next;
      ()
    | None -> ()
  in
  loop t.orders.head
;;

let create ?(initial_balance = 0.) () =
  Utils.refuse_tty ();
  { ic = In_channel.stdin
  ; lot_size = 50.
  ; buf = Bytes.create message_size
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
  }
;;

let place_order b id side kind qty =
  let rec loop (cur_node : order DList.node option) =
    match cur_node with
    | Some node -> if node.value.id = id then cur_node else loop node.next
    | None -> None
  in
  match loop b.orders.head with
  | Some node ->
    node.value.qty <- qty;
    node.value.kind <- kind
  | None ->
    (match side with
     | Buy -> Printf.printf "placed Buy order\n"
     | Sell -> Printf.printf "placed Sell order\n");
    flush_all ();
    let new_order = { id; side; kind; qty } in
    DList.append b.orders new_order
;;

let entry_price b =
  match b.positions.head with
  | Some node -> Some node.value.qty
  | None -> None
;;

let average_entry_price b =
  if b.positions.head = None
  then None
  else (
    let rec loop (cur_node : position DList.node option) (acc : float) =
      match cur_node with
      | Some node -> loop node.next (acc +. (node.value.price *. node.value.qty))
      | None -> acc
    in
    Some (loop b.positions.head 0. /. b.net_qty))
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
