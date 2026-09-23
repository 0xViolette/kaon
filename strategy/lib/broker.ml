type side =
  | Buy
  | Sell

type order_kind =
  | Market
  | Limit of float
  | Stop of float
  | StopLimit of
      { stop : float
      ; limit : float
      }

type order =
  { side : side
  ; kind : order_kind
  ; qty : float
  }

module OrderList = struct
  type node =
    { value : order
    ; mutable prev : node option
    ; mutable next : node option
    }

  type t =
    { mutable head : node option
    ; mutable tail : node option
    }

  let append t (o : order) =
    let new_node = { value = o; prev = None; next = None } in
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
  ; mutable timestamp : int64 option
  ; mutable open_ : float
  ; mutable high : float
  ; mutable low : float
  ; mutable close : float
  ; orders : OrderList.t
  }

(* let ic = In_channel.stdin *)
(* let buf = Bytes.create message_size *)

let message_size = 40

let create () =
  Utils.refuse_tty ();
  { ic = In_channel.stdin
  ; buf = Bytes.create message_size
  ; market_position = 0
  ; timestamp = None
  ; open_ = nan
  ; high = nan
  ; low = nan
  ; close = nan
  ; orders = OrderList.create ()
  }
;;

let place_order b side kind qty =
  (match side with
   | Buy -> Printf.printf "placed Buy order\n"
   | Sell -> Printf.printf "placed Sell order\n");
  flush_all ();
  let new_order = { side; kind; qty } in
  OrderList.append b.orders new_order
;;

let market_position b = b.market_position

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

let step (b : t) =
  match next_bar b.ic b.buf with
  | Some bar ->
    b.timestamp <- Some bar.timestamp;
    b.open_ <- bar.open_;
    b.high <- bar.high;
    b.low <- bar.low;
    b.close <- bar.close;
    true
  | None -> false
;;
