type side =
  | Buy
  | Sell

type order_kind =
  | Limit of float
  | Stop of float
  | StopLimit of float

type order =
  { side : side
  ; kind : order_kind
  ; qty : float
  }

type t =
  { mutable market_position : int
  ; orders : order Queue.t
  }

let create () = { market_position = 0; orders = Queue.create () }
let create_order side kind qty = { side; kind; qty }
let place_order b (o : order) = Queue.push o b.orders
let market_position b = b.market_position
