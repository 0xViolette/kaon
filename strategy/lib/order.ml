type side =
  | Buy
  | Sell

type kind =
  | Market
  | Limit of float
  | Stop of float

type fill =
  { id : string
  ; side : side
  ; price : float
  ; qty : float
  }

type order =
  { id : string
  ; seq : int
  ; side : side
  ; kind : kind
  ; qty : float
  }
