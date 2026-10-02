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

type request =
  { tag : string
  ; action : action
  ; kind : kind
  }

and action =
  | Go_long of float
    (* be long [target]; covers a short first; no pyramiding *)
  | Go_short of float
  | Close_long (* flatten a long; no-op otherwise *)
  | Close_short
