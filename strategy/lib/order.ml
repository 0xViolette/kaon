type side =
  | Buy
  | Sell

type 'a kind =
  | Market
  | Limit of 'a
  | Stop of 'a

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
  ; kind : float kind
  ; qty : float
  }

type request =
  { tag : string
  ; action : action
  ; kind : float kind
  }

and action =
  | Go_long of float
    (* be long [target]; covers a short first; no pyramiding *)
  | Go_short of float
  | Close_long (* flatten a long; no-op otherwise *)
  | Close_short
