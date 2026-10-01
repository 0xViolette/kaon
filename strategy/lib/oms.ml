open Order

type intent =
  | Go_long of float
    (* be long [target]; covers a short first; no pyramiding *)
  | Go_short of float
  | Close_long (* flatten a long; no-op otherwise *)
  | Close_short

type desired =
  { tag : string
  ; intent : intent
  ; kind : kind
  }

let side_of = function
  | Go_long _ | Close_short -> Buy
  | Go_short _ | Close_long -> Sell
;;

let qty intent ~net =
  let s = Ledger.dir net in
  match intent with
  | Go_long target -> if s > 0. then target else target -. net
  | Go_short target -> if s < 0. then target else target +. net
  | Close_long -> if s > 0. then net else 0.
  | Close_short -> if s < 0. then -.net else 0.
;;

let submit broker desired =
  let net = Broker.net_qty broker in
  List.filter_map
    (fun d ->
       let q = qty d.intent ~net in
       if q > 0.
       then (
         Broker.place broker ~id:d.tag (side_of d.intent) d.kind q;
         Some d)
       else None)
    desired
;;

let realign broker resting =
  let net = Broker.net_qty broker in
  List.iter
    (fun d ->
       let q = qty d.intent ~net in
       if q > 0.
       then Broker.modify broker d.tag ~qty:q ()
       else Broker.cancel broker d.tag)
    resting
;;

let settle broker resting =
  let rec go fills =
    match Broker.next_fill broker with
    | None -> List.rev fills
    | Some f ->
      realign broker resting;
      go (f :: fills)
  in
  go []
;;
