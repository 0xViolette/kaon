open Order

let side_of = function
  | Go_long _ | Close_short -> Buy
  | Go_short _ | Close_long -> Sell
;;

let qty action ~net =
  let s = Ledger.dir net in
  match action with
  | Go_long target -> if s > 0. then target else target -. net
  | Go_short target -> if s < 0. then target else target +. net
  | Close_long -> if s > 0. then net else 0.
  | Close_short -> if s < 0. then -.net else 0.
;;

let submit broker requests =
  let net = Broker.net_qty broker in
  List.filter_map
    (fun request ->
       let q = qty request.action ~net in
       if q > 0.
       then (
         Broker.place
           broker
           ~id:request.tag
           (side_of request.action)
           request.kind
           q;
         Some request)
       else None)
    requests
;;

let realign broker resting_requests =
  let net = Broker.net_qty broker in
  List.iter
    (fun request ->
       let q = qty request.action ~net in
       if q > 0.
       then Broker.modify broker request.tag ~qty:q ()
       else Broker.cancel broker request.tag)
    resting_requests
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
