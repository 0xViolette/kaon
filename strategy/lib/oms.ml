open Order

let side_of = function
  | Go_long _ | Close_short -> Buy
  | Go_short _ | Close_long -> Sell
;;

(* net is directional (+/-) *)
let qty action ~net =
  match action with
  | Go_long target -> if net > 0. then target else target -. net
  | Go_short target -> if net < 0. then target else target +. net
  | Close_long -> if net > 0. then net else 0.
  | Close_short -> if net < 0. then -.net else 0.
;;

let submit b requests =
  let net = Broker.net_dir b *. Broker.net_lots b in
  List.filter_map
    (fun request ->
       let q = qty request.action ~net in
       if q > 0.
       then (
         Broker.place
           b
           ~id:request.tag
           (side_of request.action)
           request.kind
           q;
         Some request)
       else None)
    requests
;;

let realign b resting_requests =
  let net = Broker.net_dir b *. Broker.net_lots b in
  List.iter
    (fun request ->
       let q = qty request.action ~net in
       if q > 0.
       then Broker.modify b request.tag ~qty:q ()
       else Broker.cancel b request.tag)
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
