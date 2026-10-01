(* signed: +long, -short *)
type position =
  { price : float
  ; lots : float
  }

type t =
  { lot_size : float
  ; initial_balance : float
  ; mutable cash : float
  ; mutable realized : float
  ; mutable positions : position list
  }

let is_zero x = Float.abs x < 1e-7
let dir x = if is_zero x then 0. else if x > 0. then 1. else -1.

let create ~lot_size ~initial_balance =
  { lot_size
  ; initial_balance
  ; cash = initial_balance
  ; realized = 0.
  ; positions = []
  }
;;

let qty_from_lots t lots = lots *. t.lot_size

let net_lots t =
  List.fold_left
    (fun acc position -> acc +. position.lots)
    0.
    t.positions
;;

let side t = dir (net_lots t)

let entry_price t =
  match t.positions with
  | lot :: _ -> Some lot.price
  | [] -> None
;;

let avg_entry_price t =
  match t.positions with
  | [] -> None
  | positions ->
    Some
      (List.fold_left
         (fun acc position ->
            acc +. (position.price *. position.lots))
         0.
         positions
       /. net_lots t)
;;

let apply t ~lots ~price =
  let qty = qty_from_lots t lots in
  t.cash <- t.cash -. (qty *. price);
  let rec consume positions lots_to_fill =
    match positions with
    | position :: rest when dir position.lots <> dir lots_to_fill ->
      let matched_lots =
        dir position.lots
        *. Float.min
             (Float.abs position.lots)
             (Float.abs lots_to_fill)
      in
      let matched_qty = qty_from_lots t matched_lots in
      t.realized
      <- t.realized +. (matched_qty *. (price -. position.price));
      let remaining_position =
        { position with lots = position.lots -. matched_lots }
      in
      let rest =
        if is_zero remaining_position.lots
        then rest
        else remaining_position :: rest
      in
      let lots_to_fill = lots_to_fill +. matched_lots in
      if is_zero lots_to_fill then rest else consume rest lots_to_fill
    | _ ->
      if is_zero lots_to_fill
      then positions
      else positions @ [ { price; lots = lots_to_fill } ]
  in
  t.positions <- consume t.positions lots
;;
