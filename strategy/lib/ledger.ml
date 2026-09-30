(* signed: +long, -short *)
type lot =
  { price : float
  ; qty : float
  }

type t =
  { lot_size : float
  ; initial_balance : float
  ; mutable cash : float
  ; mutable realized : float
  ; mutable lots : lot list
  }

let is_zero x = Float.abs x < 1e-7
let sgn x = if is_zero x then 0 else if x > 0. then 1 else -1

let create ~lot_size ~initial_balance =
  { lot_size; initial_balance; cash = initial_balance; realized = 0.; lots = [] }
;;

let net_qty t = List.fold_left (fun acc lot -> acc +. lot.qty) 0. t.lots
let side t = sgn (net_qty t)

let entry_price t =
  match t.lots with
  | lot :: _ -> Some lot.price
  | [] -> None
;;

let avg_entry_price t =
  match t.lots with
  | [] -> None
  | lots ->
    Some
      (List.fold_left (fun acc lot -> acc +. (lot.price *. lot.qty)) 0. lots /. net_qty t)
;;

let apply t ~signed_qty ~price =
  t.cash <- t.cash -. (signed_qty *. t.lot_size *. price);
  let rec offset lots remaining =
    match lots with
    | lot :: rest when sgn lot.qty = -sgn remaining ->
      let dir = Float.of_int (sgn lot.qty) in
      let closed = Float.min (Float.abs lot.qty) (Float.abs remaining) in
      t.realized <- t.realized +. (dir *. closed *. t.lot_size *. (price -. lot.price));
      let left = { lot with qty = lot.qty -. (dir *. closed) } in
      let rest = if is_zero left.qty then rest else left :: rest in
      let remaining = remaining +. (dir *. closed) in
      if is_zero remaining then rest else offset rest remaining
    | _ -> if is_zero remaining then lots else lots @ [ { price; qty = remaining } ]
  in
  t.lots <- offset t.lots signed_qty
;;
