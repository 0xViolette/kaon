module type S = Strategy_intf.S

module Make () : S = struct
  type 'a signal = 'a Language.signal
  type set = Runtime.set = Set : 'a signal * 'a -> set

  let g = Graph.create ()
  let r = ref None
  let tick : int signal = Graph.tick g

  let step setters =
    match !r with
    | Some r -> Runtime.step r setters
    | None -> failwith "forgot to compile"
  ;;

  let value s =
    match !r with
    | Some r -> Runtime.value r s
    | None -> failwith "forgot to compile"
  ;;

  let undefined () = Graph.undefined g
  let const x = Graph.const g x
  let input () = Graph.input g
  let pre x = Graph.pre g x
  let delay n x = Graph.delay g n x
  let window n x = Graph.window g n x
  let recurrence f = Graph.recurrence g f
  let neg = Graph.neg g
  let abs = Graph.abs g
  let sqrt = Graph.sqrt g
  let not = Graph.not g
  let floor = Graph.floor g
  let ceil = Graph.ceil g
  let is_pending s = Graph.is_pending g s
  let add = Graph.add g
  let add_const_r s x = add s (const x)
  let add_const_l x s = add (const x) s
  let sub = Graph.sub g
  let sub_const_r s x = sub s (const x)
  let sub_const_l x s = sub (const x) s
  let mul = Graph.mul g
  let mul_const_r s x = mul s (const x)
  let mul_const_l x s = mul (const x) s
  let div = Graph.div g
  let div_const_r s x = div s (const x)
  let div_const_l x s = div (const x) s
  let lt = Graph.lt g
  let lt_const_r s x = lt s (const x)
  let lt_const_l x s = lt (const x) s
  let le = Graph.le g
  let le_const_r s x = le s (const x)
  let le_const_l x s = le (const x) s
  let gt = Graph.gt g
  let gt_const_r s x = gt s (const x)
  let gt_const_l x s = gt (const x) s
  let ge = Graph.ge g
  let ge_const_r s x = ge s (const x)
  let ge_const_l x s = ge (const x) s
  let ( +~ ) = add
  let ( +~. ) = add_const_r
  let ( +.~ ) = add_const_l
  let ( -~ ) = sub
  let ( -~. ) = sub_const_r
  let ( -.~ ) = sub_const_l
  let ( *~ ) = mul
  let ( *~. ) = mul_const_r
  let ( *.~ ) = mul_const_l
  let ( /~ ) = div
  let ( /~. ) = div_const_r
  let ( /.~ ) = div_const_l
  let ( <~ ) = lt
  let ( <~. ) = lt_const_r
  let ( <.~ ) = lt_const_l
  let ( <=~ ) = le
  let ( <=~. ) = le_const_r
  let ( <=.~ ) = le_const_l
  let ( >~ ) = gt
  let ( >~. ) = gt_const_r
  let ( >.~ ) = gt_const_l
  let ( >=~ ) = ge
  let ( >=~. ) = ge_const_r
  let ( >=.~ ) = ge_const_l
  let ( &&~ ) = Graph.and_ g
  let ( ||~ ) = Graph.or_ g
  let ( <>~ ) a b = Graph.neq g a b
  let ( =~ ) a b = Graph.eq g a b
  let fmap f a = Graph.fmap g f a
  let lift2 f a b = Graph.lift2 g f a b

  let lift3 f a b c =
    lift2
      (fun ab c ->
         match ab with
         | Some f -> f c
         | None -> None)
      (lift2 (fun a b -> Some (fun c -> f a b c)) a b)
      c
  ;;

  let lift4 f a b c d =
    lift2
      (fun abc d ->
         match abc with
         | Some f -> f d
         | None -> None)
      (lift3 (fun a b c -> Some (fun d -> f a b c d)) a b c)
      d
  ;;

  let cond c t e =
    lift3
      (fun c t e ->
         match c with
         | Some true -> t
         | Some false -> e
         | None -> None)
      c
      t
      e
  ;;

  let value_when event signal = recurrence (fun prev -> cond event signal prev)

  let compile () =
    match !r with
    | Some _ -> failwith "already compiled"
    | None ->
      g.compiled <- true;
      r := Some (Runtime.compile g)
  ;;

  module Indicator = struct
    (* let sum n s = window n s |> List.fold_left ( +~ ) @@ const 0. *)
    let sum n s =
      let acc = recurrence (fun prev -> cond (is_pending prev) s (prev +~ s)) in
      recurrence (fun prev ->
        cond
          (is_pending prev)
          (cond (is_pending (delay (n - 1) s)) (undefined ()) acc)
          (prev +~ s -~ delay n s))
    ;;

    let sma n s = sum n s /~. Float.of_int n
    let min = Graph.min g
    let max = Graph.max g

    let ema n s =
      let alpha = 2. /. float_of_int (n + 1) in
      recurrence (fun prev_ema ->
        cond
          (is_pending prev_ema)
          (sma n s)
          ((s *~. alpha) +~ (prev_ema *~. (1.0 -. alpha))))
    ;;

    let crossover a b = pre a <~ pre b &&~ (a >~ b)
    let crossunder a b = pre a >~ pre b &&~ (a <~ b)
    let highest n s = window n s |> List.fold_left max @@ const Float.neg_infinity
    let lowest n s = window n s |> List.fold_left min @@ const Float.infinity
    let bars_since c = recurrence (fun prev -> cond c (const 0.) (prev +~. 1.0))
  end
end
