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
  let add_const_r s x = Graph.add g s (const x)
  let add_const_l x s = Graph.add g s (const x)
  let sub = Graph.sub g
  let sub_const_r s x = Graph.sub g s (const x)
  let sub_const_l x s = Graph.sub g s (const x)
  let mul = Graph.mul g
  let mul_const_r s x = Graph.mul g s (const x)
  let mul_const_l x s = Graph.mul g s (const x)
  let div = Graph.div g
  let div_const_r s x = Graph.div g s (const x)
  let div_const_l x s = Graph.div g s (const x)
  let lt = Graph.lt g
  let le = Graph.le g
  let gt = Graph.gt g
  let ge = Graph.ge g
  let cond i t e = Graph.cond g i t e
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
  let ( <=~ ) = le
  let ( >~ ) = gt
  let ( >=~ ) = ge
  let ( &&~ ) = Graph.and_ g
  let ( ||~ ) = Graph.or_ g
  let ( !=~ ) a b = Graph.neq g a b
  let ( =~ ) a b = Graph.eq g a b
  let map f a = Graph.map g f a

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
