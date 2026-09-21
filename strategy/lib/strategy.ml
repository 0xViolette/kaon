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
  let loop ~init f = Graph.loop g ~init f
  let neg = Graph.neg g
  let abs = Graph.abs g
  let sqrt = Graph.sqrt g
  let not = Graph.not g
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
  let default a b = Graph.default g a b
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
    let sum n s = window n s |> List.fold_left ( +~ ) @@ const 0.
    let sma n s = sum n s /~. Float.of_int n
    let min = Graph.min g
    let max = Graph.max g

    let ema n s =
      let alpha = 2. /. float_of_int (n + 1) in
      (* recurse once the recursion has a value; until then fall back to
         the sma seed — [default] keeps selecting the seed while the body
         is None, so the seed's own warmup takes as long as it takes *)
      loop ~init:[||] (fun prev_ema ->
        default ((s *~. alpha) +~ (prev_ema *~. (1.0 -. alpha))) (sma n s))
    ;;

    let crossover a b = pre a <~ pre b &&~ (a >~ b)
    let crossunder a b = pre a >~ pre b &&~ (a <~ b)
    let highest n s = window n s |> List.fold_left max @@ const Float.neg_infinity
    let lowest n s = window n s |> List.fold_left min @@ const Float.infinity
    let bars_since c = loop ~init:[||] (fun prev -> cond c (const 0.) (prev +~. 1.0))
  end
end
