module type S = Strategy_intf.S

module Make () : S = struct
  let g = Graph.create ()
  let const x = Graph.const g x
  let input () = Graph.input g
  let pre x = Graph.pre g x
  let delay n x = Graph.delay g n x
  let window n x = Graph.window g n x
  let loop ~init f = Graph.loop g ~init f
  let neg = Graph.neg g
  let abs = Graph.abs g
  let sqrt = Graph.sqrt g
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
  let min = Graph.min g
  let max = Graph.max g
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

  let compile () =
    g.compiled <- true;
    Runtime.compile g
  ;;

  module Indicator = struct
    let sum n s = window n s |> List.fold_left ( +~ ) @@ const 0.
    let sma n s = sum n s /~. Float.of_int n

    let ema n alpha s =
      loop
        ~init:(Node (sma n s, n))
        (fun prev_ema -> (s *~. alpha) +~ (prev_ema *~. (1.0 -. alpha)))
    ;;

    let crossover a b = cond (pre a <~ pre b &&~ (a >~ b)) (const true) (const false)
    let crossunder a b = cond (pre a >~ pre b &&~ (a <~ b)) (const true) (const false)
  end
end
