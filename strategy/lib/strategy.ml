module Make =
functor
  ()
  ->
  struct
    let g = Graph.create ()
    let const x = Graph.const g x
    let input () = Graph.input g
    let pre x = Graph.pre g x
    let delay n x = Graph.delay g n x
    let window n x = Graph.window g n x
    let loop ~init f = Graph.loop g ~init f
    let int_add = Graph.int_add g
    let int_sub = Graph.int_sub g
    let int_mul = Graph.int_mul g
    let int_div = Graph.int_div g
    let int_lt = Graph.int_lt g
    let float_add = Graph.float_add g
    let float_sub = Graph.float_sub g
    let float_mul = Graph.float_mul g
    let float_div = Graph.float_div g
    let float_lt = Graph.float_lt g
    let int_neg = Graph.int_neg g
    let float_neg = Graph.float_neg g
    let ( +~ ) = int_add
    let ( +.~ ) = float_add
    let ( *~ ) = int_mul
    let ( *.~ ) = float_mul
    let ( -~ ) = int_sub
    let ( -.~ ) = float_sub
    let ( /~ ) = int_div
    let ( /.~ ) = float_div

    let compile () =
      g.compiled <- true;
      Runtime.compile g
    ;;
  end
