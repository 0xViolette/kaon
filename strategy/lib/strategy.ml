include Strategy_intf

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
  let is_pending s = Graph.is_pending g s
  let add = Graph.add g
  let sub = Graph.sub g
  let mul = Graph.mul g
  let div = Graph.div g
  let lt = Graph.lt g
  let le = Graph.le g
  let gt = Graph.gt g
  let ge = Graph.ge g

  module Ops = struct
    let ( ! ) = const
    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div
    let ( < ) = lt
    let ( <= ) = le
    let ( > ) = gt
    let ( >= ) = ge
    let ( && ) = Graph.and_ g
    let ( || ) = Graph.or_ g
    let ( = ) a b = Graph.eq g a b
    let ( <> ) a b = Graph.neq g a b
    let abs = Graph.abs g
    let sqrt = Graph.sqrt g
    let not = Graph.not g
    let floor = Graph.floor g
    let ceil = Graph.ceil g
  end

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
    open Ops

    (* let sum n s = window n s |> List.fold_left ( +~ ) @@ const 0. *)
    let sum n s =
      let acc = recurrence (fun prev -> cond (is_pending prev) s (prev + s)) in
      recurrence (fun prev ->
        cond
          (is_pending prev)
          (cond (is_pending (delay (Int.sub n 1) s)) (undefined ()) acc)
          (prev + s - delay n s))
    ;;

    let sma n s = sum n s / !(Float.of_int n)
    let min = Graph.min g
    let max = Graph.max g

    let ema n s =
      let alpha = Float.div 2. (float_of_int (Int.add n 1)) in
      recurrence (fun prev_ema ->
        cond
          (is_pending prev_ema)
          (sma n s)
          ((s * !alpha) + (prev_ema * !(Float.sub 1.0 alpha))))
    ;;

    let crossover a b = pre a < pre b && a > b
    let crossunder a b = pre a > pre b && a < b
    let highest n s = window n s |> List.fold_left max @@ const Float.neg_infinity
    let lowest n s = window n s |> List.fold_left min @@ const Float.infinity
    let bars_since c = recurrence (fun prev -> cond c !0. (prev + !1.0))
  end
end
