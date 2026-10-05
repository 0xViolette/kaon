include Strategy_intf

module Make () : S = struct
  type 'a signal = 'a Language.signal
  type set = Runtime.set = Set : float signal * float -> set

  let g = Graph.create ()
  let r = ref None
  let tick : float signal = Graph.tick g

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
  let pre s = Graph.pre g s

  let rec delay n s =
    match n with
    | 0 -> s
    | n when n > 0 -> delay (n - 1) (pre s)
    | _ -> invalid_arg "delay: cannot be negative"
  ;;

  let rec window length s =
    if length < 0
    then invalid_arg "window: lookback cannot be negative";
    if length = 0 then [] else s :: window (length - 1) (pre s)
  ;;

  let recurrence f = Graph.recurrence g f
  let lift2 f a b = Graph.lift2 g f a b
  let fmap f a = Graph.fmap g f a
  let gmap f a = Graph.gmap g f a
  let emap2 f a b = Graph.emap2 g f a b
  let cond c t e = Graph.cond g c t e
  let read f = gmap (fun _ -> f ()) (undefined ())
  let map f = fmap (fun a -> if Float.is_nan a then nan else f a)

  let map2 f =
    lift2 (fun a b ->
      if Float.is_nan a || Float.is_nan b then nan else f a b)
  ;;

  (* booleans are floats: [0.] is false, anything else true, [nan] pending *)
  let of_bool b = if b then 1. else 0.

  (*unary operations*)
  let neg = map Float.neg
  let abs = map Float.abs
  let sqrt = map Float.sqrt
  let not = map (fun b -> of_bool (b = 0.))
  let floor = map Float.floor
  let ceil = map Float.ceil

  (*binary operations*)
  let add = map2 Float.add
  let sub = map2 Float.sub
  let mul = map2 Float.mul
  let div = map2 Float.div
  let eq = map2 (fun a b -> of_bool (a = b))
  let neq = map2 (fun a b -> of_bool (a <> b))
  let lt = map2 (fun a b -> of_bool (a < b))
  let le = map2 (fun a b -> of_bool (a <= b))
  let gt = map2 (fun a b -> of_bool (a > b))
  let ge = map2 (fun a b -> of_bool (a >= b))
  let min = map2 Float.min
  let max = map2 Float.max
  let and_ = map2 (fun a b -> of_bool (a <> 0. && b <> 0.))
  let or_ = map2 (fun a b -> of_bool (a <> 0. || b <> 0.))
  let is_pending = fmap (fun a -> of_bool (Float.is_nan a))

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
    let ( && ) = and_
    let ( || ) = or_
    let ( = ) = eq
    let ( <> ) = neq
    let abs = abs
    let sqrt = sqrt
    let not = not
    let floor = floor
    let ceil = ceil
  end

  let value_when event signal =
    recurrence (fun prev -> cond event signal prev)
  ;;

  let compile () =
    match !r with
    | Some _ -> failwith "already compiled"
    | None ->
      g.compiled <- true;
      r := Some (Runtime.compile g)
  ;;

  module Indicator = struct
    let sum n s =
      let acc =
        recurrence (fun prev ->
          cond (is_pending prev) s Ops.(prev + s))
      in
      recurrence (fun prev ->
        cond
          (is_pending prev)
          (cond (is_pending (delay (n - 1) s)) (undefined ()) acc)
          Ops.(prev + s - delay n s))
    ;;

    let sma n s = Ops.(sum n s / !(Float.of_int n))
    let min = min
    let max = max

    let ema n s =
      let alpha = Float.div 2. (float_of_int (n + 1)) in
      recurrence (fun prev_ema ->
        cond
          (is_pending prev_ema)
          (sma n s)
          Ops.((s * !alpha) + (prev_ema * !(Float.sub 1.0 alpha))))
    ;;

    let crossover a b = Ops.(pre a < pre b && a > b)
    let crossunder a b = Ops.(pre a > pre b && a < b)

    let highest n s =
      window n s |> List.fold_left max @@ const Float.neg_infinity
    ;;

    let lowest n s =
      window n s |> List.fold_left min @@ const Float.infinity
    ;;

    let bars_since c =
      recurrence Ops.(fun prev -> cond c !0. (prev + !1.0))
    ;;
  end
end
