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
  let read f = fmap (fun _ -> f ()) (undefined ())

  let map2 f =
    lift2 (fun a b ->
      match a, b with
      | Some a, Some b -> Some (f a b)
      | _ -> None)
  ;;

  let pure = const
  let ( <*> ) f x = map2 ( @@ ) f x
  let map f a = pure f <*> a

  let lift3 f a b c =
    lift2
      (fun ab c ->
         match ab, c with
         | Some (a, b), c -> f a b c
         | _ -> None)
      (lift2 (fun a b -> Some (a, b)) a b)
      c
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

  (*unary operations*)
  let neg = map Float.neg
  let abs = map Float.abs
  let sqrt = map Float.sqrt
  let not = map Bool.not
  let floor = map Float.floor
  let ceil = map Float.ceil

  (*binary operations*)
  let add = map2 Float.add
  let sub = map2 Float.sub
  let mul = map2 Float.mul
  let div = map2 Float.div
  let eq a b = map2 ( = ) a b
  let neq a b = map2 ( <> ) a b
  let lt = map2 ( < )
  let le = map2 ( <= )
  let gt = map2 ( > )
  let ge = map2 ( >= )
  let min = map2 Float.min
  let max = map2 Float.max
  let and_ = map2 ( && )
  let or_ = map2 ( || )
  let is_pending s = fmap (Fun.compose Option.some Option.is_none) s

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
