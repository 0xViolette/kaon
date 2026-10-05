type t =
  { ctx : Context.t
  ; mutable compiled : bool
  ; id : int
  }

let fresh_token =
  let c = ref 0 in
  fun () ->
    let token = !c in
    incr c;
    token
;;

let create () =
  { ctx = Context.create (); compiled = false; id = fresh_token () }
;;

open Language

let make g def =
  if not g.compiled
  then Context.make g.ctx g.id def
  else failwith "Cannot mess with a compiled graph"
;;

let undefined g = make g Undefined
let const g v = make g (Const v)
let input g = make g Input
let tick g = make g Tick

(* constant folding: an operation on constants is itself a constant *)
let fmap g f a = make g (Language.Fmap (f, a))
let lift2 g f a b = make g (Language.Lift2 (f, a, b))

let pre g a =
  match a.def with
  | Const _ -> a
  | _ -> make g (Pre a)
;;

(* [loop ~init body]: on bar i < length(init) the loop copies init.(i)
   *verbatim* (Some or None — exactly what the source holds that bar);
   from bar length(init) on it takes the value of [body knot], where [knot]
   holds the loop's value from the previous bar. init = [||] starts the
   body on bar 0. A None handed to the recursion is usually permanent:
   seed with values that are valid at the bar they are consumed. *)
let recurrence g (body : 'a Language.signal -> 'a Language.signal)
  : 'a Language.signal
  =
  let knot = make g Knot in
  let result = body knot in
  make g (Rec (knot, result))
;;
