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

let make g (def : 'a def) =
  if not g.compiled
  then Context.make g.ctx g.id def
  else failwith "Cannot mess with a compiled graph"
;;

let undefined g = make g Undefined Float
let const g v = make g (Const v) Float
let input g = make g Input Float
let tick g = make g Tick Float

(* constant folding: an operation on constants is itself a constant *)
let fmap g f a = make g (Language.Fmap (f, a)) Float
let gmap g f a = make g (Language.Effect (f, a)) Effect
let emap2 g f a b = make g (Language.Effect2 (f, a, b)) Effect
let lift2 g f a b = make g (Language.Lift2 (f, a, b)) Float
let cond g c t e = make g (Language.Cond (c, t, e)) Float
let pre g a = make g (Pre a) a.kind

let recurrence g body =
  let knot = make g Knot Float in
  let result = body knot in
  make g (Rec (knot, result)) Float
;;
