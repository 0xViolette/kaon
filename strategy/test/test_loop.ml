open Lib

(* Loop semantics suite: [loop ~init body] copies init.(i) verbatim on
   bar i (Some or None), then runs [body]. No warmup metadata, no waiting:
   the user seeds with values that are valid at the bar they are consumed.
   Each scenario runs a fresh graph against a hand-computed or
   reference-recurrence expectation. *)

let failures = ref 0
let tol = 1e-9

let opt_close a b =
  match a, b with
  | None, None -> true
  | Some x, Some y -> Float.abs (x -. y) < tol
  | _ -> false
;;

let pp_float = function
  | None -> "None"
  | Some x -> Printf.sprintf "%.6g" x
;;

type driver =
  { step_in : float -> unit
  ; read : unit -> float option
  }

let run name inputs expected mk =
  let module M = Strategy.Make () in
  let d = mk (module M : Strategy_intf.S) in
  Printf.printf "== %s\n" name;
  Array.iteri
    (fun i x ->
       d.step_in x;
       let got = d.read () in
       let want = expected.(i) in
       let ok = opt_close got want in
       if not ok then incr failures;
       Printf.printf
         "  bar %-2d in=%-6g got=%-10s want=%-10s %s\n"
         i
         x
         (pp_float got)
         (pp_float want)
         (if ok then "OK" else "MISMATCH"))
    inputs;
  print_newline ()
;;

(* ---------- reference recurrences (independent of the DSL) ---------- *)

let sma_series n xs =
  Array.mapi
    (fun i _ ->
       if i < n - 1
       then None
       else (
         let acc = ref 0. in
         for j = i - (n - 1) to i do
           acc := !acc +. xs.(j)
         done;
         Some (!acc /. Float.of_int n)))
    xs
;;

(* ema n s given s as a float option series: seed = first bar where the
   trailing n values of s are all Some; then the standard recurrence.
   Assumes s stays Some once it becomes Some (true for our scenarios). *)
let ema_series n s =
  let alpha = 2. /. Float.of_int (n + 1) in
  let out = Array.make (Array.length s) None in
  let prev = ref None in
  Array.iteri
    (fun i _ ->
       match !prev with
       | None ->
         if i >= n - 1
         then (
           let acc = ref 0. in
           let ok = ref true in
           for j = i - (n - 1) to i do
             match s.(j) with
             | Some v -> acc := !acc +. v
             | None -> ok := false
           done;
           if !ok
           then (
             let seed = !acc /. Float.of_int n in
             out.(i) <- Some seed;
             prev := Some seed))
       | Some p ->
         (match s.(i) with
          | Some v ->
            let e = (v *. alpha) +. (p *. (1. -. alpha)) in
            out.(i) <- Some e;
            prev := Some e
          | None -> prev := None))
    s;
  out
;;

let bars_since_series threshold xs =
  let out = Array.make (Array.length xs) None in
  let cnt = ref None in
  Array.iteri
    (fun i x ->
       if x > threshold
       then (
         out.(i) <- Some 0.;
         cnt := Some 0.)
       else (
         match !cnt with
         | Some c ->
           out.(i) <- Some (c +. 1.);
           cnt := Some (c +. 1.)
         | None -> ()))
    xs;
  out
;;

(* ------------------------------- scenarios ------------------------------- *)

(* 1. Plain ema on an input: sma seed while the recursion is None, then
   the recursion takes over. *)
let () =
  let xs = [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8. |] in
  let expected = ema_series 3 (Array.map (fun x -> Some x) xs) in
  run "ema 3 inp" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = Indicator.ema 3 inp in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

(* 2. ema of a history-carrying signal: the seed (sma 3 of sma 4) takes
   as long as it takes; [default] keeps selecting it until the recursion
   has a value. *)
let () =
  let xs = [| 2.; 4.; 6.; 8.; 10.; 12.; 14.; 16.; 18.; 20.; 22.; 24. |] in
  let expected = ema_series 3 (sma_series 4 xs) in
  run "ema 3 (sma 4 inp)" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = Indicator.(ema 3 (sma 4 inp)) in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

(* 3. ema of an event-driven signal: the seed is None until two bars after
   the event fires; the loop simply keeps seeding until then. *)
let () =
  let xs = [| 1.; 1.; 9.; 1.; 1.; 2.; 1.; 1.; 1.; 1. |] in
  let expected = ema_series 3 (bars_since_series 5. xs) in
  run "ema 3 (bars_since (inp > 5))" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = Indicator.(ema 3 (bars_since (inp >~ const 5.))) in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

(* 4. Nested loops: ema of ema. *)
let () =
  let xs = [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. |] in
  let expected = ema_series 2 (ema_series 3 (Array.map (fun x -> Some x) xs)) in
  run "ema 2 (ema 3 inp) — nested loops" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = Indicator.(ema 2 (ema 3 inp)) in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

(* 5. Verbatim per-bar seeds: init.(i) is copied on bar i, then the body. *)
let () =
  let xs = [| 1.; 2.; 3.; 4.; 5.; 6. |] in
  let expected = [| Some 0.; Some 1.; Some 2.; Some 3.; Some 4.; Some 5. |] in
  run "init [|const 0; const 1; const 2|], body = prev + 1" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = loop ~init:[| const 0.; const 1.; const 2. |] (fun p -> p +~. 1.) in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

(* 6. One seed repeated for n bars is Array.make — no second construct. *)
let () =
  let xs = [| 1.; 2.; 3.; 4.; 5.; 6. |] in
  let expected = [| Some 7.; Some 7.; Some 7.; Some 8.; Some 9.; Some 10. |] in
  run "init (Array.make 3 (const 7)), body = prev + 1" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = loop ~init:(Array.make 3 (const 7.)) (fun p -> p +~. 1.) in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

(* 7. Empty init is legal: the body runs from bar 0; [default] bootstraps
   the recursion from a fallback while the knot is still None. *)
let () =
  let xs = [| 1.; 2.; 3.; 4. |] in
  let expected = [| Some 10.; Some 11.; Some 12.; Some 13. |] in
  run "init [||] + default bootstrap" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = loop ~init:[||] (fun p -> default (p +~. 1.) (const 10.)) in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

(* 8. THE CONTRACT, stated negatively: init is verbatim. sma 2 inp is None
   on bar 0, the loop copies that None into the recursion, and the loop
   never recovers. Seeding with not-yet-valid signals is a user error —
   use [default] in the body (scenario 7) for history-carrying seeds. *)
let () =
  let xs = [| 1.; 2.; 3.; 4.; 5.; 6. |] in
  let expected = [| None; None; None; None; None; None |] in
  run "verbatim init: seeding a None kills the loop" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = loop ~init:[| Indicator.sma 2 inp |] (fun p -> p +~. 1.) in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

(* 9. Latch: hold the value of [inp] from the last bar where it exceeded
   5 — event-driven state with no init at all. *)
let () =
  let xs = [| 1.; 1.; 9.; 1.; 1.; 8.; 1. |] in
  let expected = [| None; None; Some 9.; Some 9.; Some 9.; Some 8.; Some 8. |] in
  run "latch: cond event value prev" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = loop ~init:[||] (fun prev -> cond (inp >~ const 5.) inp prev) in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

(* 10. default pointwise: the first signal while it is Some, else the
   second. *)
let () =
  let xs = [| 1.; 2.; 3.; 4.; 5.; 6. |] in
  let expected = [| Some 5.; Some 5.; Some 1.; Some 2.; Some 3.; Some 4. |] in
  run "default (delay 2 inp) (const 5)" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = default (delay 2 inp) (const 5.) in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

(* 11. bar_index is [tick], which counts bars from 1 (the runtime seeds
   the counter at Some 1 on the first bar), so we read it as a float. *)
let () =
  let xs = [| 1.; 2.; 3.; 4.; 5. |] in
  let expected = [| Some 1.; Some 2.; Some 3.; Some 4.; Some 5. |] in
  run "bar_index" xs expected (fun m ->
    let module M = (val m : Strategy_intf.S) in
    let open M in
    let inp = input () in
    let probe = map Float.of_int tick in
    compile ();
    { step_in = (fun x -> step [ Set (inp, x) ]); read = (fun () -> value probe) })
;;

let () =
  if !failures = 0
  then print_endline "All loop scenarios behaved as expected."
  else (
    Printf.eprintf "%d mismatch(es) found.\n" !failures;
    exit 1)
;;
