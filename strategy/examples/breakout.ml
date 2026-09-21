open Lib

(* Volatility-shock entry + ATR trailing stop, under the simple loop
   semantics: [loop ~init body] copies init.(i) VERBATIM on bar i (Some
   or None), then runs the body. There is no warmup bookkeeping — the
   user seeds with values that are valid at the bar they are consumed.

   Strategy:
   - ENTRY: close jumps >5% over the previous close (unknown bar).
   - FILL:  latch the close of the shock bar.
   - STOP:  once in, trail max(prev_stop, close - 2*ATR(6)); exit when
     close falls through the stop.

   Act A (the mistake): seeding the stop loop through [init] with the
   trailing-stop expression. init.(0) is copied on bar 0 — where the ATR
   is None — and that None enters the recursion permanently. The stop is
   dead; the exit never fires. (The fill survives only because its body
   selects [close] on the event bar without touching [prev] — bodies that
   use prev strictly have no such rescue.)

   Act B (the fix): event-driven and history-dependent seeds belong in
   the BODY, not in init. [default] falls back to the seed while the
   recursion is still None (Pine's nz); [cond] gates the strategy on the
   entry event. No fixed bar arithmetic anywhere — the strategy starts
   whenever the event fires and the ATR happens to be ready. *)

let pp_float = function
  | None -> "None"
  | Some x -> Printf.sprintf "%.4g" x
;;

let pp_bool = function
  | None -> "None"
  | Some b -> if b then "true" else "false"
;;

(* ~fixed:false = Act A (init-seeded, dead); ~fixed:true = Act B. *)
let build m ~fixed =
  let module M = (val m : Strategy_intf.S) in
  let open M in
  let close = input () in
  let shock = close >~ pre close *~. 1.05 in
  let tr = abs (close -~ pre close) in
  let atr6 =
    loop ~init:[||] (fun p ->
      default ((tr *~. (1. /. 6.)) +~ (p *~. (5. /. 6.))) (Indicator.sma 6 tr))
  in
  let trail = close -~ (atr6 *~. 2.) in
  if fixed
  then (
    (* latch the fill price: on the shock bar take close, else keep prev *)
    let fill = loop ~init:[||] (fun prev -> cond shock close prev) in
    (* entered = Some true from the first shock on *)
    let entered = Indicator.bars_since shock >=~ const 0. in
    (* trail the stop once in; [default prev trail] bootstraps the ratchet
       from the trail itself until the stop has a value *)
    let stop =
      loop ~init:[||] (fun prev ->
        cond entered (Indicator.max (default prev trail) trail) prev)
    in
    let exit = close <~ stop in
    compile ();
    fun c ->
      step [ Set (close, c) ];
      value shock, value fill, value stop, value exit)
  else (
    let fill =
      loop ~init:[| cond shock close (undefined ()) |] (fun prev -> cond shock close prev)
    in
    let stop = loop ~init:[| trail |] (fun prev -> Indicator.max prev trail) in
    let exit = close <~ stop in
    compile ();
    fun c ->
      step [ Set (close, c) ];
      value shock, value fill, value stop, value exit)
;;

let run name closes expected_stop ~fixed =
  let module M = Strategy.Make () in
  let step = build (module M : Strategy_intf.S) ~fixed in
  Printf.printf "%s\n" name;
  Printf.printf
    "%-4s %-6s %-7s %-7s %-9s %-6s %s\n"
    "bar"
    "close"
    "shock?"
    "fill"
    "stop"
    "exit?"
    "note";
  let mismatches = ref 0 in
  Array.iteri
    (fun i c ->
       let shock, fill, stop, exit = step c in
       let note =
         if shock = Some true
         then "ENTRY shock"
         else if exit = Some true
         then "STOP HIT -> exit"
         else ""
       in
       let ok =
         match stop, expected_stop.(i) with
         | None, None -> true
         | Some a, Some e -> Float.abs (a -. e) < 1e-4
         | _ -> false
       in
       if not ok then incr mismatches;
       Printf.printf
         "%-4d %-6g %-7s %-7s %-9s %-6s %s %s\n"
         i
         c
         (pp_bool shock)
         (pp_float fill)
         (pp_float stop)
         (pp_bool exit)
         note
         (if ok then "" else "<-- MISMATCH"))
    closes;
  if !mismatches = 0
  then Printf.printf "  (all bars as expected)\n\n"
  else Printf.printf "  %d MISMATCH(ES)\n\n" !mismatches
;;

(* Early shock: entry on bar 2, four bars before the ATR exists. *)
let early =
  [| 100.; 101.; 107.; 108.; 110.; 112.; 115.; 117.; 116.; 114.; 111.; 108.; 105.; 104. |]
;;

(* Act A — the init copies trail's bar-0 None: the stop (and the strategy)
   never comes to life. *)
let () =
  run
    "Act A: stop seeded via init (verbatim copy of a not-yet-valid signal)"
    early
    (Array.make 14 None)
    ~fixed:false
;;

(* Act B — body-side seeding: the stop stays None until the ATR exists
   (honest: no stop before that), then trails and catches the decline. *)
let () =
  run
    "Act B: stop seeded in the body via default/cond — early shock"
    early
    [| None
     ; None
     ; None
     ; None
     ; None
     ; None
     ; Some 110. (* first bar the ATR exists: 115 - 2*2.5 *)
     ; Some 112.1667 (* ratchet: 117 - 2*2.4167 *)
     ; Some 112.1667
     ; Some 112.1667
     ; Some 112.1667 (* close 111 < stop: STOP HIT *)
     ; Some 112.1667
     ; Some 112.1667
     ; Some 112.1667
    |]
    ~fixed:true
;;

(* Act B on late-shock data: same code, entry on bar 9; the ATR is long
   since ready, so the stop starts on the entry bar itself. *)
let () =
  run
    "Act B: same code, late shock (bar 9)"
    [| 100.
     ; 100.
     ; 100.
     ; 100.
     ; 100.
     ; 100.
     ; 100.
     ; 100.
     ; 100.
     ; 112.
     ; 113.
     ; 115.
     ; 118.
     ; 117.
    |]
    [| None
     ; None
     ; None
     ; None
     ; None
     ; None
     ; None
     ; None
     ; None
     ; Some 108. (* 112 - 2*2 *)
     ; Some 109.3333
     ; Some 111.2778
     ; Some 113.8981
     ; Some 113.8981
    |]
    ~fixed:true
;;
