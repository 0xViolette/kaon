module Make =
functor
  ()
  ->
  struct
    type anyNode = Any : 'a node -> anyNode [@@unboxed]

    and 'a node =
      { mutable value : 'a option
      ; mutable pre_value : 'a option
      ; mutable compute : unit -> unit
      ; mutable rank : int
      ; kind : 'a kind
      }

    and 'a kind =
      | Input
      | Const
      | Derived
      | Knot

    let registry : anyNode list ref = ref []
    let schedule : anyNode array ref = ref [||]
    let register a = registry := a :: !registry
    let peek a = a.value

    let map f a =
      let rec self =
        { value = None
        ; pre_value = None
        ; rank = 1 + a.rank
        ; kind = Derived
        ; compute =
            (fun () ->
              self.pre_value <- self.value;
              self.value
              <- (match peek a with
                  | Some x -> Some (f x)
                  | _ -> None))
        }
      in
      register @@ Any self;
      self
    ;;

    let zip f a b =
      let rec self =
        { value = None
        ; pre_value = None
        ; rank = 1 + max a.rank b.rank
        ; kind = Derived
        ; compute =
            (fun () ->
              self.pre_value <- self.value;
              self.value
              <- (match peek a, peek b with
                  | Some x, Some y -> Some (f x y)
                  | _ -> None))
        }
      in
      register @@ Any self;
      self
    ;;

    let map3 f a b c = zip (fun (a, b) c -> f a b c) (zip Pair.make a b) c
    let map4 f a b c d = map3 (fun (a, b) c d -> f a b c d) (zip Pair.make a b) c d

    let const v : 'a node =
      { value = Some v
      ; pre_value = Some v
      ; compute = (fun () -> ())
      ; rank = 0
      ; kind = Const
      }
    ;;

    let input () : 'a node =
      let self =
        { value = None
        ; pre_value = None
        ; compute = (fun () -> ())
        ; rank = 0
        ; kind = Input
        }
      in
      register @@ Any self;
      self
    ;;

    let pre src =
      if src.kind = Const then invalid_arg "pre: not allowed for const node";
      let rec self =
        { value = None
        ; pre_value = None
        ; rank = src.rank + 1
        ; kind = Derived
        ; compute =
            (fun () ->
              self.pre_value <- self.value;
              self.value <- src.pre_value)
        }
      in
      register @@ Any self;
      self
    ;;

    let rec delay n node =
      match n with
      | 0 -> node
      | n when n > 0 -> delay (n - 1) (pre node)
      | _ -> invalid_arg "delay: cannot be negative"
    ;;

    let rec window n node =
      if n < 0 then invalid_arg "window: lookback cannot be negative";
      if n = 0 then [] else node :: window (n - 1) (pre node)
    ;;

    type 'a init =
      | Consts of 'a list
      | Nodes of 'a node list

    let loop ~(init : 'a init) (f : 'a node -> 'a node) : 'a node =
      let init, init_rank =
        match init with
        | Consts ps ->
          (match ps with
           | [] -> invalid_arg "loop: a recurrence needs at least one element"
           | _ -> List.map (fun p -> fun () -> Some p) ps, 0)
        | Nodes ns ->
          (match ns with
           | [] -> invalid_arg "loop: a recurrence needs at least one element"
           | _ ->
             ( List.map (fun n -> fun () -> peek n) ns
             , List.fold_left (fun m n -> max m n.rank) 0 ns ))
      in
      let knot =
        { value = None
        ; pre_value = None
        ; rank = 0
        ; kind = Knot
        ; compute = (fun () -> ())
        }
      in
      register @@ Any knot;
      let result = f knot in
      let result =
        match result.kind with
        | Derived -> result
        | _ -> map (fun x -> x) result
      in
      knot.compute
      <- (fun () ->
           knot.pre_value <- knot.value;
           knot.value <- peek result);
      let raw = result.compute in
      let inits = Array.of_list init in
      let idx = ref 0 in
      result.compute
      <- (fun () ->
           result.pre_value <- result.value;
           result.value <- inits.(!idx) ();
           incr idx;
           if !idx = Array.length inits then result.compute <- raw);
      result.rank <- max result.rank (1 + init_rank);
      result
    ;;

    let set_input a x =
      match a.kind with
      | Input ->
        a.compute
        <- (fun () ->
             a.pre_value <- a.value;
             a.value <- Some x)
      | _ -> failwith "Cannot set a derived node"
    ;;

    let compile () =
      let arr = Array.of_list (List.rev !registry) in
      Array.stable_sort (fun (Any a) (Any b) -> compare a.rank b.rank) arr;
      schedule := arr
    ;;

    let eval () =
      let ns = !schedule in
      if Array.length ns <> List.length !registry
      then failwith "forgot to compile"
      else
        for i = 0 to Array.length ns - 1 do
          let (Any n) = ns.(i) in
          n.compute ()
        done
    ;;

    let ( +~ ) = zip ( + )
    let ( +.~ ) = zip ( +. )
    let ( *~ ) = zip ( * )
    let ( *.~ ) = zip ( *. )
    let ( -~ ) = zip ( - )
    let ( -.~ ) = zip ( -. )
    let ( /~ ) = zip ( / )
    let ( /.~ ) = zip ( /. )
  end
