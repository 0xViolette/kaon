module Make =
functor
  ()
  ->
  struct
    type anyNode = Any : 'a node -> anyNode [@@unboxed]

    and 'a node =
      { mutable value : 'a option
      ; mutable previous_value : 'a option
      ; mutable compute : unit -> unit
      ; mutable rank : int
      ; kind : 'a kind
      }

    and 'a kind =
      | Input
      | Const
      | Derived
      | Knot

    let reject_const who node =
      match node.kind with
      | Const -> invalid_arg (who ^ ": not allowed for const node")
      | _ -> ()
    ;;

    let registry : anyNode list ref = ref []
    let schedule : anyNode array ref = ref [||]
    let register node = registry := node :: !registry
    let value_of a = a.value

    let map f src =
      let rec mapped =
        { value = None
        ; previous_value = None
        ; rank = 1 + src.rank
        ; kind = Derived
        ; compute =
            (fun () ->
              mapped.previous_value <- mapped.value;
              mapped.value
              <- (match value_of src with
                  | Some x -> Some (f x)
                  | _ -> None))
        }
      in
      register @@ Any mapped;
      mapped
    ;;

    let zip f l r =
      let rec zipped =
        { value = None
        ; previous_value = None
        ; rank = 1 + max l.rank r.rank
        ; kind = Derived
        ; compute =
            (fun () ->
              zipped.previous_value <- zipped.value;
              zipped.value
              <- (match value_of l, value_of r with
                  | Some x, Some y -> Some (f x y)
                  | _ -> None))
        }
      in
      register @@ Any zipped;
      zipped
    ;;

    let map3 f a b c = zip (fun (a, b) c -> f a b c) (zip Pair.make a b) c
    let map4 f a b c d = map3 (fun (a, b) c d -> f a b c d) (zip Pair.make a b) c d

    let const v : 'a node =
      { value = Some v
      ; previous_value = Some v
      ; compute = (fun () -> ())
      ; rank = 0
      ; kind = Const
      }
    ;;

    let input () : 'a node =
      let node =
        { value = None
        ; previous_value = None
        ; compute = (fun () -> ())
        ; rank = 0
        ; kind = Input
        }
      in
      register @@ Any node;
      node
    ;;

    let pre src =
      reject_const "pre" src;
      let rec self =
        { value = None
        ; previous_value = None
        ; rank = src.rank + 1
        ; kind = Derived
        ; compute =
            (fun () ->
              self.previous_value <- self.value;
              self.value <- src.previous_value)
        }
      in
      register @@ Any self;
      self
    ;;

    let rec delay n node =
      match n with
      | 0 -> node
      | n when n > 0 ->
        reject_const "delay" node;
        delay (n - 1) (pre node)
      | _ -> invalid_arg "delay: cannot be negative"
    ;;

    let rec window length node =
      if length < 0 then invalid_arg "window: lookback cannot be negative";
      if length = 0
      then []
      else (
        reject_const "window" node;
        node :: window (length - 1) (pre node))
    ;;

    type 'a init =
      | Nodes of 'a node array
      | Node of 'a node * int

    let loop ~(init : 'a init) (body : 'a node -> 'a node) : 'a node =
      (match init with
       | Nodes nodes ->
         if Array.length nodes = 0 then invalid_arg "loop: init nodes cannot be empty"
       | Node (_, len) ->
         if len <= 0 then invalid_arg "loop: init length must be positive");
      let initial_rank =
        match init with
        | Nodes nodes ->
          Array.fold_left (fun max_rank node -> max max_rank node.rank) 0 nodes
        | Node (node, _) -> node.rank
      in
      let knot =
        { value = None
        ; previous_value = None
        ; rank = 0
        ; kind = Knot
        ; compute = (fun () -> ())
        }
      in
      register @@ Any knot;
      let raw_result = body knot in
      let result =
        match raw_result.kind with
        | Derived -> raw_result
        | _ -> map (fun x -> x) raw_result
      in
      knot.compute
      <- (fun () ->
           knot.previous_value <- knot.value;
           knot.value <- value_of result);
      let raw_compute = result.compute in
      result.rank <- max result.rank (1 + initial_rank);
      let index = ref 0 in
      match init with
      | Nodes nodes ->
        result.compute
        <- (fun () ->
             result.previous_value <- result.value;
             result.value <- value_of nodes.(!index);
             incr index;
             if !index >= Array.length nodes then result.compute <- raw_compute);
        result
      | Node (node, len) ->
        result.compute
        <- (fun () ->
             result.previous_value <- result.value;
             result.value <- value_of node;
             incr index;
             if !index >= len then result.compute <- raw_compute);
        result
    ;;

    let set_input node value =
      match node.kind with
      | Input ->
        node.compute
        <- (fun () ->
             node.previous_value <- node.value;
             node.value <- Some value)
      | Const -> invalid_arg "set_input: not allowed for const node"
      | _ -> failwith "set_input: cannot set a derived node"
    ;;

    let compile () =
      let nodes = Array.of_list (List.rev !registry) in
      Array.stable_sort (fun (Any l) (Any r) -> compare l.rank r.rank) nodes;
      schedule := nodes
    ;;

    let eval () =
      let nodes = !schedule in
      if Array.length nodes <> List.length !registry
      then failwith "forgot to compile"
      else
        for i = 0 to Array.length nodes - 1 do
          let (Any node) = nodes.(i) in
          node.compute ()
        done
    ;;

    let ( +~ ) = zip Int.add
    let ( +.~ ) = zip Float.add
    let ( *~ ) = zip Int.mul
    let ( *.~ ) = zip Float.mul
    let ( -~ ) = zip Int.sub
    let ( -.~ ) = zip Float.sub
    let ( /~ ) = zip Int.div
    let ( /.~ ) = zip Float.div
  end
