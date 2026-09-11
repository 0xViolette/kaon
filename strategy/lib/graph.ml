module Make =
functor
  ()
  ->
  struct
    type packed_node = Pack : 'a node -> packed_node

    and dep =
      | Binary of (packed_node * packed_node)
      | Unary of packed_node
      | Rec of packed_node
      | Empty

    and 'a node = {
      mutable value : 'a option;
      mutable prev_value : 'a option;
      mutable height : int;
      mutable dep : dep;
      mutable warmup : (unit -> 'a option) list;
      mutable compute : unit -> unit;
    }

    let nodes : packed_node list ref = ref []
    let register node = nodes := Pack node :: !nodes
    let value a = a.value

    let map f a =
      let rec self =
        {
          value = None;
          prev_value = None;
          height = 1 + a.height;
          dep = Unary (Pack a);
          warmup = [];
          compute =
            (fun () ->
              self.prev_value <- self.value;
              self.value <-
                (match value a with Some x -> Some (f x) | _ -> None));
        }
      in
      register self;
      self

    let map2 f a b =
      let rec self =
        {
          value = None;
          prev_value = None;
          height = 1 + max a.height b.height;
          warmup = [];
          dep = Binary (Pack a, Pack b);
          compute =
            (fun () ->
              self.prev_value <- self.value;
              self.value <-
                (match (value a, value b) with
                | Some x, Some y -> Some (f x y)
                | _ -> None));
        }
      in
      register self;
      self

    let map3 f a b c = map2 (fun (a, b) c -> f a b c) (map2 Pair.make a b) c

    let map4 f a b c d =
      map3 (fun (a, b) c d -> f a b c d) (map2 Pair.make a b) c d

    let var () =
      let self =
        {
          warmup = [];
          value = None;
          prev_value = None;
          height = 0;
          dep = Empty;
          compute = (fun () -> ());
        }
      in
      register self;
      self

    let pre src =
      let rec self =
        {
          warmup = [];
          value = None;
          prev_value = None;
          height = src.height + 1;
          dep = Unary (Pack src);
          compute =
            (fun () ->
              self.prev_value <- self.value;
              self.value <- src.prev_value);
        }
      in
      register self;
      self

    let rec delay n node = if n = 0 then node else delay (n - 1) (pre node)

    type 'a seed = Points of 'a list | Nodes of 'a node list

    let rec_node ~(seeds : 'a seed) (f : 'a node -> 'a node) : 'a node =
      let warm, h =
        match seeds with
        | Points ps -> (
            match ps with
            | [] -> invalid_arg "rec_node: a recurrence needs at least one seed"
            | _ -> (List.map (fun p -> fun () -> Some p) ps, 0))
        | Nodes ns -> (
            match ns with
            | [] -> invalid_arg "rec_node: a recurrence needs at least one seed"
            | _ ->
                let ret = List.map (fun n -> fun () -> value n) ns in
                (ret, List.fold_left (fun acc a -> max acc a.height) 0 ns))
      in

      let self =
        {
          value = None;
          prev_value = None;
          height = 0;
          dep = Empty;
          warmup = [];
          compute = (fun () -> ());
        }
      in
      register self;
      let body = f self in
      self.dep <- Rec (Pack body);
      self.compute <-
        (fun () ->
          self.prev_value <- self.value;
          self.value <- value body);
      body.warmup <- warm;
      body.height <- max body.height (1 + h);
      body

    let set a x =
      match a.dep with
      | Empty ->
          a.prev_value <- a.value;
          a.value <- Some x
      | _ -> failwith "Cannot set a derived node"

    let ordered_nodes : packed_node array option ref = ref None

    let compile () =
      let arr = Array.of_list !nodes in
      Array.sort (fun (Pack n1) (Pack n2) -> compare n1.height n2.height) arr;
      ordered_nodes := Some arr

    let stabilise () =
      match !ordered_nodes with
      | None -> failwith "graph hasn't been compiled"
      | Some nodes ->
          Array.iter
            (fun (Pack node) ->
              match node.warmup with
              | w :: rest ->
                  node.prev_value <- node.value;
                  node.value <- w ();
                  node.warmup <- rest
              | [] -> node.compute ())
            nodes

    let ( $+ ) a b = map2 ( + ) a b
    let ( $+. ) a b = map2 ( +. ) a b
    let ( $* ) a b = map2 ( * ) a b
    let ( $*. ) a b = map2 ( *. ) a b
    let ( $- ) a b = map2 ( - ) a b
    let ( $-. ) a b = map2 ( -. ) a b
    let ( $/ ) a b = map2 ( / ) a b
    let ( $/. ) a b = map2 ( /. ) a b
  end
