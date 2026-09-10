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
      mutable warmup : 'a option list;
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

    let rec_node (seeds : 'a list) (f : 'a node list -> 'a node) : 'a node =
      match seeds with
      | [] -> invalid_arg "rec_node: a recurrence needs at least one seed"
      | seeds ->
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
          let rec delay n node =
            if n = 0 then node else delay (n - 1) (pre node)
          in
          let taps = List.init (List.length seeds) (fun i -> delay i self) in
          let body = f taps in
          self.dep <- Rec (Pack body);
          self.compute <-
            (fun () ->
              self.prev_value <- self.value;
              self.value <- value body);
          body.warmup <- List.map (fun s -> Some s) seeds;
          body

    let rec1 s0 f =
      rec_node [ s0 ] (function [ x ] -> f x | _ -> assert false)

    let rec2 s0 s1 f =
      rec_node [ s0; s1 ] (function [ x; y ] -> f x y | _ -> assert false)

    let set a x =
      match a.dep with
      | Empty -> a.value <- Some x
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
              | s :: rest ->
                  node.prev_value <- node.value;
                  node.value <- s;
                  node.warmup <- rest
              | [] -> node.compute ())
            nodes
  end
