module Make =
functor
  ()
  ->
  struct
    type any = Any : 'a t -> any

    and 'a t = {
      mutable value : 'a option;
      mutable pre_value : 'a option;
      mutable rank : int;
      mutable init : (unit -> 'a option) list;
      mutable compute : unit -> unit;
    }

    let registry : any list ref = ref []
    let schedule : any list Dynarray.t = Dynarray.create ()
    let scheduled = ref 0

    let scheduler (Any a) =
      let ( .!() ) = Dynarray.get in
      let ( .!()<- ) = Dynarray.set in

      while Dynarray.length schedule <= a.rank do
        Dynarray.add_last schedule []
      done;

      schedule.!(a.rank) <- Any a :: schedule.!(a.rank)

    let register a = registry := a :: !registry
    let peek a = a.value

    let map f a =
      let rec self =
        {
          value = None;
          pre_value = None;
          rank = 1 + a.rank;
          init = [];
          compute =
            (fun () ->
              self.pre_value <- self.value;
              self.value <-
                (match peek a with Some x -> Some (f x) | _ -> None));
        }
      in
      register @@ Any self;
      self

    let zip f a b =
      let rec self =
        {
          value = None;
          pre_value = None;
          rank = 1 + max a.rank b.rank;
          init = [];
          compute =
            (fun () ->
              self.pre_value <- self.value;
              self.value <-
                (match (peek a, peek b) with
                | Some x, Some y -> Some (f x y)
                | _ -> None));
        }
      in
      register @@ Any self;
      self

    let map3 f a b c = zip (fun (a, b) c -> f a b c) (zip Pair.make a b) c

    let map4 f a b c d =
      map3 (fun (a, b) c d -> f a b c d) (zip Pair.make a b) c d

    let input () : 'a t =
      let self =
        {
          init = [];
          value = None;
          pre_value = None;
          rank = 0;
          compute = (fun () -> ());
        }
      in
      register @@ Any self;
      self

    let pre src =
      let rec self =
        {
          init = [];
          value = None;
          pre_value = None;
          rank = src.rank + 1;
          compute =
            (fun () ->
              self.pre_value <- self.value;
              self.value <- src.pre_value);
        }
      in
      register @@ Any self;
      self

    let rec delay n node = if n = 0 then node else delay (n - 1) (pre node)

    type 'a init = Const of 'a list | Copy of 'a t list

    let loop ~(init : 'a init) (f : 'a t -> 'a t) : 'a t =
      let init, init_rank =
        match init with
        | Const ps -> (
            match ps with
            | [] -> invalid_arg "loop: a recurrence needs at least one element"
            | _ -> (List.map (fun p -> fun () -> Some p) ps, 0))
        | Copy ns -> (
            match ns with
            | [] -> invalid_arg "loop: a recurrence needs at least one element"
            | _ ->
                ( List.map (fun n -> fun () -> peek n) ns,
                  List.fold_left (fun m n -> max m n.rank) 0 ns ))
      in

      let knot =
        {
          value = None;
          pre_value = None;
          rank = 0;
          init = [];
          compute = (fun () -> ());
        }
      in
      register @@ Any knot;
      let result = f knot in
      knot.compute <-
        (fun () ->
          knot.pre_value <- knot.value;
          knot.value <- peek result);
      result.init <- init;
      result.rank <- max result.rank (1 + init_rank);
      result

    let set_input a x =
      match a.rank with
      | 0 ->
          a.compute <-
            (fun () ->
              a.pre_value <- a.value;
              a.value <- Some x)
      | _ -> failwith "Cannot set a derived node"

    let compile () =
      Dynarray.clear schedule;
      List.iter scheduler !registry;
      scheduled := List.length !registry

    let step () =
      if !scheduled <> List.length !registry then failwith "forgot to compile"
      else
        Dynarray.iter
          (fun l ->
            List.iter
              (fun (Any node) ->
                match node.init with
                | w :: rest ->
                    node.pre_value <- node.value;
                    node.value <- w ();
                    node.init <- rest
                | [] -> node.compute ())
              l)
          schedule

    let ( $+ ) a b = zip ( + ) a b
    let ( $+. ) a b = zip ( +. ) a b
    let ( $* ) a b = zip ( * ) a b
    let ( $*. ) a b = zip ( *. ) a b
    let ( $- ) a b = zip ( - ) a b
    let ( $-. ) a b = zip ( -. ) a b
    let ( $/ ) a b = zip ( / ) a b
    let ( $/. ) a b = zip ( /. ) a b
  end
