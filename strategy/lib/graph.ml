let next_id = ref 0
let _version = ref 0

let fresh_id () =
  let id = !next_id in
  incr next_id;
  id

type packed_node = Pack : 'a node -> packed_node

and 'a node =
  | Source of { id : int; mutable value : 'a }
  | Derived of {
      id : int;
      mutable version : int;
      mutable value : 'a;
      deps : packed_node array;
      compute : unit -> 'a;
    }

let rec update : type a. a node -> unit = function
  | Source _ -> ()
  | Derived d ->
      if d.version <> !_version then begin
        Array.iter (fun (Pack dep) -> update dep) d.deps;

        d.value <- d.compute ();
        d.version <- !_version
      end

let value = function Source s -> s.value | Derived d -> d.value

let read a =
  update a;
  value a

let version = function Source _ -> !_version | Derived d -> d.version

let map f a =
  Derived
    {
      id = fresh_id ();
      version = version a;
      value = f (read a);
      deps = [| Pack a |];
      compute = (fun () -> f (value a));
    }

let map2 f a b =
  if version a <> version b then failwith "versions of a and b don't match"
  else
    Derived
      {
        id = fresh_id ();
        version = version a;
        value = f (value a) (value b);
        deps = [| Pack a; Pack b |];
        compute = (fun () -> f (value a) (value b));
      }

let create_source v = Source { id = fresh_id (); value = v }

let set a v =
  incr _version;
  match a with
  | Derived _ -> failwith "cannot set a derived node"
  | Source s -> s.value <- v

(*  example current usage

open Graph

let a = create_source 3
let b = create_source 4
let c = map2 (fun a b -> (a * a) + (b * b)) a b;;

print_int (read c);
print_newline ();

set a 5;
set b 12;

print_int (read c);
print_newline ()

*)
