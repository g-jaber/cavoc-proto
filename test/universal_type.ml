(* to be tested with ./_build/default/bin/explore.exe test/universal_type.ml test/universal_type.mli *)

type t = unit

let get_set () =
  let store = ref (fun () -> (assert false)) in
  let set a = store := (fun () -> a) in
  let get () = (!store) () in
  (get,set)