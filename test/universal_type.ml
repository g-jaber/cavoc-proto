type t = unit

let get_set =
  let store = ref (assert false) in
  let set a = store := a in
  let get () = !store in
  (get,set)