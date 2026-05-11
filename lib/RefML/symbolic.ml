open Util.Pmap

type symbolic_id = int [@@deriving to_yojson]

type constraints = (symbolic_id, unit) pmap

let pp_symbolic fmt = Format.fprintf fmt "s%d"
let string_of_symbolic symid = "s" ^ string_of_int symid

let constraints_to_yojson constraints =
  let aux (id, _) = `String (string_of_symbolic id) in
  let constraints = List.map aux (Util.Pmap.to_list constraints) in
  `List constraints
  

let fresh_symbolic =
  let i = ref 0 in
  (fun () -> i := !i + 1 ; !i)

let pp_constraints fmt constraints =
  let pp_pair fmt (s, _) = Format.fprintf fmt "%a = ()" pp_symbolic s in
  let pp_sep fmt () = Format.fprintf fmt ";" in
  Format.fprintf fmt "[%a]" (pp_pmap ~pp_sep pp_pair) constraints

let empty = empty

let unconstrained contraints =
  let x = fresh_symbolic () in
  (x, add (x, ()) contraints)
