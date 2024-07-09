type 'a namespan = ('a, 'a) Pmap.pmap

let empty_nspan = Pmap.empty
let id_nspan l = Pmap.list_to_pmap @@ List.map (fun x -> (x, x)) l
let add_nspan = Pmap.add_span
let is_in_dom_im = Pmap.is_in_dom_im

let rec combine = function
  | ([],[]) -> Pmap.empty
  | (n1::l1,n2::l2) -> Pmap.add (n1,n2) (combine (l1,l2))
  | _ -> failwith "Error: cannot build a span, the two lists have different lengths."

let string_of_span string_of_name nspan =
  Pmap.string_of_pmap "" "," string_of_name string_of_name nspan

let pp_namespan pp_name fmt nspan =
  let pp_empty fmt () = Format.fprintf fmt "⋅" in
  let pp_pair fmt (n1,n2) = Format.fprintf fmt "%a : %a" pp_name n1 pp_name n2 in
  Pmap.pp_pmap ~pp_empty pp_pair fmt nspan

