type ('a, 'b) pmap = ('a * 'b) list

let empty = []
let is_empty = function [] -> true | _ -> false
let singleton (a, b) = [ (a, b) ]
let concat p1 p2 = p1 @ p2
let list_to_pmap l = l
let dom pmap = List.map fst pmap
let codom pmap = List.map snd pmap
let mem = List.mem_assoc

let rec lookup x = function
  | [] -> None
  | (y, v) :: _ when x = y -> Some v
  | _ :: pmap -> lookup x pmap

let rec lookup_exn x = function
  | [] -> raise Not_found
  | (y, v) :: _ when x = y -> v
  | _ :: pmap -> lookup_exn x pmap

let rec is_in_dom_im (a, b) = function
  | [] -> false
  | (a', b') :: p -> if a = a' && b = b' then true else is_in_dom_im (a, b) p

let add (a, b) p = (a, b) :: p

let add_span (a, b) p =
  if is_in_dom_im (a, b) p then None else Some (add (a, b) p)

let rec modadd_pmap (x, v) = function
  | [] -> [ (x, v) ]
  | (y, _) :: pmap when x = y -> (y, v) :: pmap
  | hd :: pmap -> hd :: modadd_pmap (x, v) pmap

let rec modadd_pmap2 (x1, v1) (x2, v2) = function
  | [] -> [ (x1, v1); (x2, v2) ]
  | (y, _) :: pmap when x1 = y -> (y, v1) :: modadd_pmap (x2, v2) pmap
  | (y, _) :: pmap when x2 = y -> (y, v2) :: modadd_pmap (x1, v1) pmap
  | hd :: pmap -> hd :: modadd_pmap2 (x1, v1) (x2, v2) pmap

let rec string_of_pmap empty sep string_of_dom string_of_im = function
  | [] -> empty
  | [ (x, v) ] -> string_of_dom x ^ sep ^ string_of_im v
  | (x, v) :: pmap ->
      string_of_dom x ^ sep ^ string_of_im v ^ ", "
      ^ string_of_pmap empty sep string_of_dom string_of_im pmap

let map_dom f = List.map (fun (x, v) -> (f x, v))
let map_im f = List.map (fun (x, v) -> (x, f v))
let map = List.map
let map_list = List.map
let filter_map = List.filter_map
let fold = List.fold_left

let disjoint pmap1 pmap2 =
  List.for_all (fun (x, _) -> not @@ (List.mem_assoc x) pmap2) pmap1

let rec select_im b = function
  | [] -> []
  | (a, b') :: tl -> if b = b' then a :: select_im b tl else select_im b tl
