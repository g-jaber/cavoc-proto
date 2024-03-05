(* We consider three kind of names:
   - Function names
   - Continuation names
   - Polymorphic names
*)

type id = string


type name = FName of id | CName of id | PName of id

let is_fname = function FName _ -> true | _ -> false
let is_cname = function CName _ -> true | _ -> false
let trim_name_id id =
  if id.[0] = '_' then String.sub id 1 (String.length id - 1)
  else failwith @@ "The id " ^ id ^ "does not start with _. It is not a name."

let fname_of_id id = FName id
let count_fname = ref 0

let fresh_fname () =
  let fn = !count_fname in
  count_fname := !count_fname + 1;
  FName ("f" ^ string_of_int fn)

let count_cname = ref 0

let fresh_cname () =
  let cn = !count_cname in
  count_cname := !count_cname + 1;
  "c" ^ string_of_int cn

let count_pname = ref 0

let fresh_pname () =
  let pn = !count_pname in
  count_pname := !count_pname + 1;
  PName ("p" ^ string_of_int pn)

let string_of_name = function FName f -> f | CName c -> c | PName p -> p

let compare s s' = 
  String.compare (string_of_name s) (string_of_name s')

let cname_of_id id = CName id
let cname_to_id = function CName cn -> Some cn | _ -> None

type cont_name = id

let string_of_cont_name cn = cn
let get_cont_name = cname_to_id
let inj_cont_name = cname_of_id

let dummy_cn = fresh_cname ()

let is_callable = function
  | FName _ -> true
  | CName _ -> true
  | PName _ -> false


(* Name manipulations *)

(*module NameSet = Set.Make(
  struct 
    type t = name
    let compare = compare
  end)
*)

module NameMap = Map.Make(
  struct 
    type t = name
    let compare = compare
end)
  
type 'a nmap = 'a NameMap.t

let nmap_empty = NameMap.empty
let nmap_mem = NameMap.mem
let nmap_union = fun nmap1 nmap2 -> 
  NameMap.union (fun _ v _ -> Some v) nmap1 nmap2 
let nmap_map = NameMap.map 
let nmap_add = NameMap.add
let nmap_find = NameMap.find
let nmap_find_opt = NameMap.find_opt
let nmap_filter = fun f_opt -> NameMap.filter_map (fun _ a -> f_opt a)
let nmap_to_list = NameMap.bindings
let nmap_of_list = fun l ->
  List.fold_left
   (fun nmap (n, a) -> 
     NameMap.add n a nmap) 
   NameMap.empty l

let string_of_nmap string_of_empty sep string_of_im nmap = 
  if NameMap.is_empty nmap then 
    string_of_empty
  else 
    NameMap.fold 
     (fun name v acc -> 
       string_of_name name ^ sep ^ string_of_im v ^ ", "
       ^ acc) nmap ""
