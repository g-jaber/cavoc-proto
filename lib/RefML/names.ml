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

let pp_name fmt = function
  | FName id | CName id | PName id -> Format.fprintf fmt "%s" id

let cname_of_id id = CName id
let cname_to_id = function CName cn -> Some cn | _ -> None

type cont_name = id

let string_of_cont_name cn = cn
let get_cont_name = cname_to_id
let inj_cont_name = cname_of_id

let is_callable = function
  | FName _ -> true
  | CName _ -> true
  | PName _ -> false
