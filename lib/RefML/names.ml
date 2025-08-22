(* We consider three kind of names:
   - Function names
   - Continuation names
   - Polymorphic names
*)

type name = FName of string | CName of string | PName of string

let is_callable = function FName _ | CName _ -> true | _ -> false
let is_cname = function CName _ -> true | _ -> false

let trim_name_id id =
  if id.[0] = '_' then String.sub id 1 (String.length id - 1)
  else failwith @@ "The id " ^ id ^ "does not start with _. It is not a name."

let from_string id = FName id
let count_fname = ref 0

let fresh_name () =
  let fn = !count_fname in
  count_fname := !count_fname + 1;
  FName ("f" ^ string_of_int fn)
let count_pname = ref 0

let fresh_pname () =
  let pn = !count_pname in
  count_pname := !count_pname + 1;
  PName ("p" ^ string_of_int pn)

let string_of_name = function FName f -> f | CName c -> c | PName p -> p

let pp_name fmt = function
  | FName id | CName id | PName id -> Format.fprintf fmt "%s" id

