(* We consider two kind of names:
   - Function names
   - Polymorphic names
*)

module FNames =
  Lang.Names.MakeGen (struct
      let is_callable = true let is_cname = false
    end)
    (struct
      let prefix = "f"
    end)
    ()

module PNames =
  Lang.Names.MakeGen (struct
      let is_callable = false let is_cname = false
    end)
    (struct
      let prefix = "p"
    end)
    ()

type name = FName of FNames.name | PName of PNames.name [@@deriving to_yojson]

let string_of_name = function
  | FName f -> FNames.string_of_name f
  | PName p -> PNames.string_of_name p

let pp_name fmt = function
  | FName fn -> FNames.pp_name fmt fn
  | PName pn -> PNames.pp_name fmt pn

let is_callable = function FName _ -> true | PName _ -> false
let is_cname _ = false

let trim_name_id id =
  if id.[0] = '_' then String.sub id 1 (String.length id - 1)
  else failwith @@ "The id " ^ id ^ "does not start with _. It is not a name."

