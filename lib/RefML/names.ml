(* We consider two kind of names:
   - Function names
   - Polymorphic names
*)

module FNames : Lang.Names.NAMES with type name = int * string =
  Lang.Names.MakeInt (struct
      let is_callable = true let is_cname = false
    end)
    (struct
      let prefix = "f"
    end)
    ()

module PNames =
  Lang.Names.MakeInt (struct
      let is_callable = false let is_cname = false
    end)
    (struct
      let prefix = "p"
    end)
    ()

type name = (FNames.name, PNames.name) Either.t

let name_to_yojson = function
  | Either.Left fn -> FNames.name_to_yojson fn
  | Either.Right pn -> PNames.name_to_yojson pn

let embed_fname fn = Either.Left fn
let embed_pname pn = Either.Right pn

let string_of_name = function
  | Either.Left f -> FNames.string_of_name f
  | Either.Right p -> PNames.string_of_name p

let pp_name fmt = function
  | Either.Left fn -> FNames.pp_name fmt fn
  | Either.Right pn -> PNames.pp_name fmt pn

let is_callable = function Either.Left _ -> true | Either.Right _ -> false
let is_cname _ = false

let trim_name_id id =
  if id.[0] = '_' then String.sub id 1 (String.length id - 1)
  else failwith @@ "The id " ^ id ^ "does not start with _. It is not a name."
