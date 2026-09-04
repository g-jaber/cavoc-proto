(* Three kinds of names: type names, function names and polymorphic names. *)

module FNames : Lang.Names.NAMES with type name = int =
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

module TNames : Lang.Names.NAMES_GEN with type name = Types.id = struct
  type name = Types.id

  let name_to_yojson id = `String id
  let string_of_name id = id
  let pp_name = Format.pp_print_string
  let is_callable _ = false
  let is_cname _ = false
  let fresh_name = Types.fresh_typename
  let from_string id = id
end

module ValueNames = Lang.Names.MakeAggregate (FNames) (PNames)
include Lang.Names.MakeAggregate (TNames) (ValueNames)

let embed_tname tn = Either.Left tn
let embed_fname fn = Either.Right (Either.Left fn)
let embed_pname pn = Either.Right (Either.Right pn)

let trim_name_id id =
  if id.[0] = '_' then String.sub id 1 (String.length id - 1)
  else failwith @@ "The id " ^ id ^ "does not start with _. It is not a name."
