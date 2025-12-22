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

module PNamesP =
  Lang.Names.MakeInt (struct
      let is_callable = false let is_cname = false
    end)
    (struct
      let prefix = "p"
    end)
    ()

module PNamesO =
  Lang.Names.MakeInt (struct
      let is_callable = false let is_cname = false
    end)
    (struct
      let prefix = "p"
    end)
    ()

module PNames = Lang.Names.MakeAggregate (PNamesP) (PNamesO)

include Lang.Names.MakeAggregate (FNames) (PNames)

let embed_fname fn = Either.Left fn
let embed_pname pn = Either.Right pn
let embed_pnameP pn = Either.Right (Either.Left pn)
let embed_pnameO pn = Either.Right (Either.Right pn)

let trim_name_id id =
  if id.[0] = '_' then String.sub id 1 (String.length id - 1)
  else failwith @@ "The id " ^ id ^ "does not start with _. It is not a name."
