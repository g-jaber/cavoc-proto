module FNamectx =
  Lang.Typectx.Make_List
    (*Names.FNames*)
    (struct
      type t = Types.negative_type

      let to_yojson = Types.negative_type_to_yojson
      let pp = Types.pp_negative_type
    end)

module PNamectx =
  Lang.Typectx.Make_List
    (*Names.PNames*)
    (struct
      type t = Types.negative_type

      let to_yojson = Types.negative_type_to_yojson
      let pp = Types.pp_negative_type
    end)

module Namectx =
  Lang.Typectx.AggregateCommon (FNamectx) (PNamectx)
    (struct
      type t = Names.name

      let embed1 fn = Names.embed_fname fn
      let embed2 pn = Names.embed_pname pn

      let extract1 = function
        | Either.Left fn -> Some fn
        | Either.Right _ -> None

      let extract2 = function
        | Either.Left _ -> None
        | Either.Right pn -> Some pn
    end)
    (struct
      let classify = function
        | Types.TArrow _ | Types.TForall _ -> true
        | Types.TId _ | Types.TName _ -> false
        | _ -> failwith "Trying to add a name of the wrong type. Please report"
    end)