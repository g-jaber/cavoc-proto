module FNamectx =
  Lang.Typectx.Make_PMAP
    (Names.FNames)
    (struct
      type t = Types.negative_type

      let to_yojson = Types.negative_type_to_yojson
      let pp = Types.pp_negative_type
    end)

module PNamectx =
  Lang.Typectx.Make_PMAP
    (Names.PNames)
    (struct
      type t = Types.negative_type

      let to_yojson = Types.negative_type_to_yojson
      let pp = Types.pp_negative_type
    end)

module Namectx =
  Lang.Typectx.AggregateCommon (FNamectx) (PNamectx)
    (struct
      type t = Names.name

      let embed1 fn = Names.FName fn
      let embed2 pn = Names.PName pn

      let extract1 = function
        | Names.FName fn -> Some fn
        | Names.PName _ -> None

      let extract2 = function
        | Names.FName _ -> None
        | Names.PName pn -> Some pn
    end)
    (struct
      let classify = function
        | Types.TArrow _ | Types.TForall _ -> true
        | Types.TId _ | Types.TName _ -> false
        | _ -> failwith "Trying to add a name of the wrong type. Please report"
    end)