module NegType = struct
  type t = Types.negative_type

  let to_yojson = Types.negative_type_to_yojson
  let pp = Types.pp_negative_type
end

module FNamectx = Lang.Typectx.Make_List (Names.FNames) (NegType)
module PNamectxP = Lang.Typectx.Make_List (Names.PNamesP) (NegType)
module PNamectxO = Lang.Typectx.Make_List (Names.PNamesO) (NegType)

module PNamectx =
  Lang.Typectx.AggregateCommon (PNamectxP) (PNamectxO) (Names.PNames)
    (struct
      let embed1 pn = Either.Left pn
      let embed2 pn = Either.Right pn

      let extract1 = function
        | Either.Left pn -> Some pn
        | Either.Right _ -> None

      let extract2 = function
        | Either.Left _ -> None
        | Either.Right pn -> Some pn
    end)
    (struct
      let classify = function
        | Types.TId _ -> true
        | Types.TName _ -> false
        | _ -> failwith "Trying to add a name of the wrong type. Please report"
    end)

module Namectx =
  Lang.Typectx.AggregateCommon (FNamectx) (PNamectx) (Names)
    (struct
      let embed1 fn = Names.embed_fname fn
      let embed2 pn = Either.Right pn

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
