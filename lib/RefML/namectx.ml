module NegType = struct
  type t = Types.negative_type

  let to_yojson = Types.negative_type_to_yojson
  let pp = Types.pp_negative_type
end

module TNamectx = Lang.Typectx.Make_PMAP (Names.TNames) (NegType)
module FNamectx = Lang.Typectx.Make_List (Names.FNames) (NegType)
module PNamectx = Lang.Typectx.Make_List (Names.PNames) (NegType)

(* Δ: the function and polymorphic names, typed over the type names. *)
module ValueNamectx =
  Lang.Typectx.AggregateCommon (FNamectx) (PNamectx) (Names.ValueNames)
    (struct
      let embed1 fn = Either.Left fn
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
        | Types.TName _ -> false
        | _ -> failwith "Trying to add a name of the wrong type. Please report"
    end)

(* A typing context is Θ ⊢ Δ: the type names, then the names typed over them. *)
module Namectx =
  Lang.Typectx.AggregateCommon (TNamectx) (ValueNamectx) (Names)
    (struct
      let embed1 tn = Names.embed_tname tn
      let embed2 nn = Either.Right nn

      let extract1 = function
        | Either.Left tn -> Some tn
        | Either.Right _ -> None

      let extract2 = function
        | Either.Left _ -> None
        | Either.Right nn -> Some nn
    end)
    (struct
      let classify = function
        | Types.TypeUniverse -> true
        | Types.TArrow _ | Types.TForall _ | Types.TName _ -> false
        | _ -> failwith "Trying to add a name of the wrong type. Please report"
    end)

let has_type_name ((tnamectx, _) : Namectx.t) tn =
  List.mem tn (TNamectx.get_names tnamectx)
