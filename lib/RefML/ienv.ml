module IEnvF =
  Lang.Ienv.Make_List
    (Renaming.FRenaming)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let embed_name nn =
        Syntax.force_negative_val (Syntax.Name (Names.embed_fname nn))

      let pp = Syntax.pp_negative_val
    end)

module IEnvP =
  Lang.Ienv.Make_List
    (Renaming.PRenaming)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let embed_name nn =
        Syntax.force_negative_val (Syntax.Name (Names.embed_pname nn))

      let pp = Syntax.pp_negative_val
    end)

module IEnv =
  Lang.Ienv.AggregateCommon (IEnvF) (IEnvP) (Renaming.Renaming)
    (struct
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
