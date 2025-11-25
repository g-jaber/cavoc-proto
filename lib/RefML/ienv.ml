module IEnvF =
  Lang.Ienv.Make_List
    (Renaming.FRenaming)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let embed_name nn =
        Syntax.force_negative_val (Syntax.Name (Names.embed_fname nn))

      let renam_act frenaming =
        Syntax.rename_negative_val
          (frenaming, Renaming.PRenaming.id Renaming.PRenaming.Namectx.empty)

      let pp = Syntax.pp_negative_val
    end)

module IEnvPolP =
  Lang.Ienv.Make_List
    (Renaming.PRenamingP)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let embed_name nn =
        Syntax.force_negative_val (Syntax.Name (Names.embed_pnameP nn))

      let renam_act prenaming =
        Syntax.rename_negative_val
          ( Renaming.FRenaming.id Renaming.FRenaming.Namectx.empty,
            (prenaming, Renaming.PRenamingO.id Renaming.PRenamingO.Namectx.empty)
          )

      let pp = Syntax.pp_negative_val
    end)

module IEnvPolO =
  Lang.Ienv.Make_List
    (Renaming.PRenamingO)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let embed_name nn =
        Syntax.force_negative_val (Syntax.Name (Names.embed_pnameO nn))

      let renam_act prenaming =
        Syntax.rename_negative_val
          ( Renaming.FRenaming.id Renaming.FRenaming.Namectx.empty,
            (Renaming.PRenamingP.id Renaming.PRenamingP.Namectx.empty, prenaming)
          )

      let pp = Syntax.pp_negative_val
    end)

module IEnvP =
  Lang.Ienv.AggregateCommon (IEnvPolP) (IEnvPolO) (Renaming.PRenaming)
    (struct
      let embed1 pn = Either.left pn
      let embed2 pn = Either.right pn

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
        | ty ->
            failwith @@ "Trying to add a polymorphic name of the wrong type. "
            ^ Types.string_of_typ ty ^ " Please report"
    end)

module IEnv =
  Lang.Ienv.AggregateCommon (IEnvF) (IEnvP) (Renaming.Renaming)
    (struct
      let embed1 fn = Names.embed_fname fn
      let embed2 pn = Either.right pn

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
