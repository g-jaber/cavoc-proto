module IEnvT =
  Lang.Ienv.Make_PMAP
    (Renaming.TRenaming)
    (struct
      (* Below is a hack to be able to later aggregate the type name context with the other name context*)
      type t = Syntax.negative_val [@@deriving to_yojson]

      let embed_name tn =
        Syntax.force_negative_val (Syntax.Name (Names.embed_tname tn))

      let renam_act trenaming =
        Syntax.rename_negative_val
          ( trenaming,
            Renaming.ValueRenaming.id Namectx.ValueNamectx.empty )

      let pp = Syntax.pp_negative_val
    end)

module IEnvF =
  Lang.Ienv.Make_List
    (Renaming.FRenaming)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let embed_name nn =
        Syntax.force_negative_val (Syntax.Name (Names.embed_fname nn))

      let renam_act frenaming =
        Syntax.rename_negative_val
          ( Renaming.TRenaming.id Namectx.TNamectx.empty,
            (frenaming, Renaming.PRenaming.id Namectx.PNamectx.empty) )

      let pp = Syntax.pp_negative_val
    end)

module IEnvP =
  Lang.Ienv.Make_List
    (Renaming.PRenaming)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let embed_name nn =
        Syntax.force_negative_val (Syntax.Name (Names.embed_pname nn))

      let renam_act prenaming =
        Syntax.rename_negative_val
          ( Renaming.TRenaming.id Namectx.TNamectx.empty,
            (Renaming.FRenaming.id Namectx.FNamectx.empty, prenaming) )

      let pp = Syntax.pp_negative_val
    end)

module IEnvValue =
  Lang.Ienv.AggregateCommon (IEnvF) (IEnvP) (Renaming.ValueRenaming)
    (struct
      let embed1 fn = Either.left fn
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
        | Types.TName _ -> false
        | ty ->
            failwith @@ "Trying to add a name of the wrong type. "
            ^ Types.string_of_typ ty ^ " Please report"
    end)

module IEnv =
  Lang.Ienv.AggregateCommon (IEnvT) (IEnvValue) (Renaming.Renaming)
    (struct
      let embed1 tn = Names.embed_tname tn
      let embed2 nn = Either.right nn

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
        | ty ->
            failwith @@ "Trying to add a name of the wrong type. "
            ^ Types.string_of_typ ty ^ " Please report"
    end)
