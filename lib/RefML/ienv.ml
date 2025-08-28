module IEnvF =
  Lang.Ienv.Make_List
    (Namectx.FNamectx)
    (*Names.FNames*)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let pp = Syntax.pp_negative_val
    end)

module IEnvP =
  Lang.Ienv.Make_List
    (Namectx.PNamectx)
    (*Names.PNames*)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let pp = Syntax.pp_negative_val
    end)

module IEnv =
  Lang.Ienv.AggregateCommon (IEnvF) (IEnvP)
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
