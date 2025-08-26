module IEnvF =
  Lang.Ienv.Make_PMAP
    (Names.FNames)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let pp = Syntax.pp_negative_val
    end)

module IEnvP =
  Lang.Ienv.Make_PMAP
    (Names.PNames)
    (struct
      type t = Syntax.negative_val [@@deriving to_yojson]

      let pp = Syntax.pp_negative_val
    end)

module IEnv =
  Lang.Ienv.AggregateCommon (IEnvF) (IEnvP)
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
