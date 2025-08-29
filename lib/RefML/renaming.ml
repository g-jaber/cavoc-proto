module FRenaming : Lang.Renaming.RENAMING with module Namectx = Namectx.FNamectx =
  Lang.Renaming.Make (Namectx.FNamectx)

module PRenaming : Lang.Renaming.RENAMING with module Namectx = Namectx.PNamectx =
  Lang.Renaming.Make (Namectx.PNamectx)

module Renaming = Lang.Renaming.Aggregate (FRenaming) (PRenaming) (Namectx.Namectx)