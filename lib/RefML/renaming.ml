module FRenaming : Lang.Renaming.RENAMING with module Namectx = Namectx.FNamectx =
  Lang.Renaming.Make (Namectx.FNamectx)

module PRenamingP : Lang.Renaming.RENAMING with module Namectx = Namectx.PNamectxP =
  Lang.Renaming.Make (Namectx.PNamectxP)

module PRenamingO : Lang.Renaming.RENAMING with module Namectx = Namectx.PNamectxO =
  Lang.Renaming.Make (Namectx.PNamectxO)

module PRenaming = Lang.Renaming.Aggregate (PRenamingP) (PRenamingO) (Namectx.PNamectx)

module Renaming = Lang.Renaming.Aggregate (FRenaming) (PRenaming) (Namectx.Namectx)