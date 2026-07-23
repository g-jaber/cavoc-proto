module FRenaming : Lang.Renaming.WEAKENING with module Namectx = Namectx.FNamectx =
  Lang.Renaming.MakeWeak (Namectx.FNamectx)

module PRenamingP : Lang.Renaming.WEAKENING with module Namectx = Namectx.PNamectxP =
  Lang.Renaming.MakeWeak (Namectx.PNamectxP)

module PRenamingO : Lang.Renaming.WEAKENING with module Namectx = Namectx.PNamectxO =
  Lang.Renaming.MakeWeak (Namectx.PNamectxO)

module PRenaming = Lang.Renaming.AggregateWeak (PRenamingP) (PRenamingO) (Namectx.PNamectx)

module Renaming = Lang.Renaming.AggregateWeak (FRenaming) (PRenaming) (Namectx.Namectx)