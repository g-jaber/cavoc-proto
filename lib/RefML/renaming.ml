module FRenaming =
  Lang.Renaming.MakeInjectiveRenaming
    (Lang.Renaming.MakeDeBruijnWeakening (Namectx.FNamectx))

module PRenamingP =
  Lang.Renaming.MakeInjectiveRenaming
    (Lang.Renaming.MakeDeBruijnWeakening (Namectx.PNamectxP))

module PRenamingO =
  Lang.Renaming.MakeInjectiveRenaming
    (Lang.Renaming.MakeDeBruijnWeakening (Namectx.PNamectxO))

module PRenaming =
  Lang.Renaming.AggregateInjectiveRenaming (PRenamingP) (PRenamingO)
    (Namectx.PNamectx)

module Renaming =
  Lang.Renaming.AggregateInjectiveRenaming (FRenaming) (PRenaming)
    (Namectx.Namectx)
