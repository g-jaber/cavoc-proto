module TRenaming = Lang.Renaming.MakeGensymRenaming (Namectx.TNamectx)

module FRenaming =
  Lang.Renaming.MakeInjectiveRenaming
    (Lang.Renaming.MakeDeBruijnWeakening (Namectx.FNamectx))

module PRenaming =
  Lang.Renaming.MakeInjectiveRenaming
    (Lang.Renaming.MakeDeBruijnWeakening (Namectx.PNamectx))

module ValueRenaming =
  Lang.Renaming.AggregateInjectiveRenaming (FRenaming) (PRenaming)
    (Namectx.ValueNamectx)

module Renaming =
  Lang.Renaming.AggregateInjectiveRenaming (TRenaming) (ValueRenaming)
    (Namectx.Namectx)
