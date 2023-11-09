module type INT = sig
  module ContNames : Lang.Cps.CONT_NAMES include Interactive.INT
end

module Int_Make : functor (CpsLang : Lang.Cps.LANG) ->
  INT
    with type OpLang.name = CpsLang.name
     and type ContNames.name = CpsLang.name
     and type Actions.Moves.name = CpsLang.name
