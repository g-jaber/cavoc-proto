module type INT = sig
  module ContNames : Lang.Names.CONT_NAMES include Interactive.INT
end

module Int_Make (CpsLang : Lang.Cps.INTLANG) :
  INT
    with type IntLang.name = CpsLang.name
     and type ContNames.name = CpsLang.name
     and type Actions.Moves.name = CpsLang.name
