module type INT = sig
  module ContNames : Lang.Names.CONT_NAMES include Interactive.INT
end

module Int_Make (CpsLang : Lang.Cps.INTLANG) :
  INT
    with type IntLang.name = CpsLang.name
     and type ContNames.name = CpsLang.name
     and type Actions.Moves.name = CpsLang.name = struct

  module ContNames = struct
    type name = CpsLang.name
    type cont_name = CpsLang.cont_name

    let string_of_name = CpsLang.string_of_name
    let is_callable = CpsLang.is_callable
    let inj_cont_name = CpsLang.inj_cont_name
    let get_cont_name = CpsLang.get_cont_name
    let string_of_cont_name = CpsLang.string_of_cont_name
    let fresh_cname = CpsLang.fresh_cname
  end
include Interactive.Make(CpsLang)
end
