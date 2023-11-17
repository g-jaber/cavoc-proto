module type WITHNUP = sig
  include Language.WITHNUP include Names.CONT_NAMES with type name := name
end

module Make : functor (OpLang : WITHNUP) ->
  Interactive.LANG with type name = OpLang.name

module type INTLANG = sig
  include Interactive.LANG
  include  Names.CONT_NAMES with type name := name
end
