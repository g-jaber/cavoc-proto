module Make : Interactive.FOCUSING_F

module type INTLANG = sig
  include Interactive.LANG
  include  Names.CONT_NAMES with type name := name
end