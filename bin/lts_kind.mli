type oplang = RefML
type control_structure = DirectStyle (* with stack of evaluation contexts *) | CPS (* with continuation names*)
type restriction = Visibility | WellBracketing
type kind_lts = {
  oplang : oplang;
  control : control_structure;
  restrictions : restriction list;
} [@@deriving yojson]
val build_lts : kind_lts -> (module Lts.Strategy.LTS_WITH_INIT)
