type oplang = RefML
type control_structure = DirectStyle | CPS
type restriction = Visibility | WellBracketing
type kind_lts = {
  oplang : oplang;
  control : control_structure;
  restrictions : restriction list;
}
val build_lts : kind_lts -> (module Lts.Strategy.LTS_WITH_INIT)
