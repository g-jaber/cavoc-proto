type oplang = RefML
type control_structure = DirectStyle (* with stack of evaluation contexts *) | CPS (* with continuation names*)
type restriction = Visibility | WellBracketing
type kind_lts = {
  oplang : oplang;
  symbolic: bool;
  control : control_structure;
  restrictions : restriction list;
} [@@deriving yojson]

module type SINGLE_RESULT_LTS_WITH_INIT = Lts.Strategy.LTS_WITH_INIT with type 'a EvalMonad.r = 'a
module type MULTI_RESULT_LTS_WITH_INIT  = Lts.Strategy.LTS_WITH_INIT with type 'a EvalMonad.r = 'a list

module type SINGLE_RESULT_COMPOSITION_WITH_INIT =
  Ogs.Compose_lts.COMPOSITION_WITH_INIT with type 'a EvalMonad.r = 'a

val build_concrete_lts : kind_lts -> (module SINGLE_RESULT_LTS_WITH_INIT)

val build_symbolic_lts : kind_lts -> (module MULTI_RESULT_LTS_WITH_INIT)

val build_compose_lts : kind_lts -> (module SINGLE_RESULT_COMPOSITION_WITH_INIT)
