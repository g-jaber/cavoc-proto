type oplang = RefML

type control_structure =
  | DirectStyle
  (* with stack of evaluation contexts *)
  | CPS (* with continuation names*)

type restriction = Visibility | WellBracketing

type kind_lts = {
  oplang: oplang;
  symbolic: bool;
  control: control_structure;
  restrictions: restriction list;
}
[@@deriving yojson]

module type SINGLE_RESULT_LTS_WITH_INIT =
  Lts.Strategy.LTS_WITH_INIT with type 'a EvalMonad.r = 'a

module type MULTI_RESULT_LTS_WITH_INIT =
  Lts.Strategy.LTS_WITH_INIT with type 'a EvalMonad.r = 'a list

module type SINGLE_RESULT_COMPOSITION_WITH_INIT =
  Ogs.Compose_lts.COMPOSITION_WITH_INIT with type 'a EvalMonad.r = 'a

module type SINGLE_RESULT_LTS_WITH_CLIENT = sig
  include SINGLE_RESULT_LTS_WITH_INIT

  (* The source of the client whose play is the recorded one, None until the
     module has been played against; absent where no synthesis exists. *)
  val synthesize_client_source :
    (TypingLTS.position -> TypingLTS.Moves.pol_move list -> string option)
    option
end

module type SINGLE_RESULT_ARENA = sig
  module TypingLTS : Lts.Typing.LTS

  (* The arena a signature declares, with no implementation on either
     side. *)
  val initial_position : Lexing.lexbuf -> TypingLTS.position

  (* The moves offered at a position: the well-typed ones the play so far
     still leaves definable. *)
  (* The prototype does not consume answered continuations, so moves answering
     them are still offered though neither participant can implement them. *)
  val offered_moves :
    arena:TypingLTS.position ->
    TypingLTS.position ->
    TypingLTS.Moves.pol_move list ->
    (TypingLTS.Moves.pol_move * TypingLTS.position) list

  (* The two participants the play defines, the moves chronological and in the
     module's own polarity; None until that side has been played. *)
  val synthesize_module_source :
    TypingLTS.position -> TypingLTS.Moves.pol_move list -> string option

  val synthesize_client_source :
    TypingLTS.position -> TypingLTS.Moves.pol_move list -> string option
end

val build_concrete_lts : kind_lts -> (module SINGLE_RESULT_LTS_WITH_CLIENT)

(* The arena of a signature-only page: visibility and well-bracketing are
   forced, so that every move offered keeps the play definable. *)
val build_arena : unit -> (module SINGLE_RESULT_ARENA)
val build_symbolic_lts : kind_lts -> (module MULTI_RESULT_LTS_WITH_INIT)
val build_compose_lts : kind_lts -> (module SINGLE_RESULT_COMPOSITION_WITH_INIT)
