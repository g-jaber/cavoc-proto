module type LTS = sig
  (* The following field is to be instantiated *)
  module TypingLTS : Typing.LTS
  module EvalMonad : Util.Monad.RUNNABLE

  type active_conf
  type passive_conf [@@deriving to_yojson]
  type conf = Active of active_conf | Passive of passive_conf

  val string_of_active_conf : active_conf -> string
  val string_of_passive_conf : passive_conf -> string
  val pp_active_conf : Format.formatter -> active_conf -> unit
  val pp_passive_conf : Format.formatter -> passive_conf -> unit
  val equiv_act_conf : active_conf -> active_conf -> bool
  val p_trans : active_conf -> (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.m
  val o_trans : passive_conf -> TypingLTS.Moves.pol_move -> active_conf option

  val o_trans_gen :
    passive_conf -> (TypingLTS.Moves.pol_move * active_conf) TypingLTS.BranchMonad.m
end


module type LTS_WITH_INIT = sig
  include LTS
  val lexing_init_aconf : Lexing.lexbuf -> active_conf
  val lexing_init_pconf : Lexing.lexbuf -> Lexing.lexbuf -> passive_conf
end

module type LTS_WITH_INIT_BIN = sig
  include LTS
  val lexing_init_aconf : Lexing.lexbuf -> Lexing.lexbuf -> active_conf
  val lexing_init_pconf : Lexing.lexbuf -> Lexing.lexbuf -> Lexing.lexbuf -> passive_conf
end

