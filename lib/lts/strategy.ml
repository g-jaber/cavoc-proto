(**
   The [Strategy] module contains the various signatures implemented by the
   LTSs representing interaction.
 *)

module type LTS = sig
  (* The following field is to be instantiated *)
  module TypingLTS : Typing.LTS
  module EvalMonad : Util.Monad.RUNNABLE

  (**
    There are two types of configurations.
    - {!type: passive_conf} is obtained after the propnent has played ;
    - {!type: active_conf} is obtained after the opponent has played.
   *)

  type active_conf
  type passive_conf [@@deriving to_yojson]
  type conf = Active of active_conf | Passive of passive_conf

  val string_of_active_conf : active_conf -> string
  val string_of_passive_conf : passive_conf -> string
  val pp_active_conf : Format.formatter -> active_conf -> unit
  val pp_passive_conf : Format.formatter -> passive_conf -> unit
  val equiv_act_conf : active_conf -> active_conf -> bool
  val get_active_pos : active_conf -> TypingLTS.position
  val get_passive_pos : passive_conf -> TypingLTS.position

  (**
    [p_trans] is used to generate the next configuration from a given
    {!type: active_conf}. Depending on the type {!type: EvalMonad.r},
    [p_trans] may produce more than one configuration.
   *)
  val p_trans : active_conf -> (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.m
  val o_trans : passive_conf -> TypingLTS.Moves.pol_move -> active_conf option

  (**
    [o_trans_gen] is used to generate all possible moves from a given
    {!type: passive_conf}
   *)
  val o_trans_gen :
    passive_conf -> (TypingLTS.Moves.pol_move * active_conf) TypingLTS.BranchMonad.m
end

(**
  [LTS_WITH_INIT] is a {!module-type: LTS} capable of parsing its
  initial configuration from lexing buffers.
 *)
module type LTS_WITH_INIT = sig
  include LTS
  val lexing_init_aconf : Lexing.lexbuf -> active_conf

  (**
    [lexing_init_pconf] takes two lexing buffers, respectively containing the
    implementation and the signature of a module, and return an initial
    {{!type: Lts.Strategy.LTS.passive_conf}passive configuration}

    [opponent_signature], when provided, is the signature of names the module may use but
    does not implement: they become Opponent names of the configuration.
   *)
  val lexing_init_pconf :
    ?opponent_signature:Lexing.lexbuf -> Lexing.lexbuf -> Lexing.lexbuf -> passive_conf
end

module type LTS_WITH_INIT_BIN = sig
  include LTS
  val lexing_init_aconf : Lexing.lexbuf -> Lexing.lexbuf -> active_conf
  val lexing_init_pconf : Lexing.lexbuf -> Lexing.lexbuf -> Lexing.lexbuf -> passive_conf
end
