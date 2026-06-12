(**
  The [Interactive_build] module contains the main interaction loop of the
  prototype.
 *)

module type IBUILD = sig
  (**
    [M] is the monad used to interact with the user. The backend does not
    impose any restriction on this monad, it is only here because it could
    be needed by some of the callbacks passed to {!val: interactive_build}. 
   *)
  module M : Util.Monad.MONAD
  
  type conf

  (**
    [interactive_build] starts the interaction loop. The environment is
    expected to provide callbacks to be called at different stages of the
    interaction.

    @param show_move display a textual representation of the move after each turn.
    @param show_conf display the current configuration after each turn
    @param show_moves_list display the list of possibles moves when it's the user's
    turn.
    @param get_move returns the move choosen by the user

    @returns The reason why the interaction stopped
   *)
  val interactive_build :
    show_move:(string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the 
    number of moves *)
    get_move:(int -> int M.m) ->
    conf ->
    string M.m
end

(**
  [RUN_LTS] is used by {!module: Make} to signify that it accepts any
  {{!module-type: Strategy.LTS}LTS}, provided that the LTS is capable of resolving
  non-determinism caused by an interpreter returning multiple
  {{!type: Strategy.LTS.passive_conf}passive configurations}.

  Since resolving non-determinism may require some action of the user, the
  monad {!module: RUN_LTS.M} is required.

  See {!val: Strategy.LTS.p_trans}
  See {!module: IBUILD.M}
 *)
module type RUN_LTS = sig
  include Strategy.LTS

  module M : Util.Monad.MONAD

  val choose : (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.m -> (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.result M.m
end

module Make : functor (M : Util.Monad.MONAD) (IntLTS : RUN_LTS with module M = M) ->
  IBUILD with module M = M and type conf = IntLTS.conf
