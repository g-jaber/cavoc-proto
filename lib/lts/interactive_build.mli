(**
  The [Interactive_build] module contains the main interaction loop of the
  prototype.
 *)

(**
  What the user did when the interaction loop asked them to act.

  The user may always leave the game rather than answer, so every callback that
  consults the user returns a [user_action]. Quitting is thus part of the
  interface of the loop.
 *)
type 'a user_action =
  | Chose of 'a  (** The user answered the question that was asked. *)
  | Quit  (** The user left the game, and the interaction must stop. *)

(** Why the interaction loop stopped. *)
type outcome =
  | Prop_stopped  (** Proponent (i.e. the module) has stopped playing. *)
  | Prop_diverges  (** The program diverges. *)
  | User_quit  (** Opponent (i.e. the user) left the game. *)

(** A description of [outcome], to be shown to the user. *)
val string_of_outcome : outcome -> string

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
    @param get_move returns the move choosen by the user, or {!constructor:Quit}
    if they chose to leave the game instead.

    @returns Why the interaction stopped
   *)
  val interactive_build :
    show_move:(string -> unit) ->
    show_conf:(Yojson.Safe.t -> unit) ->
    show_moves_list:(Yojson.Safe.t list -> unit) ->
    (* the argument of get_move is the
    number of moves *)
    get_move:(int -> int user_action M.m) ->
    conf ->
    outcome M.m
end

(**
  [RUN_LTS] is used by {!module: Make} to signify that it accepts any
  {{!module-type: Strategy.LTS}LTS}, provided that the LTS is capable of resolving
  non-determinism caused by an interpreter returning multiple
  {{!type: Strategy.LTS.passive_conf}passive configurations}.

  Since resolving non-determinism may require some action of the user, the
  monad {!module: RUN_LTS.M} is required. For the same reason [choose] returns
  a {!type: user_action}: asking the user which configuration to keep is
  another point at which they may leave the game.

  See {!val: Strategy.LTS.p_trans}
  See {!module: IBUILD.M}
 *)
module type RUN_LTS = sig
  include Strategy.LTS

  module M : Util.Monad.MONAD

  val choose :
    (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.m ->
    (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.result user_action M.m
end

module Make : functor (M : Util.Monad.MONAD) (IntLTS : RUN_LTS with module M = M) ->
  IBUILD with module M = M and type conf = IntLTS.conf
