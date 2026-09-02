(* The definability of an extra memory into the store of the programming language. *)
module type REIFICATION = sig
  type move
  type name
  type pattern
  type state
  type oplang_store
  type oplang_term
  type oplang_pattern
  type oplang_value

  (* Evaluating an occurrence's reified advance against [reify_state s]
     yields [reify_state (advance o s)]. *)
  (* Per-occurrence calls receive the recording's final state, whose knowledge
     extends every occurrence's. *)
  val reify_state : state -> oplang_store

  val reify_advance :
    move -> pattern -> (name -> oplang_value) -> state -> oplang_term

  val reify_pattern : pattern -> oplang_pattern

  (* Wrap a branch's body with whatever binds the provided levels its Player
     move reads, giving the body the value of each. *)
  val reify_reads :
    name list -> state -> ((name -> oplang_value) -> oplang_term) -> oplang_term

  (* The pre-play declarations of a recording ending in this state: every cell
     the state tracks, with its initial value. *)
  val reify_store_declarations : state -> oplang_store

  (* The definability of [guard_of_state]: a term whose value against
     [reify_state s] is matched by [reify_pattern (guard_of_state s)]. *)
  val reify_state_reading : state -> oplang_term option
end
