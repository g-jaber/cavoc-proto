(* This module provides two functions to compose the interaction between
    two configurations of the same bipartite LTS.
   One of the configuration must be active, and the other one is passive.
*)

module Make
    (Namectx : Lang.Typectx.TYPECTX)
    (IntLTS :
      Strategy.LTS
        with type TypingLTS.Moves.Renaming.Namectx.t = Namectx.t * Namectx.t
         and type TypingLTS.Moves.Renaming.Namectx.Names.name =
          (Namectx.Names.name, Namectx.Names.name) Either.t) : Strategy.LTS =
struct
  include Util.Monad.UserChooseWrite (struct
    type t = IntLTS.TypingLTS.Moves.pol_move

    let show = IntLTS.TypingLTS.Moves.string_of_pol_move
  end)

  module TypingLTS = IntLTS.TypingLTS
  module EvalMonad = IntLTS.EvalMonad



  type[@warning "-37"] active_conf =
    | ActiveLeft of IntLTS.active_conf * IntLTS.passive_conf
    | ActiveRight of IntLTS.passive_conf * IntLTS.active_conf

  type passive_conf = IntLTS.passive_conf * IntLTS.passive_conf
    [@@deriving to_yojson]

  type conf = Active of active_conf | Passive of passive_conf

  let string_of_active_conf : active_conf -> string = failwith "TODO"
  let string_of_passive_conf : passive_conf -> string = failwith "TODO"
  let pp_active_conf : Format.formatter -> active_conf -> unit = failwith "TODO"

  let pp_passive_conf : Format.formatter -> passive_conf -> unit =
    failwith "TODO"

  let equiv_act_conf : active_conf -> active_conf -> bool = failwith "TODO"

  let rec p_trans :
      active_conf -> (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.m =
    function
    | ActiveLeft (actconfL, pasconfR) ->
        let open EvalMonad in
        let* (((dir, (nn, copat)) as move), pasconfL) =
          IntLTS.p_trans actconfL in
        begin
          match nn with
          | Either.Left _ -> return (move, (pasconfL, pasconfR))
          | Either.Right nn' -> begin
              match IntLTS.o_trans pasconfR (dir, (Either.Left nn', copat)) with
              | None -> failwith "wrong move. Please report."
              | Some actconfR -> p_trans (ActiveRight (pasconfL, actconfR))
            end
        end
    | ActiveRight (pasconfL, actconfR) ->
        let open EvalMonad in
        let* (((dir, (nn, copat)) as move), pasconfR) =
          IntLTS.p_trans actconfR in
        begin
          match nn with
          | Either.Right _ -> return (move, (pasconfL, pasconfR))
          | Either.Left nn' -> begin
              match
                IntLTS.o_trans pasconfR (dir, (Either.Right nn', copat))
              with
              | None -> failwith "wrong move. Please report."
              | Some actconfL -> p_trans (ActiveLeft (actconfL, pasconfR))
            end
        end

  let o_trans (pasconfL, pasconfR) ((_dir, (nn, _copat)) as move) :
      active_conf option =
    match nn with
    | Either.Left _nn' -> begin
        match IntLTS.o_trans pasconfL move with
        | None -> None
        | Some actconfL -> Some (ActiveLeft (actconfL, pasconfR))
      end
    | Either.Right _nn' -> begin
        match IntLTS.o_trans pasconfR move with
        | None -> None
        | Some actconfR -> Some (ActiveRight (pasconfL, actconfR))
      end

  let o_trans_gen (pasconfL, pasconfR) :
      (TypingLTS.Moves.pol_move * active_conf) TypingLTS.BranchMonad.m =
    let open TypingLTS.BranchMonad in
    para_pair
      (let* (move, actconfL) = IntLTS.o_trans_gen pasconfL in
       return (move, ActiveLeft (actconfL, pasconfR)))
      (let* (move, actconfR) = IntLTS.o_trans_gen pasconfR in
       return (move, ActiveRight (pasconfL, actconfR)))
end
