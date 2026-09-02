(* The language-side operations synthesizing a source program from a
   view-function strategy, over the well-bracketed fragment that discloses no
   location and gives back no polymorphic name. *)

module type OPLANG_OPS = sig
  type name
  type typ
  type value
  type term
  type abstract_val

  (* The language's match patterns; guards arrive already reified. *)
  type source_pattern

  (* The private store of the synthesized program. *)
  type store

  (* The value of a free name comes from the caller — the environment or a
     store read; that of a bound one from the matching functions. *)
  val value_of_abstract_val :
    (name -> value) -> (name -> value) -> abstract_val -> value

  (* A branch of a match: its pattern as an abstract value with the source
     identifier of each bound name, its reified guard if any, and its body
     given the values of the bound names. *)
  type branch = {
    pattern: abstract_val;
    guard: source_pattern option;
    identifier_of_bound_name: name -> string;
    body: (name -> value) -> term;
  }

  (* λx. match x with the branches, x the given identifier: the value
     implementing a Player name. Branches sharing a pattern are discriminated
     by matching their guards against the state reading; an unguarded branch
     is unconditional. *)
  val pattern_matching_abstraction :
    term option -> string -> typ -> branch list -> value

  (* let r = callee argument in match r with the branches, r the given
     identifier; the callee is a free name, given a value like the free names
     of a value. *)
  val pattern_matching_call :
    term option ->
    string ->
    (name -> value) ->
    name ->
    value ->
    branch list ->
    term

  (* The computation performing an answer: evaluate to the value. *)
  val term_of_returned_value : value -> term

  (* t; u *)
  val sequence : term -> term -> term

  (* The store allocated around a client term, one reference per location;
     the identity on an empty store. *)
  val allocate_store : store -> term -> term
end

module type WITHAVAL_INOUT_DEFINABILITY = sig
  include Language.WITHAVAL_INOUT

  module Definability :
    OPLANG_OPS
      with type name = Names.name
       and type typ = typ
       and type value = value
       and type term = term
       and type abstract_val = AVal.abstract_val
end

(* What the generic synthesis consumes: the language's operations over the
   interactive names and types, with the value component of a move. *)
module type INTERACTIVE_OPS = sig
  module Renaming : Renaming.INJECTIVE_RENAMING

  type abstract_normal_form

  include
    OPLANG_OPS
      with type name := Renaming.Namectx.Names.name
       and type typ := Renaming.Namectx.typ

  (* The value component of a fragment move. *)
  val abstract_val_of_a_nf : abstract_normal_form -> abstract_val
end

module type WITHAVAL_NEG_DEFINABILITY = sig
  include Language.WITHAVAL_NEG

  module Definability :
    INTERACTIVE_OPS
      with module Renaming = Renaming
       and type abstract_normal_form =
        (AVal.abstract_val, unit, Names.name, Names.name) Nf.nf_term
        * Store.store
end

(* The CPS glue of Cps.MakeCompBase, equipped with the interactive operations
   of the synthesis. *)
module MakeComp (OpLang : WITHAVAL_INOUT_DEFINABILITY) () :
  WITHAVAL_NEG_DEFINABILITY
    with module EvalMonad = OpLang.EvalMonad
     and type negative_type = (OpLang.negative_type, OpLang.typ) Either.t
     and type Definability.abstract_val = OpLang.AVal.abstract_val
     and type Definability.value = OpLang.value
     and type Definability.term = OpLang.term
     and type Definability.source_pattern = OpLang.Definability.source_pattern
     and type Definability.store = OpLang.Definability.store = struct
  include Cps.MakeCompBase (OpLang) ()

  module Definability = struct
    module Renaming = Renaming

    type abstract_normal_form =
      (AVal.abstract_val, unit, Names.name, Names.name) Nf.nf_term * Store.store

    type abstract_val = OpLang.AVal.abstract_val
    type value = OpLang.value
    type term = OpLang.term
    type source_pattern = OpLang.Definability.source_pattern
    type store = OpLang.Definability.store

    (* A question's value is a pair, an answer's a bare abstract value; a raise
       shares an answer's subject shape, so it is caught here, not at the move
       level. *)
    let abstract_val_of_a_nf (a_nf_term, store) =
      if not (Store.Storectx.is_empty (Store.infer_type_store store)) then
        failwith
          "Definability: a move discloses locations, outside the definable \
           fragment.";
      Nf.case_nf_term
        ~on_call:(fun _ aval () ->
          match aval with
          | APair (oplang_abstract_val, _) -> oplang_abstract_val
          | APack _ ->
              failwith
                "Definability: existential packs are outside the definable \
                 fragment."
          | AVal _ ->
              failwith
                "Definability: a call without a continuation component. Please \
                 report.")
        ~on_return:(fun _ aval ->
          match aval with
          | AVal oplang_abstract_val -> oplang_abstract_val
          | APair _ | APack _ ->
              failwith
                "Definability: a return carrying a continuation component. \
                 Please report.")
        ~on_raise:(fun _ _ ->
          failwith "Definability: raises are outside the definable fragment.")
        ~on_error:(fun _ ->
          failwith "Definability: error moves are never emitted. Please report.")
        a_nf_term

    let value_of_abstract_val value_of_free_name value_of_bound_name
        oplang_abstract_val =
      OpLang.Definability.value_of_abstract_val
        (fun nn -> value_of_free_name (inj_name nn))
        (fun nn -> value_of_bound_name (inj_name nn))
        oplang_abstract_val

    type branch = {
      pattern: abstract_val;
      guard: source_pattern option;
      identifier_of_bound_name: Names.name -> string;
      body: (Names.name -> value) -> term;
    }

    let adapt_branch (branch : branch) : OpLang.Definability.branch =
      {
        pattern= branch.pattern;
        guard= branch.guard;
        identifier_of_bound_name=
          (fun nn -> branch.identifier_of_bound_name (inj_name nn));
        body=
          (fun value_of_bound_name ->
            branch.body (fun aggregate_name ->
                match aggregate_name with
                | Either.Left nn -> value_of_bound_name nn
                | Either.Right _ ->
                    failwith
                      "Definability: a continuation name has no pattern \
                       binder. Please report."));
      }

    let pattern_matching_abstraction state_reading argument_identifier nty
        branches =
      match nty with
      | Either.Left oplang_nty -> begin
          match OpLang.get_input_type oplang_nty with
          | ([], input_typ) ->
              OpLang.Definability.pattern_matching_abstraction state_reading
                argument_identifier input_typ
                (List.map adapt_branch branches)
          | (_ :: _, _) ->
              failwith
                "Definability: universally quantified types are outside the \
                 definable fragment."
        end
      | Either.Right _ ->
          failwith
            "Definability: a continuation is matched where it is introduced, \
             not as a value. Please report."

    let pattern_matching_call state_reading result_identifier value_of_free_name
        callee argument branches =
      match callee with
      | Either.Left oplang_callee ->
          OpLang.Definability.pattern_matching_call state_reading
            result_identifier
            (fun nn -> value_of_free_name (inj_name nn))
            oplang_callee argument
            (List.map adapt_branch branches)
      | Either.Right _ ->
          failwith
            "Definability: a continuation name is answered, never called. \
             Please report."

    let term_of_returned_value = OpLang.Definability.term_of_returned_value
    let sequence = OpLang.Definability.sequence
    let allocate_store = OpLang.Definability.allocate_store
  end
end
