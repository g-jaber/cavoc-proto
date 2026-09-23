(* Abstract normal forms with their interactive environments and the
   operations on their names. *)
(* Abstract normal forms can be thought of as named copatterns when the
   language is of signature Language.WITHAVAL_INOUT. *)
module type A_NF = sig
  (* Interactive environments γ are partial maps from names to interactive
     values. *)
  module IEnv : Ienv.IENV

  type abstract_normal_form [@@deriving to_yojson]

  val renaming_a_nf :
    IEnv.Renaming.t -> abstract_normal_form -> abstract_normal_form

  val get_subject_name :
    abstract_normal_form -> IEnv.Renaming.Namectx.Names.name

  (* fold_free_names_of_a_nf f acc A folds f over the free names (starting
     with the subject name) of A; the store part of A is not traversed. *)
  val fold_free_names_of_a_nf :
    ('a -> IEnv.Renaming.Namectx.Names.name -> 'a) ->
    'a ->
    abstract_normal_form ->
    'a

  (* map_free_names_of_a_nf f A renames the free names (including the
     subject name) of A along f; the store part of A is not traversed. *)
  val map_free_names_of_a_nf :
    (IEnv.Renaming.Namectx.Names.name -> IEnv.Renaming.Namectx.Names.name) ->
    abstract_normal_form ->
    abstract_normal_form

  (* The first argument is a string inserted between the negative part of
     the normal form and the abstract values filling the positive parts. *)
  val pp_a_nf :
    pp_dir:(Format.formatter -> unit) ->
    Format.formatter ->
    abstract_normal_form ->
    unit

  (* Like pp_a_nf, with bound and free names displayed by the provided
     printers; head names are free. *)
  val pp_a_nf_in :
    pp_dir:(Format.formatter -> unit) ->
    pp_free_name:(Format.formatter -> IEnv.Renaming.Namectx.Names.name -> unit) ->
    pp_bound_name:(Format.formatter -> IEnv.Renaming.Namectx.Names.name -> unit) ->
    Format.formatter ->
    abstract_normal_form ->
    unit

  val string_of_a_nf : string -> abstract_normal_form -> string

  (* Equivalence up to what the environment can observe. The heap part is
     compared only when asked: POGS relates heaps a posteriori. *)
  val is_equiv_a_nf :
    compare_heaps:bool -> abstract_normal_form -> abstract_normal_form -> bool
end

(* Abstract normal forms with their generation and type checking. *)
module type TYPED_A_NF = sig
  include A_NF
  module BranchMonad : Util.Monad.BRANCH
  module Storectx : Typectx.TYPECTX

  (* The disclosed store context Σ' a move leaves. *)
  val store_ctx_of_a_nf : abstract_normal_form -> Storectx.t

  (* From the interactive name context Γ_P, all the pairs (A,Δ,Γ'_P) formed
     by an abstracted normal form A such that Γ_P;_ ⊢ A ▷ Δ. *)
  (* The names introduced by A are de Bruijn levels of the locally built Δ,
     given an ambient identity by the weakening Δ ↪ Γ_O + Δ that the machine
     computes, so that Γ_O is not needed here. *)
  val generate_a_nf :
    Storectx.t ->
    IEnv.Renaming.Namectx.t ->
    (abstract_normal_form * IEnv.Renaming.Namectx.t * IEnv.Renaming.Namectx.t)
    BranchMonad.m

  (* The typing judgment Σ;Γ_P;Γ_O ⊢ A ▷ Δ, returning the interactive name
     context Γ'_P where the linear resources of Γ_P used by A have been
     removed; None when the type checking fails. *)
  val type_check_a_nf :
    Storectx.t ->
    IEnv.Renaming.Namectx.t ->
    IEnv.Renaming.Namectx.t ->
    abstract_normal_form * IEnv.Renaming.Namectx.t ->
    IEnv.Renaming.Namectx.t option
end

module type LANG = sig
  include TYPED_A_NF
  module EvalMonad : Util.Monad.RUNNABLE

  type opconf

  val string_of_opconf : opconf -> string
  val pp_opconf : Format.formatter -> opconf -> unit

  type store [@@deriving to_yojson]

  val string_of_store : store -> string
  val pp_store : Format.formatter -> store -> unit
  val infer_type_store : store -> Storectx.t

  (* The typed focusing process implemented by abstracting_nf decomposes a
     normal form into an abstract normal form for the observable part and a
     typed interactive environment for the negative part. *)
  (* abstracting_nf nf Γₒ Σ returns a triple (anf,γ,Σ') where anf{γ} = nf and
     Σ;Γₒ ⊢ anf ▷ Δ,Σ' and Σ;Γₒ ⊢ γ:Δ. *)
  val eval :
    opconf * IEnv.Renaming.Namectx.t * Storectx.t ->
    ((abstract_normal_form * IEnv.Renaming.Namectx.t * Storectx.t)
    * IEnv.t
    * store)
    EvalMonad.m

  val concretize_a_nf :
    store -> IEnv.t -> abstract_normal_form * IEnv.Renaming.t -> opconf * IEnv.t

  (* Every location holding a ground value becomes public; the identity for a
     language without heap. *)
  val disclose_heap : store -> store

  (* The store part of a move extended with the values of the public
     locations it does not cover yet, disclosing through them as eval does. *)
  val complete_abstract_store :
    store -> abstract_normal_form * IEnv.t -> abstract_normal_form * IEnv.t
end

module type LANG_WITH_INIT = sig
  include LANG

  val get_typed_opconf :
    ?opponent_signature:Lexing.lexbuf ->
    string ->
    Lexing.lexbuf ->
    opconf * IEnv.Renaming.Namectx.t

  (* The function get_typed_ienv
     retrive a module declaration and its signature from the two in_channel taken as input.
     It evaluates the list of computation declarations
     into a list of value declarations together with the store
     generated by this evaluation.
     We return a Proponent and an Opponent name context. *)
  val get_typed_ienv :
    ?opponent_signature:Lexing.lexbuf ->
    Lexing.lexbuf ->
    Lexing.lexbuf ->
    IEnv.t * store * IEnv.Renaming.Namectx.t * IEnv.Renaming.Namectx.t

  (* The name context a signature declares, with no implementation behind
     it. *)
  val get_typed_namectx : Lexing.lexbuf -> IEnv.Renaming.Namectx.t
end

(* The store part of a move: the disclosed store context Σ' the move leaves,
   the value of every location of Σ' in the order of their names, and the
   operational store restricted to the constraints of the branch and the
   declarations. *)
type ('abstract_val, 'store_ctx, 'store) abstract_store = {
  storectx: 'store_ctx;
  values: 'abstract_val list;
  constraints: 'store;
}

(* The operations on abstract stores shared by the CPS and the direct-style
   interactive languages. *)
module Abstract_store
    (OpLang : sig
      module Store : Language.STORE
      module IEnv : Ienv.IENV

      module AVal :
        Abstract_val.AVAL
          with type store = Store.store
           and type store_ctx = Store.Storectx.t
           and type typ = Store.typ
           and type interactive_env = IEnv.t
           and type name_ctx = IEnv.Renaming.Namectx.t
           and type name = IEnv.Renaming.Namectx.Names.name
           and type renaming = IEnv.Renaming.t
    end) =
struct
  open OpLang

  type t = (AVal.abstract_val, Store.Storectx.t, Store.store) abstract_store

  (* The declarations alone are readable from Σ', so only the constraints of
     the branch are worth printing. *)
  let has_constraints (astore : t) =
    astore.constraints <> Store.without_heap astore.storectx Store.empty_store

  let pp_in ~pp_free_name ~pp_bound_name fmt (astore : t) =
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    let pp_value fmt (loc_name, aval) =
      Format.fprintf fmt "%a ↪ %a" Store.LocCtx.Names.pp_name loc_name
        (AVal.pp_abstract_val_in ~pp_free_name ~pp_bound_name)
        aval in
    let values =
      List.combine
        (Store.LocCtx.get_names (Store.loc_ctx astore.storectx))
        astore.values in
    if values <> [] then
      Format.fprintf fmt "[@[<hov>%a@]]"
        (Format.pp_print_list ~pp_sep pp_value)
        values;
    if has_constraints astore then Store.pp_store fmt astore.constraints

  let is_printable (astore : t) = astore.values <> [] || has_constraints astore

  let rename (astore : t) renaming =
    let rename_value aval = AVal.rename aval renaming in
    let values = List.map rename_value astore.values in
    { astore with values }

  let is_equiv ~compare_heaps (astore1 : t) (astore2 : t) =
    ((not compare_heaps)
    || List.length astore1.values = List.length astore2.values
       && List.for_all2
            (AVal.is_equiv_abstract_val astore1.constraints astore2.constraints)
            astore1.values astore2.values)
    && Store.is_equiv_store astore1.constraints astore2.constraints

  (* The store part of a Player move whose first values are given, from the
     store holding the constraints of the branch. *)
  let abstracting store values constraints ienv =
    let (values', ienv', store') = AVal.abstracting_store ienv store values in
    let storectx = Store.infer_type_store store' in
    let astore : t =
      {
        storectx;
        values= values';
        constraints= Store.without_heap storectx constraints;
      } in
    (astore, ienv', store')

  (* A stored value may disclose locations, so the values are generated until
     every location of the Σ' declared so far has one. *)
  let generate namectxP storectx lnamectx =
    let open AVal.BranchMonad in
    let rec loop storectx lnamectx values =
      let loc_ctx = Store.loc_ctx storectx in
      match
        List.nth_opt (Store.LocCtx.get_names loc_ctx) (List.length values)
      with
      | None -> return (values, (storectx, lnamectx))
      | Some loc_name ->
          let ty =
            Store.embed_loc_typ (Store.LocCtx.lookup_exn loc_ctx loc_name)
          in
          let* (aval, (storectx', lnamectx')) =
            AVal.generate_abstract_val storectx lnamectx namectxP ty in
          loop storectx' lnamectx' (values @ [ aval ]) in
    let* (values, (storectx', lnamectx')) = loop storectx lnamectx [] in
    let astore : t =
      {
        storectx= storectx';
        values;
        constraints= Store.without_heap storectx' Store.empty_store;
      } in
    return (astore, (storectx', lnamectx'))

  (* The values are checked at the types Σ' gives them from the pair (Σ, Δ)
     the value of the move has built, which must end as (Σ', Δ) of the move. *)
  let type_check namectxP namectxO storectx lnamectx
      ((astore : t), lnamectx_move) =
    let rec loop storectx lnamectx values =
      let loc_ctx = Store.loc_ctx storectx in
      let covered = List.length astore.values - List.length values in
      match (List.nth_opt (Store.LocCtx.get_names loc_ctx) covered, values) with
      | (None, []) ->
          loc_ctx = Store.loc_ctx astore.storectx
          && IEnv.Renaming.Namectx.erase_display_hints lnamectx
             = IEnv.Renaming.Namectx.erase_display_hints lnamectx_move
      | (Some loc_name, aval :: values') -> begin
          let ty =
            Store.embed_loc_typ (Store.LocCtx.lookup_exn loc_ctx loc_name)
          in
          match
            AVal.type_check_abstract_val storectx lnamectx namectxP namectxO ty
              aval
          with
          | Some (storectx', lnamectx') -> loop storectx' lnamectx' values'
          | None -> false
        end
      | (None, _ :: _) | (Some _, []) -> false in
    loop storectx lnamectx astore.values

  let concretize store ienv (astore : t) =
    AVal.concretize_store ienv
      (Store.update_store store astore.constraints)
      astore.storectx astore.values
end

(* The following functor create a module of type Interactive.LANG_WITH_INIT
   from a module OpLang of type Language.WITHAVAL_NEG *)
module Make (OpLang : Language.WITHAVAL_NEG) :
  LANG_WITH_INIT
    with module EvalMonad = OpLang.EvalMonad
     and module IEnv = OpLang.IEnv
     and type abstract_normal_form =
      ( OpLang.AVal.abstract_val,
        unit,
        OpLang.Names.name,
        OpLang.Names.name )
      OpLang.Nf.nf_term
      * ( OpLang.AVal.abstract_val,
          OpLang.Store.Storectx.t,
          OpLang.Store.store )
        abstract_store = struct
  module EvalMonad = OpLang.EvalMonad
  module BranchMonad = OpLang.AVal.BranchMonad
  module IEnv = OpLang.IEnv
  module Store = OpLang.Store
  module AStore = Abstract_store (OpLang)

  type opconf = OpLang.opconf

  let pp_opconf = OpLang.pp_opconf
  let string_of_opconf = Format.asprintf "%a" pp_opconf

  type store = OpLang.Store.store [@@deriving to_yojson]

  let string_of_store = OpLang.Store.string_of_store
  let pp_store = OpLang.Store.pp_store

  module Storectx = OpLang.Store.Storectx

  let infer_type_store = OpLang.Store.infer_type_store

  type abstract_normal_form =
    ( OpLang.AVal.abstract_val,
      unit,
      IEnv.Renaming.Namectx.Names.name,
      IEnv.Renaming.Namectx.Names.name )
    OpLang.Nf.nf_term
    * AStore.t

  let store_ctx_of_a_nf (_, (astore : AStore.t)) = astore.storectx

  let pp_a_nf_in ~pp_dir ~pp_free_name ~pp_bound_name fmt (a_nf_term, astore) =
    let pp_ectx fmt () = Format.pp_print_string fmt "" in
    let pp_a_nf_term =
      OpLang.Nf.pp_nf_term ~pp_dir
        (OpLang.AVal.pp_abstract_val_in ~pp_free_name ~pp_bound_name)
        pp_ectx pp_free_name pp_free_name in
    if AStore.is_printable astore then
      Format.fprintf fmt "%a,%a" pp_a_nf_term a_nf_term
        (AStore.pp_in ~pp_free_name ~pp_bound_name)
        astore
    else pp_a_nf_term fmt a_nf_term

  let pp_a_nf ~pp_dir =
    let pp_name = OpLang.IEnv.Renaming.Namectx.Names.pp_name in
    pp_a_nf_in ~pp_dir ~pp_free_name:pp_name ~pp_bound_name:pp_name

  let string_of_a_nf dir =
    let pp_dir fmt = Format.pp_print_string fmt dir in
    Format.asprintf "%a" (pp_a_nf ~pp_dir)

  let get_subject_name (a_nf_term, _) =
    let f_fn nn = (nn, Some nn) in
    let f_cn nn = (nn, Some nn) in
    match snd @@ OpLang.Nf.map_fn None f_fn a_nf_term with
    | None -> snd @@ OpLang.Nf.map_cn None f_cn a_nf_term
    | Some _ as res -> res

  let abstract_normal_form_to_yojson a_nf =
    let[@warning "-8"] (Some nn) = get_subject_name a_nf in
    `Assoc
      [
        ("subjectName", IEnv.Renaming.Namectx.Names.name_to_yojson nn);
        ("string", `String (string_of_a_nf "" a_nf));
      ]

  let renaming_a_nf renaming (a_nf_term, astore) =
    let a_nf_term' =
      OpLang.Nf.map
        ~f_val:(fun aval -> OpLang.AVal.rename aval renaming)
        ~f_fn:Fun.id ~f_cn:Fun.id ~f_ectx:Fun.id a_nf_term in
    (a_nf_term', AStore.rename astore renaming)

  let concretize_a_nf store ienv (a_nf, renaming) =
    (* we get renaming : Δ → Γₒ + Δ and ienv : Γₚ → Γₒ*)
    let lnamectx = OpLang.Renaming.dom renaming in
    Util.Debug.print_debug @@ "concretize the a nf " ^ string_of_a_nf "" a_nf;
    Util.Debug.print_debug @@ "IEnv provided in input  : " ^ IEnv.to_string ienv;
    Util.Debug.print_debug @@ "Renaming provided in input  : "
    ^ IEnv.Renaming.to_string renaming;
    let (a_nf_term', astore') = renaming_a_nf renaming a_nf in
    Util.Debug.print_debug @@ "After renaming: "
    ^ string_of_a_nf "" (a_nf_term', astore');
    (* ienv':Γₚ → Γₒ+Δ *)
    let ienv' = IEnv.weaken_r ienv lnamectx in
    let renaming_lifted = IEnv.embed_renaming renaming in
    Util.Debug.print_debug @@ "Renaming as ienv  : "
    ^ IEnv.to_string renaming_lifted;
    (* ienv'' = ienv ⊗ renaming, so that ienv'':Γₚ+Δ → Γₒ+Δ *)
    let ienv'' = IEnv.copairing ienv' renaming_lifted in
    let get_ty nn =
      OpLang.negating_type
        (IEnv.Renaming.Namectx.lookup_exn (IEnv.dom ienv') nn) in
    let typed_term = OpLang.type_annotating_val get_ty a_nf_term' in
    (* The store part first: it allocates the locations the value mentions. *)
    let newstore = AStore.concretize store ienv' astore' in
    let f_val (aval, gty) = OpLang.AVal.subst_pnames ienv' newstore gty aval in
    let f_fn nn = IEnv.lookup_exn ienv'' nn in
    let f_cn = f_fn in
    let f_ectx () = () in
    let nf_term' = OpLang.Nf.map ~f_val ~f_fn ~f_cn ~f_ectx typed_term in
    let newterm = OpLang.refold_nf_term nf_term' in
    Util.Debug.print_debug @@ "Once concretized we get "
    ^ OpLang.string_of_term newterm;
    ((newterm, newstore), ienv')
  (* We do not use ienv'' here as it has an extra identity component for the renaming of Δ*)

  let abstracting_nf_term nf_term namectxO store =
    Util.Debug.print_debug @@ "Trying to abstract the nf_term ";
    let get_ty nn =
      Util.Debug.print_debug @@ "Looking for "
      ^ IEnv.Renaming.Namectx.Names.string_of_name nn
      ^ " in "
      ^ IEnv.Renaming.Namectx.to_string namectxO;
      let nty = IEnv.Renaming.Namectx.lookup_exn namectxO nn in
      Util.Debug.print_debug @@ "The type of "
      ^ IEnv.Renaming.Namectx.Names.string_of_name nn
      ^ " is "
      ^ OpLang.string_of_negative_type nty;
      OpLang.negating_type nty in
    Util.Debug.print_debug "going to call type_annotating_val";
    let nf_typed_term = OpLang.type_annotating_val get_ty nf_term in
    let f_val (value, ty) =
      Util.Debug.print_debug @@ " Abstracting the value "
      ^ OpLang.string_of_value value
      ^ " of type " ^ OpLang.string_of_type ty;
      let (aval, ienv, store') =
        OpLang.AVal.abstracting_value value namectxO store ty in
      (aval, (ienv, store')) in
    let empty_res = (IEnv.empty namectxO, store) in
    OpLang.Nf.map_val empty_res f_val nf_typed_term

  let abstracting_nf (nf_term, store) namectxO =
    let (a_nf_term, (ienv, store')) = abstracting_nf_term nf_term namectxO store in
    if OpLang.Nf.is_error a_nf_term then None
    else
      let (astore, ienv', store'') =
        AStore.abstracting store' [] store' ienv in
      Some ((a_nf_term, astore), ienv', store'')

  let get_subject_name (a_nf_term, _) =
    let f_fn nn = (nn, Some nn) in
    let f_cn nn = (nn, Some nn) in
    match snd @@ OpLang.Nf.map_fn None f_fn a_nf_term with
    | None -> begin
        match snd @@ OpLang.Nf.map_cn None f_cn a_nf_term with
        | None -> failwith "No subject name in an a_nf_term. Please report"
        | Some nn' -> nn'
      end
    | Some nn -> nn

  let fold_free_names_of_a_nf f acc ((a_nf_term, _) as a_nf) =
    let acc' = f acc (get_subject_name a_nf) in
    OpLang.Nf.apply_val acc'
      (OpLang.AVal.fold_free_names_of_abstract_val f acc')
      a_nf_term

  let map_free_names_of_a_nf f (a_nf_term, astore) =
    let a_nf_term' =
      OpLang.Nf.map ~f_fn:f ~f_cn:f
        ~f_val:(OpLang.AVal.map_free_names_of_abstract_val f)
        ~f_ectx:Fun.id a_nf_term in
    (a_nf_term', astore)

  let eval (opconf, namectxO, _storectx) =
    let open EvalMonad in
    let* (term', store') = OpLang.normalize_opconf opconf in
    let nf_term = OpLang.get_nf_term term' in
    match abstracting_nf (nf_term, store') namectxO with
    | Some (a_nf, ienv, store'') ->
        let lnamectx = IEnv.dom ienv in
        return ((a_nf, lnamectx, store_ctx_of_a_nf a_nf), ienv, store'')
    | None -> stop ()

  let disclose_heap = Store.disclose_heap

  let complete_abstract_store store ((a_nf_term, (astore : AStore.t)), ienv) =
    let (astore', ienv', _) =
      AStore.abstracting store astore.values astore.constraints ienv in
    ((a_nf_term, astore'), ienv')

  include OpLang.AVal.BranchMonad

  let fill_abstract_val storectx namectxP_pmap nf_skeleton =
    let gen_val ty =
      OpLang.AVal.generate_abstract_val storectx IEnv.Renaming.Namectx.empty
        namectxP_pmap ty in
    OpLang.Nf.abstract_nf_term_m ~gen_val nf_skeleton

  let generate_a_nf storectx namectxP =
    let* _ = return @@ Util.Debug.print_debug @@ "Generating the skeleton " in
    let* skel = OpLang.generate_nf_term namectxP in
    let* _ = return @@ Util.Debug.print_debug @@ "Filling the skeleton " in
    let* (a_nf_term, (storectx, lnamectx)) =
      fill_abstract_val storectx namectxP skel in
    let* _ =
      return @@ Util.Debug.print_debug @@ "Once filled we get the new names "
      ^ OpLang.IEnv.Renaming.Namectx.to_string lnamectx in
    let* (astore, (_, lnamectx')) =
      AStore.generate namectxP storectx lnamectx in
    return ((a_nf_term, astore), lnamectx', namectxP)

  let type_check_a_nf storectx namectxP namectxO ((nf_term, astore), lnamectx) =
    let type_check_val aval nty =
      match
        OpLang.AVal.type_check_abstract_val storectx IEnv.Renaming.Namectx.empty
          namectxP namectxO (OpLang.negating_type nty) aval
      with
      | Some (storectx', lnamectx') ->
          AStore.type_check namectxP namectxO storectx' lnamectx'
            (astore, lnamectx)
      | None -> false in
    OpLang.type_check_nf_term ~name_ctx:namectxP ~type_check_val nf_term

  let is_equiv_a_nf ~compare_heaps (anf1, (astore1 : AStore.t))
      (anf2, (astore2 : AStore.t)) =
    OpLang.Nf.equiv_nf_term
      (OpLang.AVal.is_equiv_abstract_val astore1.constraints
         astore2.constraints)
      anf1 anf2
    && AStore.is_equiv ~compare_heaps astore1 astore2

  let get_typed_ienv = OpLang.get_typed_ienv
  let get_typed_namectx = OpLang.get_typed_namectx

  let get_typed_opconf ?opponent_signature nbprog lexBuffer =
    let (opconf, _, namectxO) =
      OpLang.get_typed_opconf ?opponent_signature nbprog lexBuffer in
    (opconf, namectxO)
end
