(**
  [Nup] contains the infrastructure needed to implement the
  {!module-type: Lang.Abstract_val.AVAL} signature.
*)

open Syntax

(**
  The {!module-type: GENERATE_VALUE} signature is implemented by modules providing a
  strategy to generate {e RefML} values. Such modules are used to instanciate
  the {!module: Make} functor.
 *)
module type GENERATE_VALUE = sig
  module BranchMonad : Util.Monad.BRANCH

  val generate_bool : Store.Storectx.t -> (value * Store.Storectx.t) BranchMonad.m
end

(**
  Currently, two strategies are provided:
    - {!module: MakeGenerateConcreteValue}, which generates concrete values ;
    - {!module: MakeGenerateSymbolicValue}, which generates symbolic variables
    instead of concrete booleans and adds these variables to the [storectx].
 *)

module MakeGenerateSymbolicValue (BranchMonad : Util.Monad.BRANCH) = struct
  module BranchMonad = BranchMonad

  let generate_bool (loc_ctx, symbolic_ctx, cons_ctx) =
    let id, symbolic_ctx' = Symbolic.unconstrained symbolic_ctx in
    let storectx' = (loc_ctx, symbolic_ctx', cons_ctx) in
    let value = Symbolic (Symbolic.Kvar id) in
    BranchMonad.return (value, storectx')
end

module MakeGenerateConcreteValue (BranchMonad : Util.Monad.BRANCH) = struct
  module BranchMonad = BranchMonad

  let generate_bool storectx =
    BranchMonad.para_list [ (Bool true, storectx) ; (Bool false, storectx) ]
end

module Make (BranchMonad : Util.Monad.BRANCH)
            (GenerateValue : GENERATE_VALUE
              with module BranchMonad = BranchMonad) :
  Lang.Abstract_val.AVAL
    with type name = Names.name
     and type interactive_env = Ienv.IEnv.t
     and type label = Syntax.label
     and type name_ctx = Namectx.Namectx.t
     and type negative_type = Types.negative_type
     and type negative_val = Syntax.negative_val
     and type renaming = Renaming.Renaming.t
     and type store_ctx = Store.Storectx.t
     and type typ = Types.typ
     and type value = Syntax.value
     and module BranchMonad = BranchMonad = struct
  (* Instantiation *)
  module BranchMonad = BranchMonad

  type name = Names.name
  type renaming = Renaming.Renaming.t
  type label = Syntax.label
  type value = Syntax.value
  type negative_val = Syntax.negative_val
  type typ = Types.typ
  type negative_type = Types.negative_type
  type store_ctx = Store.Storectx.t
  type name_ctx = Namectx.Namectx.t
  (* *)
  open Types

  type interactive_env = Ienv.IEnv.t
  type abstract_val = Syntax.value

  let pp_abstract_val = Syntax.pp_term
  let string_of_abstract_val = Format.asprintf "%a" pp_abstract_val
  let abstract_val_to_yojson aval = `String (string_of_abstract_val aval)
  let names_of_abstract_val aval = Syntax.get_names aval
  let labels_of_abstract_val = Syntax.get_labels
  let rename = Syntax.rename

  let rec unify_abstract_val nspan nup1 nup2 =
    match (nup1, nup2) with
    | (Unit, Unit) -> Some nspan
    | (Bool b1, Bool b2) -> if b1 = b2 then Some nspan else None
    | (Int n1, Int n2) -> if n1 = n2 then Some nspan else None
    | (Pair (nup11, nup12), Pair (nup21, nup22)) ->
        let nspan1_option = unify_abstract_val nspan nup11 nup21 in
        begin
          match nspan1_option with
          | None -> None
          | Some nspan1 -> unify_abstract_val nspan1 nup12 nup22
        end
    | (Name n1, Name n2) -> Util.Namespan.add_nspan (n1, n2) nspan
    | _ ->
        failwith
          ("Error: one of the terms "
          ^ string_of_abstract_val nup1
          ^ " or "
          ^ string_of_abstract_val nup2
          ^ " is not a NUP. Please report.")

  (* The following function is used to generate the nups associated to a given type.
      It takes as input a store context Σ, a name context Γ and a type τ, and
      generates all nups A and name context Δ such that
      - Σ;Γ ⊢ A : τ ▷ Δ (as a nup)
  *)

  let generate_abstract_val (_, _, cons_ctx as storectx) namectx ty =
    let open BranchMonad in
    let rec aux (storectx, lnamectx as res) = function
      | TUnit -> return (Unit, res)
      | TBool ->
          let* (value, storectx') = GenerateValue.generate_bool storectx in
          return (value, (storectx', lnamectx))
      | TInt ->
          let* i = BranchMonad.pick_int () in
          return (Int i, res)
      | TProd (ty1, ty2) ->
          let* (nup1, res) = aux res ty1 in
          let* (nup2, res) = aux res ty2 in
          return (Pair (nup1, nup2), res)
      | TSum _ ->
          failwith "Need to add injection to the syntax of expressions"
          (*
    let lnup1 = generate_nup ty1 in
    let lnup1' = List.map (fun (nup,nctx) -> (Inj (1,nup),nctx)) lnup1 in
    let lnup2' = List.map (fun (nup,nctx) -> (Inj (2,nup),nctx)) lnup1 in
    lnup1'@lnup2' *)
      | TArrow _ as ty ->
          let nty = Types.force_negative_type ty in
          let (fn, (lnamectx')) = Namectx.Namectx.add_fresh lnamectx "" nty in
          return (Name fn, (storectx, lnamectx'))
      | TId _ as ty ->
          let namectxP_pmap = Namectx.Namectx.to_pmap namectx in
          let pn_list = Util.Pmap.select_im ty namectxP_pmap in
          let* pn = para_list @@ pn_list in
          let* _ =
            return @@ Util.Debug.print_debug @@ "Reusing the pname "
            ^ Names.string_of_name pn ^ " from the namectx "
            ^ Namectx.Namectx.to_string namectx in
          return (Name pn, res)
      | TName _ as ty ->
          let nty = Types.force_negative_type ty in
          let (pn, lnamectx') = Namectx.Namectx.add_fresh lnamectx "" nty in
          Util.Debug.print_debug @@ "Creating a fresh pname "
          ^ Names.string_of_name pn ^ " and putting it in the name context "
          ^ Namectx.Namectx.to_string lnamectx';
          return (Name pn, (storectx, lnamectx'))
      | TExn ->
          Util.Debug.print_debug
          @@ "Generating exception abstract values in the store context "
          ^ Store.Storectx.to_string storectx ;
          let exn_cons_map =
            Util.Pmap.filter_map_im
              (fun ty ->
                match ty with TArrow (_, TExn) -> Some ty | _ -> None)
              cons_ctx in
          let* (c, cons_ty) = para_list @@ Util.Pmap.to_list exn_cons_map in
          begin
            match cons_ty with
            | TArrow (pty, _) ->
                let* (nup, res) = aux res pty in
                return (Constructor (c, Some nup), res)
            | _ -> failwith "TODO"
          end
      | TRecord fields -> (
        let instantiate_field m (field_name, ty) = 
          let* (current_fields, current_res) = m in
          let* (nup, new_res) = aux current_res ty in
          let new_fields = Util.Pmap.add (field_name, nup) current_fields in
          return (new_fields, new_res)
        in
        let* (instance_fields, new_res) = 
          Util.Pmap.fold instantiate_field (return (Util.Pmap.empty, res)) fields in
        return (Record instance_fields, new_res)
      )
      | ty ->
          failwith
            ("Error generating a nup on type " ^ Types.string_of_typ ty
           ^ ". Please report") in
    let empty_ctx = Namectx.Namectx.empty in
    aux (storectx, empty_ctx) ty

  (* namectxO is needed in the following definition to check freshness, while namectxP is needed for checking existence of box names*)
  let type_check_abstract_val _storectx namectx ty (nup, lnamectx) =
    let rec aux ty (nup, lnamectx) =
      let open Util.Monad.Option in
      match (ty, nup) with
      | (TUnit, Unit) -> Some lnamectx
      | (TUnit, _) -> None
      | (TBool, Bool _) -> Some lnamectx
      | (TBool, _) -> None
      | (TInt, Int _) -> Some lnamectx
      | (TInt, _) -> None
      | (TProd (ty1, ty2), Pair (nup1, nup2)) -> begin
          let* lnamectx' = aux ty1 (nup1, lnamectx) in
          aux ty2 (nup2, lnamectx')
        end
      | (TProd _, _) -> None
      | (TRecord ty_fields, Record val_fields) -> 
        let check_on_field lnamectx_m (field_name, ty) =
          let* current_lnamectx = lnamectx_m in
          let associated_val = Util.Pmap.lookup_exn field_name val_fields in
          aux ty (associated_val, current_lnamectx)
        in
        Util.Pmap.fold check_on_field (Some lnamectx) ty_fields 
      | (TRecord _, _) -> None 
      | (TArrow _, Name nn) | (TForall _, Name nn) ->
          let nty = Types.force_negative_type ty in
          Namectx.Namectx.is_last lnamectx nn nty
      | (TArrow _, _) | (TForall _, _) -> None
      | (TId id, Name nn) -> begin
          match Namectx.Namectx.lookup_exn namectx nn with
          (* What about the Not_found exception ?*)
          | TId id' when id = id' -> Some lnamectx
          | _ -> None
        end
      | (TId _, _) -> None
      | (TName _, Name nn) ->
          let nty = Types.force_negative_type ty in
          Namectx.Namectx.is_last lnamectx nn nty
      (*TODO: Should we check to who belongs the TName ? *)
      (* | (TExn, Constructor (c, nup')) ->  
        let (TArrow (param_ty, _)) = Util.Pmap.lookup_exn c (Util.Pmap.concat namectxP namectxO) in 
        type_check_abstract_val namectxP namectxO param_ty nup' *)
      | (TName _, _) -> None
      | (TVar _, _) ->
          failwith @@ "Error: trying to type-check a nup of type "
          ^ Types.string_of_typ ty ^ ". Please report."
      | (TUndef, _) | (TRef _, _) | (TSum _, _) | (TExn, _) ->
          failwith @@ "Error: type-checking a nup of type "
          ^ Types.string_of_typ ty ^ " is not yet supported."
      | (TAlgebraic _, _) -> failwith "Algebraic type are not yet supported (type_check_abstract_val)"
    in
    match aux ty (nup, lnamectx) with
    | None -> false
    | Some lnamectx when Namectx.Namectx.is_empty lnamectx -> true
    | Some _ -> false

  let abstracting_value (value : value) namectxO ty =
    let rec aux ienv value ty =
      match (value, ty) with
      | (Fun _, TArrow _)
      | (Fix _, TArrow _)
      | (Name _, TArrow _)
      | (Fun _, TForall (_, TArrow _))
      | (Fix _, TForall (_, TArrow _))
      | (Name _, TForall (_, TArrow _)) -> begin
          let nval = Syntax.force_negative_val value in
          let nty = Types.force_negative_type ty in
          let (fn, ienv') = Ienv.IEnv.add_fresh ienv "" nty nval in
          (Name fn, ienv')
        end
      | (Unit, TUnit) | (Bool _, TBool) | (Int _, TInt) -> (value, ienv)
      (* Symbolic expressions are treated as values *)
      | (Symbolic _, _) -> (value, ienv)
      | (Pair (value1, value2), TProd (ty1, ty2)) ->
          let (nup1, ienv1) = aux ienv value1 ty1 in
          let (nup2, ienv2) = aux ienv1 value2 ty2 in
          (Pair (nup1, nup2), ienv2)
      | (_, TId _) -> begin
          let nval = Syntax.force_negative_val value in
          let nty = Types.force_negative_type ty in
          let (pn, ienv') = Ienv.IEnv.add_fresh ienv "" nty nval in
          (Name pn, ienv')
        end
      | (Name _, TName _) -> (value, ienv)
      | (Constructor _, TExn) -> (value, ienv)
      | (Record val_fields, TRecord ty_fields) -> 
          let abstracting_field (new_fields, current_ienv) (field_name, expr) =
            let associated_ty = Util.Pmap.lookup_exn field_name ty_fields in
            let (nup, ienv') = aux current_ienv expr associated_ty in
            (Util.Pmap.add (field_name, nup) new_fields, ienv')
          in
          let (new_fields, ienv') = 
            Util.Pmap.fold abstracting_field (Util.Pmap.empty, ienv) val_fields in
          (Record new_fields, ienv')
      | _ ->
          failwith
            ("Error: " ^ string_of_term value ^ " of type " ^ string_of_typ ty
           ^ " cannot be abstracted because it is not a value.") in
    aux (Ienv.IEnv.empty namectxO) value ty

  let subst_pnames (_ienvf, ienvp) nup =
    let aux nup (nn, nval) =
      Syntax.subst nup (Name (Names.embed_pname nn)) (embed_negative_val nval)
    in
    Ienv.IEnvP.fold aux nup ienvp (* TODO : Not efficient at all*)
end
