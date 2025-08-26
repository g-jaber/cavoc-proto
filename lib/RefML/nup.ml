module Make (BranchMonad : Util.Monad.BRANCH) :
  Lang.Abstract_val.AVAL
    with type name = Names.name
     and type value = Syntax.value
     and type negative_val = Syntax.negative_val
     and type typ = Types.typ
     and type negative_type = Types.negative_type
     and type label = Syntax.label
     and type store_ctx = Store.Storectx.t
     and type name_ctx = Fnamectx.t * Pnamectx.t
     and type interactive_env = Ienv.IEnv.t
     and module BranchMonad = BranchMonad = struct
  (* Instantiation *)
  type name = Names.name
  type label = Syntax.label
  type value = Syntax.value
  type negative_val = Syntax.negative_val
  type typ = Types.typ
  type negative_type = Types.negative_type
  type store_ctx = Store.Storectx.t
  type name_ctx = Fnamectx.t * Pnamectx.t
  (* *)

  open Syntax
  open Types

  type interactive_env = Ienv.IEnv.t
  type abstract_val = Syntax.value

  let pp_abstract_val = Syntax.pp_term
  let string_of_abstract_val = Format.asprintf "%a" pp_abstract_val
  let names_of_abstract_val aval = Syntax.get_names aval
  let labels_of_abstract_val = Syntax.get_labels
  let empty_namectx = (Fnamectx.empty, Pnamectx.empty)

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
     We also provide a typing context that is used to retrieve the polynorphic names of
     a given type
  *)
  module BranchMonad = BranchMonad
  open BranchMonad

  let generate_abstract_val ((_, cons_ctx) as storectx) (_, pnamectxP) ty =
    let rec aux ((fnamectx, pnamectx) as lnamectx) = function
      | TUnit -> return (Unit, empty_namectx)
      | TBool ->
          let* b = para_list @@ [ true; false ] in
          return (Bool b, empty_namectx)
      | TInt ->
          let* i = BranchMonad.pick_int () in
          return (Int i, empty_namectx)
      | TProd (ty1, ty2) ->
          let* (nup1, lnamectx1) = aux lnamectx ty1 in
          let* (nup2, lnamectx2) = aux lnamectx1 ty2 in
          return (Pair (nup1, nup2), lnamectx2)
      | TSum _ ->
          failwith "Need to add injection to the syntax of expressions"
          (*
    let lnup1 = generate_nup ty1 in
    let lnup1' = List.map (fun (nup,nctx) -> (Inj (1,nup),nctx)) lnup1 in
    let lnup2' = List.map (fun (nup,nctx) -> (Inj (2,nup),nctx)) lnup1 in
    lnup1'@lnup2' *)
      | TArrow _ as ty ->
          let nty = Types.force_negative_type ty in
          let (fn, fnamectx') = Fnamectx.add_fresh fnamectx "" nty in
          return (Name (FName fn), (fnamectx', pnamectx))
      | TId _ as ty ->
          let pnamectxP_pmap = Pnamectx.to_pmap pnamectxP in
          let pn_list = Util.Pmap.select_im ty pnamectxP_pmap in
          let* pn = para_list @@ pn_list in
          return (Name (PName pn), lnamectx)
      | TName _ as ty ->
          let nty = Types.force_negative_type ty in
          let (pn, pnamectx') = Pnamectx.add_fresh pnamectx "" nty in
          return (Name (PName pn), (fnamectx, pnamectx'))
      | TExn ->
          Util.Debug.print_debug
          @@ "Generating exception abstract values in the store context "
          ^ Store.Storectx.to_string storectx;
          let exn_cons_map =
            Util.Pmap.filter_map_im
              (fun ty ->
                match ty with TArrow (_, TExn) -> Some ty | _ -> None)
              cons_ctx in
          let* (c, cons_ty) = para_list @@ Util.Pmap.to_list exn_cons_map in
          begin
            match cons_ty with
            | TArrow (pty, _) ->
                let* (nup, nctx) = aux lnamectx pty in
                return (Constructor (c, nup), nctx)
            | _ -> failwith "TODO"
          end
      | ty ->
          failwith
            ("Error generating a nup on type " ^ Types.string_of_typ ty
           ^ ". Please report") in
    aux empty_namectx ty

  (* namectxO is needed in the following definition to check freshness, while namectxP is needed for checking existence of box names*)
  let type_check_abstract_val (_, pnamectx) _ ty
      (nup, ((lfnamectx, lpnamectx) as lnamectx)) =
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
      | (TArrow _, Name (Names.FName fn)) | (TForall _, Name (Names.FName fn))
        ->
          let nty = Types.force_negative_type ty in
          let* lfnamectx' = Fnamectx.is_last lfnamectx fn nty in
          Some (lfnamectx', lpnamectx)
      | (TArrow _, _) | (TForall _, _) -> None
      | (TId id, Name (Names.PName pn)) -> begin
          match Pnamectx.lookup_exn pnamectx pn with
          (* What about the Not_found exception ?*)
          | TId id' when id = id' -> Some lnamectx
          | _ -> None
        end
      | (TId _, _) -> None
      | (TName _, Name (Names.PName pn)) ->
          let nty = Types.force_negative_type ty in
          let* lpnamectx' = Pnamectx.is_last lpnamectx pn nty in
          Some (lfnamectx, lpnamectx')
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
          ^ Types.string_of_typ ty ^ " is not yet supported." in
    match aux ty (nup, lnamectx) with
    | None -> false
    | Some lnamectx when Namectx.is_empty lnamectx -> true
    | Some _ -> false

  let abstracting_value (value : value) ty =
    let rec aux ((fnamectx, pnamectx) as lnamectx) ienv value ty =
      match (value, ty) with
      | (Fun _, TArrow _)
      | (Fix _, TArrow _)
      | (Name _, TArrow _)
      | (Fun _, TForall (_, TArrow _))
      | (Fix _, TForall (_, TArrow _))
      | (Name _, TForall (_, TArrow _)) -> begin
          let nval = Syntax.force_negative_val value in
          let nty = Types.force_negative_type ty in
          let (fn, fnamectx') = Fnamectx.add_fresh fnamectx "" nty in
          let ienv' = Ienv.IEnv.add_last_check ienv (Names.FName fn) nval in
          (Name (Names.FName fn), ienv', (fnamectx', pnamectx))
        end
      | (Unit, TUnit) | (Bool _, TBool) | (Int _, TInt) ->
          (value, Ienv.IEnv.empty, lnamectx)
      | (Pair (value1, value2), TProd (ty1, ty2)) ->
          let (nup1, ienv1, lnamectx1) = aux lnamectx ienv value1 ty1 in
          let (nup2, ienv2, lnamectx2) = aux lnamectx1 ienv1 value2 ty2 in
          (Pair (nup1, nup2), ienv2, lnamectx2)
      | (_, TId _) -> begin
          let nval = Syntax.force_negative_val value in
          let nty = Types.force_negative_type ty in
          let (pn, pnamectx') = Pnamectx.add_fresh pnamectx "" nty in
          let ienv' = Ienv.IEnv.add_last_check ienv (Names.PName pn) nval in
          (Name (Names.PName pn), ienv', (fnamectx, pnamectx'))
        end
      | (Name _, TName _) -> (value, Ienv.IEnv.empty, lnamectx)
      | (Constructor _, TExn) -> (value, Ienv.IEnv.empty, lnamectx)
      | _ ->
          failwith
            ("Error: " ^ string_of_term value ^ " of type " ^ string_of_typ ty
           ^ " cannot be abstracted because it is not a value.") in
    aux empty_namectx Ienv.IEnv.empty value ty

  let subst_names ienv nup =
    let aux nup (nn, nval) =
      Syntax.subst nup (Name nn) (embed_negative_val nval) in
    Ienv.IEnv.fold aux nup ienv
end
