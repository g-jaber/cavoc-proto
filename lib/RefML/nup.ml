module Make (M : Util.Monad.BRANCH) :
  Lang.Abstract_val.AVAL
    with type name = Names.name
     and type value = Syntax.value
     and type negative_val = Syntax.negative_val
     and type typ = Types.typ
     and type negative_type = Types.negative_type
     and type label = Syntax.label
     and type store_ctx = Store.store_ctx
     and module M = M = struct
  (* Instantiation *)
  type name = Names.name
  type label = Syntax.label
  type value = Syntax.value
  type negative_val = Syntax.negative_val
  type typ = Types.typ
  type negative_type = Types.negative_type
  type store_ctx = Store.store_ctx
  (* *)

  open Syntax
  open Types

  type name_ctx = (name, negative_type) Util.Pmap.pmap
  type interactive_env = (name, negative_val) Util.Pmap.pmap
  type abstract_val = Syntax.value

  let pp_abstract_val = Syntax.pp_term

  let string_of_abstract_val = Format.asprintf "%a" pp_abstract_val
  let names_of_abstract_val = Syntax.get_names
  let labels_of_abstract_val = Syntax.get_labels

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
  module M = M
  open M

  let rec generate_abstract_val ((_, cons_ctx) as storectx) namectxP = function
    | TUnit -> return (Unit, Util.Pmap.empty)
    | TBool ->
        let* b = para_list @@ [ true; false ] in
        return (Bool b, Util.Pmap.empty)
    | TInt ->
        let* i = M.pick_int () in
        return (Int i, Util.Pmap.empty)
    | TProd (ty1, ty2) ->
        let* (nup1, nctx1) = generate_abstract_val storectx namectxP ty1 in
        let* (nup2, nctx2) = generate_abstract_val storectx namectxP ty2 in
        return (Pair (nup1, nup2), Util.Pmap.concat nctx1 nctx2)
    | TSum _ ->
        failwith "Need to add injection to the syntax of expressions"
        (*
    let lnup1 = generate_nup ty1 in
    let lnup1' = List.map (fun (nup,nctx) -> (Inj (1,nup),nctx)) lnup1 in
    let lnup2' = List.map (fun (nup,nctx) -> (Inj (2,nup),nctx)) lnup1 in
    lnup1'@lnup2' *)
    | TArrow _ as ty ->
        let fn = Names.fresh_fname () in
        let nty = Types.force_negative_type ty in
        return (Name fn, Util.Pmap.singleton (fn, nty))
    | TId _ as ty ->
        let pn_list = Util.Pmap.select_im ty namectxP in
        let* pn = para_list @@ pn_list in
        return (Name pn, Util.Pmap.empty)
    | TName _ as ty ->
        let pn = Names.fresh_pname () in
        let nty = Types.force_negative_type ty in
        return (Name pn, Util.Pmap.singleton (pn, nty))
    | TExn ->
        Util.Debug.print_debug
        @@ "Generating exception abstract values in the store context "
        ^ Store.string_of_store_ctx storectx;
        let exn_cons_map = Util.Pmap.filter_map_im (fun ty -> match ty with TArrow (_, TExn) -> Some ty | _ ->  None) cons_ctx in
        let* (c, cons_ty) = para_list @@ Util.Pmap.to_list exn_cons_map in
        begin
          match cons_ty with 
          | TArrow(pty, _) -> let* (nup, nctx) = generate_abstract_val storectx namectxP pty in
               return (Constructor (c, nup), nctx)
          | _ -> failwith ""
        end
    | ty ->
        failwith
          ("Error generating a nup on type " ^ Types.string_of_typ ty
         ^ ". Please report")

  let rec type_check_abstract_val namectxP namectxO ty nup =
    match (ty, nup) with
    | (TUnit, Unit) -> Some Util.Pmap.empty
    | (TUnit, _) -> None
    | (TBool, Bool _) -> Some Util.Pmap.empty
    | (TBool, _) -> None
    | (TInt, Int _) -> Some Util.Pmap.empty
    | (TInt, _) -> None
    | (TProd (ty1, ty2), Pair (nup1, nup2)) -> begin
        match
          ( type_check_abstract_val namectxP namectxO ty1 nup1,
            type_check_abstract_val namectxP namectxO ty2 nup2 )
        with
        | (None, _) | (_, None) -> None
        | (Some namectxO1, Some namectxO2) ->
            if Util.Pmap.disjoint namectxO1 namectxO2 then
              Some (Util.Pmap.concat namectxO1 namectxO2)
            else None
      end
    | (TProd _, _) -> None
    | (TArrow _, Name nn) | (TForall _, Name nn) ->
        if Util.Pmap.mem nn namectxP || Util.Pmap.mem nn namectxO then None
          (* the name nn has to be fresh to be well-typed *)
        else
          let nty = Types.force_negative_type ty in
          Some (Util.Pmap.singleton (nn, nty))
    | (TArrow _, _) | (TForall _, _) -> None
    | (TId id, Name nn) -> begin
        match Util.Pmap.lookup nn namectxP with
        | None -> None
        | Some (TId id') when id = id' -> Some Util.Pmap.empty
        | Some _ -> None
      end
    | (TId _, _) -> None
    | (TName _, Name nn) ->
        if Util.Pmap.mem nn namectxP || Util.Pmap.mem nn namectxO then None
          (* the name nn has to be fresh to be well-typed *)
        else
          let nty = Types.force_negative_type ty in
          Some (Util.Pmap.singleton (nn, nty))
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

  let rec abstracting_value (value : value) ty =
    match (value, ty) with
    | (Fun _, TArrow _) | (Fix _, TArrow _) | (Name _, TArrow _) ->
        let fn = Names.fresh_fname () in
        let nval = Syntax.force_negative_val value in
        let nty = Types.force_negative_type ty in
        let ienv = Util.Pmap.singleton (fn, nval) in
        let lnamectx = Util.Pmap.singleton (fn, nty) in
        (Name fn, ienv, lnamectx)
    | (Unit, TUnit) | (Bool _, TBool) | (Int _, TInt) ->
        (value, Util.Pmap.empty, Util.Pmap.empty)
    | (Pair (value1, value2), TProd (ty1, ty2)) ->
        let (nup1, ienv1, lnamectx1) = abstracting_value value1 ty1 in
        let (nup2, ienv2, lnamectx2) = abstracting_value value2 ty2 in
        ( Pair (nup1, nup2),
          Util.Pmap.concat ienv1 ienv2,
          Util.Pmap.concat lnamectx1 lnamectx2 )
    | (_, TId _) ->
        let pn = Names.fresh_pname () in
        let nval = Syntax.force_negative_val value in
        let ienv = Util.Pmap.singleton (pn, nval) in
        let nty = Types.force_negative_type ty in
        let lnamectx = Util.Pmap.singleton (pn, nty) in
        (Name pn, ienv, lnamectx)
    | (Name _, TName _) -> (value, Util.Pmap.empty, Util.Pmap.empty)
    | (Constructor _, TExn) -> (value, Util.Pmap.empty, Util.Pmap.empty)
    | _ ->
        failwith
          ("Error: " ^ string_of_term value ^ " of type " ^ string_of_typ ty
         ^ " cannot be abstracted because it is not a value.")

  let subst_names ienv nup =
    let aux nup (nn, nval) =
      Syntax.subst nup (Name nn) (embed_negative_val nval) in
    Util.Pmap.fold aux nup ienv
end
