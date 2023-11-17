module Make (M : Util.Monad.BRANCH) = struct
  (* Instantiation *)
  type name = Syntax.name
  type value = Syntax.valML
  type typ = Types.typeML

  (* *)
  type name_ctx = (name, typ) Util.Pmap.pmap
  type val_env = (name, value) Util.Pmap.pmap
  type nup = Syntax.exprML

  let string_of_nup = Syntax.string_of_exprML
  let names_of_nup = Syntax.get_names

  open Syntax
  open Types

  let rec unify_nup nspan nup1 nup2 =
    match (nup1, nup2) with
    | (Unit, Unit) -> Some nspan
    | (Bool b1, Bool b2) -> if b1 = b2 then Some nspan else None
    | (Int n1, Int n2) -> if n1 = n2 then Some nspan else None
    | (Pair (nup11, nup12), Pair (nup21, nup22)) ->
        let nspan1_option = unify_nup nspan nup11 nup21 in
        begin
          match nspan1_option with
          | None -> None
          | Some nspan1 -> unify_nup nspan1 nup12 nup22
        end
    | (Name n1, Name n2) -> Util.Namespan.add_nspan (n1, n2) nspan
    | _ ->
        failwith
          ("Error: one of the terms " ^ string_of_nup nup1 ^ " or "
         ^ string_of_nup nup2 ^ " is not a NUP. Please report.")

  (* The following function is used to generate the nups associated to a given type.
     We also provide a typing context that is used to retrieve the polynorphic names of
     a given type
  *)
  module M = M
  open M

  let rec generate_nup namectxP = function
    | TUnit -> return (Unit, Type_ctx.empty_name_ctx)
    | TBool ->
        let* b = para_list @@ [ true; false ] in
        return (Bool b, Type_ctx.empty_name_ctx)
    | TInt ->
        let* i = M.pick_int () in
        return (Int i, Type_ctx.empty_name_ctx)
    | TProd (ty1, ty2) ->
        let* (nup1, nctx1) = generate_nup namectxP ty1 in
        let* (nup2, nctx2) = generate_nup namectxP ty2 in
        return (Pair (nup1, nup2), Util.Pmap.concat nctx1 nctx2)
    | TSum _ ->
        failwith "Need to add injection to the syntax of expressions"
        (*
    let lnup1 = generate_nup ty1 in
    let lnup1' = List.map (fun (nup,nctx) -> (Inj (1,nup),nctx)) lnup1 in
    let lnup2' = List.map (fun (nup,nctx) -> (Inj (2,nup),nctx)) lnup1 in
    lnup1'@lnup2' *)
    | TArrow _ as ty ->
        let fn = fresh_fname () in
        return (Name fn, Util.Pmap.singleton (fn, ty))
    | TId _ as ty ->
        let pn_list = Util.Pmap.select_im ty namectxP in
        let* pn = para_list @@ pn_list in
        return (Name pn, Type_ctx.empty_name_ctx)
    | TName _ as ty ->
        let pn = fresh_pname () in
        return (Name pn, Util.Pmap.singleton (pn, ty))
    | TExn as ty ->
        failwith
          ("Error: the generation of a nups on type "
         ^ Types.string_of_typeML ty ^ " is not yet supported.")
    | ty ->
        failwith
          ("Error generating a nup on type " ^ Types.string_of_typeML ty
         ^ ". Please report")

  let rec type_check_nup namectxP namectxO ty nup =
    match (ty, nup) with
    | (TUnit, Unit) -> Some Type_ctx.empty_name_ctx
    | (TUnit, _) -> None
    | (TBool, Bool _) -> Some Type_ctx.empty_name_ctx
    | (TBool, _) -> None
    | (TInt, Int _) -> Some Type_ctx.empty_name_ctx
    | (TInt, _) -> None
    | (TProd (ty1, ty2), Pair (nup1, nup2)) -> begin
        match
          ( type_check_nup namectxP namectxO ty1 nup1,
            type_check_nup namectxP namectxO ty2 nup2 )
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
        else Some (Util.Pmap.singleton (nn, ty))
    | (TArrow _, _) | (TForall _, _) -> None
    | (TId id, Name nn) -> begin
        match Util.Pmap.lookup nn namectxP with
        | None -> None
        | Some (TId id') when id = id' -> Some Type_ctx.empty_name_ctx
        | Some _ -> None
      end
    | (TId _, _) -> None
    | (TName _, Name nn) ->
        if Util.Pmap.mem nn namectxP || Util.Pmap.mem nn namectxO then None
          (* the name nn has to be fresh to be well-typed *)
        else Some (Util.Pmap.singleton (nn, ty))
    | (TName _, _) -> None
    | (TVar _, _) ->
        failwith @@ "Error: trying to type-check a nup of type "
        ^ Types.string_of_typeML ty ^ ". Please report."
    | (TUndef, _) | (TRef _, _) | (TSum _, _) | (TExn, _) ->
        failwith @@ "Error: type-checking a nup of type "
        ^ Types.string_of_typeML ty ^ " is not yet supported."

  let rec abstract_val (value : valML) ty =
    match (value, ty) with
    | (Fun _, TArrow _) | (Fix _, TArrow _) | (Name _, TArrow _) ->
        let fn = fresh_fname () in
        let ienv = Util.Pmap.singleton (fn, value) in
        let lnamectx = Util.Pmap.singleton (fn, ty) in
        (Name fn, ienv, lnamectx)
    | (Unit, TUnit) | (Bool _, TBool) | (Int _, TInt) ->
        (value, Util.Pmap.empty, Type_ctx.empty_name_ctx)
    | (Pair (value1, value2), TProd (ty1, ty2)) ->
        let (nup1, ienv1, lnamectx1) = abstract_val value1 ty1 in
        let (nup2, ienv2, lnamectx2) = abstract_val value2 ty2 in
        ( Pair (nup1, nup2),
          Util.Pmap.concat ienv1 ienv2,
          Util.Pmap.concat lnamectx1 lnamectx2 )
    | (_, TId _) ->
        let pn = fresh_pname () in
        let ienv = Util.Pmap.singleton (pn, value) in
        let lnamectx = Util.Pmap.singleton (pn, ty) in
        (Name pn, ienv, lnamectx)
    | (Name _, TName _) -> (value, Util.Pmap.empty, Type_ctx.empty_name_ctx)
    | (Constructor _, TExn) -> (value, Util.Pmap.empty, Type_ctx.empty_name_ctx)
    | _ ->
        failwith
          ("Error: " ^ string_of_exprML value ^ " of type "
         ^ string_of_typeML ty
         ^ " cannot be abstracted because it is not a value.")

  let incorporate_name (nup, n) = Pair (nup, Name n)

  let subst_names_of_nup val_env nup =
    let aux nup (nn, value) = Syntax.subst nup (Name nn) value in
    Util.Pmap.fold aux nup val_env
end
