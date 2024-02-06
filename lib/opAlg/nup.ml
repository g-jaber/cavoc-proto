module Make (M : Util.Monad.BRANCH) :
  Langop.Abstract_val.AVAL_INOUT
    with type name = Names.name
     and type value = Syntax.value
     and type negative_val = Syntax.negative_val
     and type vtyp = Types.vtyp
     and type ctyp = Types.ctyp
     and type negative_type = Types.negative_type
     and type typevar = Types.typevar
     and type interactive_env = Syntax.interactive_env
     and type name_ctx = Types.name_ctx
     and module M = M = struct
  (* Instantiation *)
  type name = Names.name
  type label = Syntax.label
  type value = Syntax.value
  type negative_val = Syntax.negative_val
  type vtyp = Types.vtyp
  type ctyp = Types.ctyp
  type negative_type = Types.negative_type
  type typevar = Types.typevar
  (* *)

  open Syntax
  open Types

  type name_ctx = (name, negative_type) Util.Pmap.pmap
  type interactive_env = Syntax.interactive_env
  type abstract_val = Syntax.value

  let string_of_abstract_val = Syntax.string_of_value
  let names_of_abstract_val = Syntax.get_names
  (*let labels_of_abstract_val = Syntax.get_labels *)

  let rec unify_abstract_val nspan nup1 nup2 =
    match (nup1, nup2) with
    | (Name n1, Name n2) -> Util.Namespan.add_nspan (n1, n2) nspan
    | (Unit, Unit) -> Some nspan
    | (Bool b1, Bool b2) -> if b1 = b2 then Some nspan else None
    | (Constructor (cons, lnup1), Constructor (cons', lnup2)) when cons=cons' -> 
        List.fold_left2 
         (fun acc nup1 nup2 -> 
          match acc with 
           | Some nspan' -> unify_abstract_val nspan' nup1 nup2
           | None -> None)  
        (Some nspan) lnup1 lnup2
     | (Record lab_nups, Record lab_nups') -> 
         List.fold_left2
         (fun acc (lab, nup) (lab', nup') -> 
          match acc with 
           | Some nspan' -> 
              if lab=lab' then unify_abstract_val nspan' nup nup' 
              else None
           | None -> None) (Some nspan) lab_nups lab_nups'   
     | (Variant (lab, nup), Variant (lab', nup')) when lab=lab' -> 
      unify_abstract_val nspan nup nup'
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

  let rec generate_abstract_val namectxP = function
    | TUnit -> return (Unit, Util.Pmap.empty)
    | TBool ->
        let* b = para_list @@ [ true; false ] in
        return (Bool b, Util.Pmap.empty)
    | TInt ->
        let* i = M.pick_int () in
        return (Int i, Util.Pmap.empty)
    (*| TProd (ty1, ty2) ->
        let* (nup1, nctx1) = generate_abstract_val storectx namectxP ty1 in
        let* (nup2, nctx2) = generate_abstract_val storectx namectxP ty2 in
        return (Pair (nup1, nup2), Util.Pmap.concat nctx1 nctx2) *)
    | TArrow _ as ty ->
        let fn = Names.fresh_fname () in
        let nty = Types.force_negative_type ty in
        return (Name fn, Util.Pmap.add (fn, nty) Util.Pmap.empty)
    | TRecord labelled_typs -> 
        let* (labelled_nups, gamma) = 
        List.fold_left 
          (fun acc (lab, ty) -> 
            let* (lab_nups, g) = acc in
            let* (nup, g') = 
             generate_abstract_val namectxP ty in 
             return (lab_nups @ [(lab, nup)],
             Util.Pmap.concat g g')) 
           (return ([], Util.Pmap.empty)) labelled_typs in 
          return (Record labelled_nups, gamma)
    | TVariant labelled_typs -> 
        let* (lab, ty) = para_list labelled_typs in 
        let* (nup, gamma) = 
         generate_abstract_val namectxP ty in 
         return (Variant (lab, nup), gamma)
    | TId _ as ty ->
        let pn_list = Util.Pmap.select_im ty namectxP in
        let* pn = para_list @@ pn_list in
        return (Name pn, Util.Pmap.empty)
    | TName _ as ty ->
        let pn = Names.fresh_pname () in
        let nty = Types.force_negative_type ty in
        return (Name pn, Util.Pmap.add (pn, nty) Util.Pmap.empty)
    | ty ->
        failwith
          ("Error generating a nup on type " ^ Types.string_of_vtyp ty
         ^ ". Please report")

  let rec type_check_abstract_val namectxP namectxO ty nup =
    match (ty, nup) with
    | (TUnit, Unit) -> Some Util.Pmap.empty
    | (TUnit, _) -> None
    | (TBool, Bool _) -> Some Util.Pmap.empty
    | (TBool, _) -> None
    | (TInt, Int _) -> Some Util.Pmap.empty
    | (TInt, _) -> None
    (*| (TProd (ty1, ty2), Pair (nup1, nup2)) -> begin
        match
          ( type_check_abstract_val namectxP namectxO ty1 nup1,
            type_check_abstract_val namectxP namectxO ty2 nup2 )
        with
        | (None, _) | (_, None) -> None
        | (Some namectxO1, Some namectxO2) ->
            if Util.Pmap.disjoint namectxO1 namectxO2 then
              Some (Util.Pmap.concat namectxO1 namectxO2)
            else None
      end*)
    | (TProd _, _) -> None
    | (TArrow _, Name nn) | (TForall _, Name nn) ->
        if Util.Pmap.mem nn namectxP || Util.Pmap.mem nn namectxO then None
          (* the name nn has to be fresh to be well-typed *)
        else
          let nty = Types.force_negative_type ty in
          Some (Util.Pmap.add (nn, nty) Util.Pmap.empty)
    | (TArrow _, _) | (TForall _, _) -> None
    | (TVariant lab_vtyp_l, Variant (lab, nup')) -> 
        let (_, vty) = List.find (fun x -> fst x = lab) lab_vtyp_l in 
        type_check_abstract_val namectxP namectxO vty nup'
    | (TRecord lab_vtyp_l, Record lab_nup_l) ->
       List.fold_left2
        (fun acc_namectx (_,vty) (_,nup') -> 
          match type_check_abstract_val namectxP namectxO vty nup' with 
          | None -> acc_namectx
          | Some nctx -> begin 
            match acc_namectx with 
             | None -> None 
             | Some acc -> Some (Util.Pmap.concat nctx acc)
          end)
         None lab_vtyp_l lab_nup_l
    | (TRecord _, _) | (TVariant _, _) -> None
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
          Some (Util.Pmap.add (nn, nty) Util.Pmap.empty)
    (* | (TExn, Constructor (c, nup')) ->  
        let (TArrow (param_ty, _)) = Util.Pmap.lookup_exn c (Util.Pmap.concat namectxP namectxO) in 
        type_check_abstract_val namectxP namectxO param_ty nup' *)
    | (TName _, _) -> None
    | (TVar _, _) ->
        failwith @@ "Error: trying to type-check a nup of type "
        ^ Types.string_of_vtyp ty ^ ". Please report."
    | (TUndef, _)  ->
        failwith @@ "Error: type-checking a nup of type "
        ^ Types.string_of_vtyp ty ^ " is not yet supported."

  let rec abstracting_value (value : value) ty =
    match (value, ty) with
    | (Lambda _, TArrow _) | (Name _, TArrow _) ->
        let fn = Names.fresh_fname () in
        let nval = Syntax.force_negative_val value in
        let nty = Types.force_negative_type ty in
        let ienv = Util.Pmap.add (fn, nval) Util.Pmap.empty in
        let lnamectx = Util.Pmap.add (fn, nty) Util.Pmap.empty in
        (Name fn, ienv, lnamectx)
    | (Unit, TUnit) | (Bool _, TBool) | (Int _, TInt) ->
        (value, Util.Pmap.empty, Util.Pmap.empty)
    (*| (Pair (value1, value2), TProd (ty1, ty2)) ->
        let (nup1, ienv1, lnamectx1) = abstracting_value value1 ty1 in
        let (nup2, ienv2, lnamectx2) = abstracting_value value2 ty2 in
        ( Pair (nup1, nup2),
          Util.Pmap.concat ienv1 ienv2,
          Util.Pmap.concat lnamectx1 lnamectx2 ) *)
    | (Variant (lab, value'), TVariant lab_vtyp_l) -> 
        let (_, vty) = List.find (fun x -> fst x = lab) lab_vtyp_l in 
        let (aval, ienv, lnamectx) = abstracting_value value' vty in
        (Variant (lab, aval), ienv, lnamectx) 
    | (Record lab_val_l, TRecord lab_vtyp_l) ->
      let (lab_aval_l, ienv', lnamectx') = 
       List.fold_left2
        (fun acc_ctx (lab, vty) (_,value') -> 
          let (lab_aval_l, ienv'', lnamectx'') = acc_ctx in
          let (aval, ienv, lnamectx) = abstracting_value value' vty in
          (lab_aval_l @[(lab, aval)],
           Util.Pmap.concat ienv ienv'',
           Util.Pmap.concat lnamectx lnamectx''))
          ([], Util.Pmap.empty, Util.Pmap.empty) 
          lab_vtyp_l lab_val_l in 
         (Record lab_aval_l, ienv', lnamectx')
    | (_, TId _) ->
        let pn = Names.fresh_pname () in
        let nval = Syntax.force_negative_val value in
        let ienv = Util.Pmap.add (pn, nval) Util.Pmap.empty in
        let nty = Types.force_negative_type ty in
        let lnamectx = Util.Pmap.add (pn, nty) Util.Pmap.empty in
        (Name pn, ienv, lnamectx)
    | (Name _, TName _) -> (value, Util.Pmap.empty, Util.Pmap.empty)
    (*| (Constructor , TExn) -> (value, Util.Pmap.empty, Util.Pmap.empty) *)
    | _ ->
        failwith
          ("Error: " ^ string_of_value value ^ " of type " ^ string_of_vtyp ty
         ^ " cannot be abstracted because it is not a value.")
     (* TODO: add cons_ctx as param and implement case (Constructor _, ty )*)

  let subst_names ienv nup =
    let aux nup (nn, nval) =
      Syntax.subst_val_in_val nup (Name nn) (Syntax.embed_negative_val nval) in
    Util.Pmap.fold aux nup ienv

  let get_input_type = function
    | Types.TArrow (ty1, _) -> ([], ty1)
    | Types.TForall (tvar_l, TArrow (ty1, _)) -> (tvar_l, ty1)
    | ty ->
        failwith @@ "Error: the type " ^ Types.string_of_vtyp ty
        ^ " is not a negative type. Please report."

  let get_output_type = function
    | Types.TArrow (_, TComp(ty2,_)) -> ty2
    | Types.TForall (_, TArrow (_, TComp(ty2, _))) -> ty2
    | ty ->
        failwith @@ "Error: the type " ^ Types.string_of_vtyp ty
        ^ " is not a negative type. Please report."
  
end
