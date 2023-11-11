(* Instantiation *)
type name = Syntax.name
type value = Syntax.valML
type typ = Types.typeML
(* *)
type name_ctx = (name,typ) Util.Pmap.pmap
type val_env = (name,value) Util.Pmap.pmap

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

let max_int = 2

(* The following function is used to generate the nups associated to a given type.
   We also provide a typing context that is used to retrieve the polynorphic names of
   a given type
*)
let rec generate_nup namectxP = function
  | TUnit -> [ (Unit, Type_ctx.empty_name_ctx) ]
  | TBool ->
      [
        (Bool true, Type_ctx.empty_name_ctx);
        (Bool false, Type_ctx.empty_name_ctx);
      ]
  | TInt ->
      let rec aux i =
        if i < 0 then [] else (Int i, Type_ctx.empty_name_ctx) :: aux (i - 1)
      in
      aux max_int
  | TProd (ty1, ty2) ->
      let nups1 = generate_nup namectxP ty1 in
      let nups2 = generate_nup namectxP ty2 in
      let aux (nup1, nctx1) =
        List.map
          (fun (nup2, nctx2) ->
            (Pair (nup1, nup2), Util.Pmap.concat nctx1 nctx2))
          nups2 in
      List.flatten (List.map aux nups1)
  | TSum _ ->
      failwith "Need to add injection to the syntax of expressions"
      (*
    let lnup1 = generate_nup ty1 in
    let lnup1' = List.map (fun (nup,nctx) -> (Inj (1,nup),nctx)) lnup1 in
    let lnup2' = List.map (fun (nup,nctx) -> (Inj (2,nup),nctx)) lnup1 in
    lnup1'@lnup2' *)
  | TArrow _ as ty ->
      let fn = fresh_fname () in
      [ (Name fn, Util.Pmap.singleton (fn, ty)) ]
  | TNeg _ as ty ->
      let cn = Syntax.cname_of_id @@ fresh_cname () in
      [ (Name cn, Util.Pmap.singleton (cn, ty)) ]
  | TId _ as ty ->
      let pn_list = Util.Pmap.select_im ty namectxP in
      List.map (fun pn -> (Name pn, Type_ctx.empty_name_ctx)) pn_list
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
  | (TArrow _, Name nn) | (TForall _, Name nn) | (TNeg _, Name nn) ->
      if Util.Pmap.mem nn namectxP || Util.Pmap.mem nn namectxO then None
        (* the name nn has to be fresh to be well-typed *)
      else Some (Util.Pmap.singleton (nn, ty))
  | (TArrow _, _) | (TForall _, _) | (TNeg _, _) -> None
  | (TId id, Name nn) -> begin
      match Util.Pmap.lookup nn namectxP with
      | None -> None
      | Some (TId id') when id = id' -> Some Type_ctx.empty_name_ctx
      | Some _ -> None
    end
  | (TVar _, Name nn) ->
      if Util.Pmap.mem nn namectxP || Util.Pmap.mem nn namectxO then None
        (* the name nn has to be fresh to be well-typed *)
      else Some (Util.Pmap.singleton (nn, ty))
  | (TVar _, _) | (TId _, _) | (TUndef, _) | (TRef _, _) | (TSum _, _) ->
      failwith "not yet implemented"

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
  | _ ->
      failwith
        ("Error: " ^ string_of_exprML value ^ " of type " ^ string_of_typeML ty
       ^ " cannot be abstracted because it is not a value.")

let incorporate_name (nup, n) = Pair (nup, Name n)

let subst_names_of_nup val_env nup =
  let aux nup (nn, value) = Syntax.subst nup (Name nn) value in
  Util.Pmap.fold aux nup val_env
