(** [Nup] contains the infrastructure needed to implement the
    {!module-type: Lang.Abstract_val.AVAL} signature. *)

open Syntax

(** Abstract values (nups) are patterns whose leaves are ground values or names:
    [ABound] names are introduced by the move, at their level in its local
    typing context Δ; [AFree] names are reused from the ambient one. Locations
    are mapped to their name in the disclosed store context, bound the first time
    they are introduced. *)
type nup =
  | AUnit
  | AInt of int
  | ABool of bool
  | APair of nup * nup
  | ARecord of (Syntax.id, nup) Util.Pmap.pmap
  | ACons of Syntax.constructor * nup
  | ASymb of Symbolic.symbolic_expr
  | AFree of Names.name
  | ABound of Names.name
  | ALocFree of Names.LocNames.name
  | ALocBound of Names.LocNames.name

(** The {!module-type: GENERATE_VALUE} signature is implemented by modules
    providing a strategy to generate {e RefML} values. Such modules are used to
    instanciate the {!module: Make} functor. *)
module type GENERATE_VALUE = sig
  module BranchMonad : Util.Monad.BRANCH

  val generate_bool :
    Store.Storectx.t -> (value * Store.Storectx.t) BranchMonad.m
end

(** Currently, two strategies are provided:
    - {!module: MakeGenerateConcreteValue}, which generates concrete values ;
    - {!module: MakeGenerateSymbolicValue}, which generates symbolic variables
      instead of concrete booleans and adds these variables to the [storectx].
*)

module MakeGenerateSymbolicValue (BranchMonad : Util.Monad.BRANCH) = struct
  module BranchMonad = BranchMonad

  let generate_bool (loc_ctx, symbolic_ctx, cons_ctx) =
    let (id, symbolic_ctx') = Symbolic.unconstrained symbolic_ctx in
    let storectx' = (loc_ctx, symbolic_ctx', cons_ctx) in
    let value = Symbolic (Symbolic.Kvar id) in
    BranchMonad.return (value, storectx')
end

module MakeGenerateConcreteValue (BranchMonad : Util.Monad.BRANCH) = struct
  module BranchMonad = BranchMonad

  let generate_bool storectx =
    BranchMonad.para_list [ (Bool true, storectx); (Bool false, storectx) ]
end

module Make
    (BranchMonad : Util.Monad.BRANCH)
    (GenerateValue : GENERATE_VALUE with module BranchMonad = BranchMonad) :
  Lang.Abstract_val.AVAL
    with type name = Names.name
     and type interactive_env = Ienv.IEnv.t
     and type label = Store.label
     and type name_ctx = Namectx.Namectx.t
     and type negative_type = Types.negative_type
     and type negative_val = Syntax.negative_val
     and type renaming = Renaming.Renaming.t
     and type store_ctx = Store.Storectx.t
     and type store = Store.store
     and type typ = Types.typ
     and type value = Syntax.value
     and type abstract_val = nup
     and module BranchMonad = BranchMonad = struct
  (* Instantiation *)
  module BranchMonad = BranchMonad

  type name = Names.name
  type renaming = Renaming.Renaming.t
  type label = Store.label
  type value = Syntax.value
  type negative_val = Syntax.negative_val
  type typ = Types.typ
  type negative_type = Types.negative_type
  type store_ctx = Store.Storectx.t
  type store = Store.store
  type name_ctx = Namectx.Namectx.t

  (* *)
  open Types

  type interactive_env = Ienv.IEnv.t
  type abstract_val = nup

  let rec pp_abstract_val_in ~pp_free_name ~pp_bound_name fmt nup =
    let pp_nup = pp_abstract_val_in ~pp_free_name ~pp_bound_name in
    match nup with
    | AUnit -> Format.pp_print_string fmt "()"
    | AInt n -> Format.pp_print_int fmt n
    | ABool b -> Format.pp_print_bool fmt b
    | ASymb sexpr -> Symbolic.pp_constraint fmt sexpr
    | APair (nup1, nup2) -> Format.fprintf fmt "(%a,%a)" pp_nup nup1 pp_nup nup2
    | ACons (c, nup') ->
        Format.fprintf fmt "%a %a" Syntax.pp_constructor c pp_nup nup'
    | ARecord fields ->
        Format.pp_print_string fmt "{ ";
        Util.Pmap.iter
          (fun (id, nup') -> Format.fprintf fmt "%s = %a; " id pp_nup nup')
          fields;
        Format.pp_print_string fmt "}"
    | AFree nn -> pp_free_name fmt nn
    | ABound nn -> pp_bound_name fmt nn
    | ALocFree loc_name | ALocBound loc_name ->
        Names.LocNames.pp_name fmt loc_name

  let pp_abstract_val =
    pp_abstract_val_in ~pp_free_name:Names.pp_name ~pp_bound_name:Names.pp_name

  let string_of_abstract_val = Format.asprintf "%a" pp_abstract_val
  let abstract_val_to_yojson aval = `String (string_of_abstract_val aval)

  let rec add_names lnames = function
    | AUnit | AInt _ | ABool _ | ASymb _ | ALocFree _ | ALocBound _ -> lnames
    | APair (nup1, nup2) -> add_names (add_names lnames nup1) nup2
    | ACons (_, nup') -> add_names lnames nup'
    | ARecord fields ->
        Util.Pmap.fold
          (fun lnames' (_, nup') -> add_names lnames' nup')
          lnames fields
    | AFree nn | ABound nn ->
        if List.mem nn lnames then lnames else nn :: lnames

  let names_of_abstract_val = add_names []

  let rec fold_free_names_of_abstract_val f acc = function
    | AUnit | AInt _ | ABool _ | ASymb _ | ABound _ | ALocFree _ | ALocBound _
      ->
        acc
    | AFree nn -> f acc nn
    | APair (nup1, nup2) ->
        fold_free_names_of_abstract_val f
          (fold_free_names_of_abstract_val f acc nup1)
          nup2
    | ACons (_, nup') -> fold_free_names_of_abstract_val f acc nup'
    | ARecord fields ->
        Util.Pmap.fold
          (fun acc' (_, nup') -> fold_free_names_of_abstract_val f acc' nup')
          acc fields

  let rec map_free_names_of_abstract_val f = function
    | (AUnit | AInt _ | ABool _ | ASymb _ | ABound _ | ALocFree _ | ALocBound _)
      as nup ->
        nup
    | AFree nn -> AFree (f nn)
    | APair (nup1, nup2) ->
        APair
          ( map_free_names_of_abstract_val f nup1,
            map_free_names_of_abstract_val f nup2 )
    | ACons (c, nup') -> ACons (c, map_free_names_of_abstract_val f nup')
    | ARecord fields ->
        ARecord (Util.Pmap.map_im (map_free_names_of_abstract_val f) fields)

  let rec add_labels label_l = function
    | AUnit | AInt _ | ABool _ | ASymb _ | AFree _ | ABound _ | ALocFree _ ->
        label_l
    | ALocBound loc_name -> Store.LocL loc_name :: label_l
    | ACons (c, _) ->
        if List.mem (Store.ConsL c) label_l then label_l
        else Store.ConsL c :: label_l
    | APair (nup1, nup2) -> add_labels (add_labels label_l nup1) nup2
    | ARecord fields ->
        Util.Pmap.fold
          (fun label_l' (_, nup') -> add_labels label_l' nup')
          label_l fields

  let labels_of_abstract_val = add_labels []

  let rec rename nup renam =
    match nup with
    | AUnit | AInt _ | ABool _ | ASymb _ | AFree _ | ALocFree _ | ALocBound _ ->
        nup
    | APair (nup1, nup2) -> APair (rename nup1 renam, rename nup2 renam)
    | ACons (c, nup') -> ACons (c, rename nup' renam)
    | ARecord fields ->
        ARecord (Util.Pmap.map_im (fun nup' -> rename nup' renam) fields)
    | ABound nn -> AFree (Renaming.Renaming.lookup renam nn)

  let rec is_equiv_abstract_val store1 store2 nup1 nup2 =
    match (nup1, nup2) with
    | (ASymb expr1, ASymb expr2) ->
        Symbolic.equiv_under store1.Store.symbolic_ctx store2.Store.symbolic_ctx
          expr1 expr2
    | (ASymb expr, ABool b) ->
        Symbolic.equiv_under store1.Store.symbolic_ctx store2.Store.symbolic_ctx
          expr (Symbolic.Kbool b)
    | (ABool b, ASymb expr) ->
        Symbolic.equiv_under store1.Store.symbolic_ctx store2.Store.symbolic_ctx
          (Symbolic.Kbool b) expr
    | (APair (nup11, nup12), APair (nup21, nup22)) ->
        is_equiv_abstract_val store1 store2 nup11 nup21
        && is_equiv_abstract_val store1 store2 nup12 nup22
    | (ACons (c1, nup1'), ACons (c2, nup2')) ->
        c1 = c2 && is_equiv_abstract_val store1 store2 nup1' nup2'
    | (ARecord fields1, ARecord fields2) ->
        let fields1 = Util.Pmap.to_list fields1 in
        let fields2 = Util.Pmap.to_list fields2 in
        List.length fields1 = List.length fields2
        && List.for_all2
             (fun (label1, nup1') (label2, nup2') ->
               label1 = label2 && is_equiv_abstract_val store1 store2 nup1' nup2')
             fields1 fields2
    | _ -> nup1 = nup2

  (* The following function is used to generate the nups associated to a given type.
      It takes as input a store context Σ, a name context Γ and a type τ, and
      generates all nups A and name context Δ such that
      - Σ;Γ ⊢ A : τ ▷ Δ (as a nup)
  *)

  let generate_abstract_val ((_, _, cons_ctx) as storectx) lnamectx namectx ty =
    let open BranchMonad in
    let rec aux ((storectx, lnamectx) as res) = function
      | TUnit -> return (AUnit, res)
      | TBool ->
          let* (value, storectx') = GenerateValue.generate_bool storectx in
          let nup =
            match value with
            | Bool b -> ABool b
            | Symbolic sexpr -> ASymb sexpr
            | _ ->
                failwith
                  "Error: the generated boolean is neither a boolean nor a \
                   symbolic value. Please report." in
          return (nup, (storectx', lnamectx))
      | TInt ->
          let* i = BranchMonad.pick_int () in
          return (AInt i, res)
      | TProd (ty1, ty2) ->
          let* (nup1, res) = aux res ty1 in
          let* (nup2, res) = aux res ty2 in
          return (APair (nup1, nup2), res)
      (* A disclosed location of that type, or the location name fresh for
         Σ. *)
      | TRef ty ->
          let (loc_ctx, symbolic_ctx, cons_ctx) = storectx in
          let free_loc_names =
            List.filter
              (fun loc_name -> Store.LocCtx.lookup_exn loc_ctx loc_name = ty)
              (Store.LocCtx.get_names loc_ctx) in
          let (loc_name, loc_ctx') = Store.LocCtx.add_fresh loc_ctx "" ty in
          para_list
            (List.map (fun loc_name -> (ALocFree loc_name, res)) free_loc_names
            @ [
                ( ALocBound loc_name,
                  ((loc_ctx', symbolic_ctx, cons_ctx), lnamectx) );
              ])
      | TSum _ ->
          failwith "Need to add injection to the syntax of expressions"
          (*
    let lnup1 = generate_nup ty1 in
    let lnup1' = List.map (fun (nup,nctx) -> (Inj (1,nup),nctx)) lnup1 in
    let lnup2' = List.map (fun (nup,nctx) -> (Inj (2,nup),nctx)) lnup1 in
    lnup1'@lnup2' *)
      | TArrow _ as ty ->
          let nty = Types.force_negative_type ty in
          let (fn, lnamectx') = Namectx.Namectx.add_fresh lnamectx "" nty in
          return (ABound fn, (storectx, lnamectx'))
      | TName tn as ty when Namectx.has_type_name namectx tn ->
          let namectxP_pmap = Namectx.Namectx.to_pmap namectx in
          let pn_list = Util.Pmap.select_im ty namectxP_pmap in
          let* pn = para_list @@ pn_list in
          let* _ =
            return @@ Util.Debug.print_debug @@ "Reusing the pname "
            ^ Names.string_of_name pn ^ " from the namectx "
            ^ Namectx.Namectx.to_string namectx in
          return (AFree pn, res)
      | TName _ as ty ->
          let nty = Types.force_negative_type ty in
          let (pn, lnamectx') = Namectx.Namectx.add_fresh lnamectx "" nty in
          Util.Debug.print_debug @@ "Creating a fresh pname "
          ^ Names.string_of_name pn ^ " and putting it in the name context "
          ^ Namectx.Namectx.to_string lnamectx';
          return (ABound pn, (storectx, lnamectx'))
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
          begin match cons_ty with
          | TArrow (pty, _) ->
              let* (nup, res) = aux res pty in
              return (ACons (c, nup), res)
          | _ -> failwith "TODO"
          end
      | TRecord fields ->
          let instantiate_field m (field_name, ty) =
            let* (current_fields, current_res) = m in
            let* (nup, new_res) = aux current_res ty in
            let new_fields = Util.Pmap.add (field_name, nup) current_fields in
            return (new_fields, new_res) in
          let* (instance_fields, new_res) =
            Util.Pmap.fold instantiate_field
              (return (Util.Pmap.empty, res))
              fields in
          return (ARecord instance_fields, new_res)
      | ty ->
          failwith
            ("Error generating a nup on type " ^ Types.string_of_typ ty
           ^ ". Please report") in
    aux (storectx, lnamectx) ty

  let type_check_abstract_val storectx lnamectx namectxP namectxO ty nup =
    let has_type namectx nn ty =
      Util.Pmap.lookup nn (Namectx.Namectx.to_pmap namectx) = Some ty in
    let is_next_name (storectx, lnamectx) nn nty =
      let (nn', lnamectx') = Namectx.Namectx.add_fresh lnamectx "" nty in
      if nn = nn' then Some (storectx, lnamectx') else None in
    let rec aux (((loc_ctx, symbolic_ctx, cons_ctx), lnamectx) as res) ty nup =
      let open Util.Monad.Option in
      match (ty, nup) with
      | (TUnit, AUnit) -> Some res
      | (TUnit, _) -> None
      | (TBool, ABool _) -> Some res
      | (TBool, _) -> None
      | (TInt, AInt _) -> Some res
      | (TInt, _) -> None
      | (TProd (ty1, ty2), APair (nup1, nup2)) -> begin
          let* res' = aux res ty1 nup1 in
          aux res' ty2 nup2
        end
      | (TProd _, _) -> None
      | (TRecord ty_fields, ARecord val_fields) ->
          let check_on_field res_m (field_name, ty) =
            let* current_res = res_m in
            let associated_val = Util.Pmap.lookup_exn field_name val_fields in
            aux current_res ty associated_val in
          Util.Pmap.fold check_on_field (Some res) ty_fields
      | (TRecord _, _) -> None
      | (TArrow _, ABound nn) | (TForall _, ABound nn) ->
          is_next_name res nn (Types.force_negative_type ty)
      | (TArrow _, _) | (TForall _, _) -> None
      | (TName tn, AFree nn) ->
          let owner =
            if Namectx.has_type_name namectxP tn then Some namectxP
            else if Namectx.has_type_name namectxO tn then Some namectxO
            else None in
          if Option.fold ~none:false ~some:(fun ctx -> has_type ctx nn ty) owner
          then Some res
          else None
      | (TName tn, ABound nn) ->
          if Namectx.has_type_name namectxO tn || Namectx.has_type_name lnamectx tn
          then is_next_name res nn (Types.force_negative_type ty)
          else None
      (* | (TExn, ACons (c, nup')) ->
        let (TArrow (param_ty, _)) = Util.Pmap.lookup_exn c (Util.Pmap.concat namectxP namectxO) in
        type_check_abstract_val namectxP namectxO param_ty nup' *)
      | (TName _, _) -> None
      | (TRef ty', ALocFree loc_name) ->
          if
            List.mem loc_name (Store.LocCtx.get_names loc_ctx)
            && Store.LocCtx.lookup_exn loc_ctx loc_name = ty'
          then Some res
          else None
      | (TRef ty', ALocBound loc_name) ->
          let (loc_name', loc_ctx') = Store.LocCtx.add_fresh loc_ctx "" ty' in
          if loc_name = loc_name' then
            Some ((loc_ctx', symbolic_ctx, cons_ctx), lnamectx)
          else None
      | (TRef _, _) -> None
      | (TVar _, _) ->
          failwith @@ "Error: trying to type-check a nup of type "
          ^ Types.string_of_typ ty ^ ". Please report."
      | (TUndef, _) | (TSum _, _) | (TExn, _) | (TypeUniverse, _) ->
          failwith @@ "Error: type-checking a nup of type "
          ^ Types.string_of_typ ty ^ " is not yet supported."
      | (TAlgebraic _, _) ->
          failwith
            "Algebraic type are not yet supported (type_check_abstract_val)"
    in
    aux (storectx, lnamectx) ty nup

  (* Exception payloads kept in abstract values must be ground. *)
  let rec nup_of_ground_value value =
    match value with
    | Unit -> AUnit
    | Int n -> AInt n
    | Bool b -> ABool b
    | Symbolic sexpr -> ASymb sexpr
    | Pair (value1, value2) ->
        APair (nup_of_ground_value value1, nup_of_ground_value value2)
    | Constructor (c, Some value') -> ACons (c, nup_of_ground_value value')
    | Constructor (_, None) ->
        failwith "Empty constructor not implemented yet (nup_of_ground_value)"
    | Record fields -> ARecord (Util.Pmap.map_im nup_of_ground_value fields)
    | _ ->
        failwith
          ("Error: the value " ^ string_of_term value
         ^ " is not ground, it cannot be kept in an abstract value. Please \
            report.")

  (* abstracting_value_from (γ, µ) V τ abstracts V at τ extending the pair
     given, so that the values of one move share its Δ. *)
  let rec abstracting_value_from (ienv, store) (value : value) ty =
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
        (ABound fn, (ienv', store))
      end
    | (Unit, TUnit) -> (AUnit, (ienv, store))
    | (Bool b, TBool) -> (ABool b, (ienv, store))
    | (Int n, TInt) -> (AInt n, (ienv, store))
    (* Symbolic expressions are treated as values *)
    | (Symbolic sexpr, _) -> (ASymb sexpr, (ienv, store))
    | (Pair (value1, value2), TProd (ty1, ty2)) ->
        let (nup1, res1) =
          abstracting_value_from (ienv, store) value1 ty1 in
        let (nup2, res2) = abstracting_value_from res1 value2 ty2 in
        (APair (nup1, nup2), res2)
    (* An Opponent polymorphic name is not refreshed. *)
    | (Name nn, TName tn) when Namectx.has_type_name (Ienv.IEnv.im ienv) tn ->
        (AFree nn, (ienv, store))
    (* A value at a Player type name is boxed. *)
    | (_, TName _) -> begin
        let nval = Syntax.force_negative_val value in
        let nty = Types.force_negative_type ty in
        let (pn, ienv') = Ienv.IEnv.add_fresh ienv "" nty nval in
        (ABound pn, (ienv', store))
      end
    | (Loc loc, TRef ty') -> begin
        match Store.loc_name_of_loc store loc with
        | Some loc_name -> (ALocFree loc_name, (ienv, store))
        | None ->
            let (loc_name, store') = Store.disclose_loc store loc ty' in
            (ALocBound loc_name, (ienv, store'))
      end
    | (Constructor (c, Some value'), TExn) ->
        (ACons (c, nup_of_ground_value value'), (ienv, store))
    | (Record val_fields, TRecord ty_fields) ->
        let abstracting_field (new_fields, current_res) (field_name, expr) =
          let associated_ty = Util.Pmap.lookup_exn field_name ty_fields in
          let (nup, res') =
            abstracting_value_from current_res expr associated_ty in
          (Util.Pmap.add (field_name, nup) new_fields, res') in
        let (new_fields, res') =
          Util.Pmap.fold abstracting_field
            (Util.Pmap.empty, (ienv, store))
            val_fields in
        (ARecord new_fields, res')
    | _ ->
        failwith
          ("Error: " ^ string_of_term value ^ " of type " ^ string_of_typ ty
         ^ " cannot be abstracted because it is not a value.")

  let abstracting_value value namectxO store ty =
    let (nup, (ienv, store')) =
      abstracting_value_from (Ienv.IEnv.empty namectxO, store) value ty
    in
    (nup, ienv, store')

  let loc_of_loc_name_exn store loc_name =
    match Store.loc_of_loc_name store loc_name with
    | Some loc -> loc
    | None ->
        failwith
          ("Error: the location name "
          ^ Names.LocNames.string_of_name loc_name
          ^ " is not disclosed in the store. Please report.")

  let rec abstracting_store ienv store values =
    let loc_ctx = store.Store.loc_ctx in
    let loc_names = Store.LocCtx.get_names loc_ctx in
    match List.nth_opt loc_names (List.length values) with
    | None -> (values, ienv, store)
    | Some loc_name ->
        let loc = loc_of_loc_name_exn store loc_name in
        let value =
          match Store.loc_lookup store loc with
          | Some value -> value
          | None ->
              failwith
                ("Error: the disclosed location " ^ Syntax.string_of_loc loc
               ^ " is not in the heap. Please report.") in
        let (nup, (ienv', store')) =
          abstracting_value_from (ienv, store) value
            (Store.LocCtx.lookup_exn loc_ctx loc_name) in
        abstracting_store ienv' store' (values @ [ nup ])

  let rec value_of_ground_nup = function
    | AUnit -> Unit
    | AInt n -> Int n
    | ABool b -> Bool b
    | ASymb sexpr -> Symbolic sexpr
    | APair (nup1, nup2) -> Pair (value_of_ground_nup nup1, value_of_ground_nup nup2)
    | ACons (c, nup') -> Constructor (c, Some (value_of_ground_nup nup'))
    | ARecord fields -> Record (Util.Pmap.map_im value_of_ground_nup fields)
    | AFree nn | ABound nn ->
        failwith
          ("Error: the name " ^ Names.string_of_name nn
         ^ " is not part of a ground abstract value. Please report.")
    | ALocFree loc_name | ALocBound loc_name ->
        failwith
          ("Error: the location name "
          ^ Names.LocNames.string_of_name loc_name
          ^ " is not part of a ground abstract value. Please report.")

  (* Instantiating Proponent polymorphic names, guided by the type. The bound
     location names are already allocated in the store. *)
  let subst_pnames ienv store ty nup =
    let namectxP = Ienv.IEnv.dom ienv in
    let rec aux ty nup =
      match (ty, nup) with
      | (TProd (ty1, ty2), APair (nup1, nup2)) ->
          Pair (aux ty1 nup1, aux ty2 nup2)
      | (TRecord ty_fields, ARecord fields) ->
          let subst_field (field, nup') =
            (field, aux (Util.Pmap.lookup_exn field ty_fields) nup') in
          Record (Util.Pmap.map subst_field fields)
      | (TName tn, AFree nn) when Namectx.has_type_name namectxP tn ->
          embed_negative_val (Ienv.IEnv.lookup_exn ienv nn)
      | (_, AFree nn) -> Name nn
      | (TRef _, (ALocFree loc_name | ALocBound loc_name)) ->
          Loc (loc_of_loc_name_exn store loc_name)
      | (_, ABound nn) ->
          failwith
            ("Error: the name " ^ Names.string_of_name nn
           ^ " of an abstract value has not been instantiated. Please report.")
      | ( _,
          ( AUnit | AInt _ | ABool _ | ASymb _ | ACons _ | APair _ | ARecord _
          | ALocFree _ | ALocBound _ ) ) ->
          value_of_ground_nup nup in
    aux ty nup

  (* A location name the move binds has no location yet.
      One is allocated for it, holding () until concretize_store writes its value. *)
  let allocate_disclosed_locs store loc_ctx =
    let allocate store loc_name =
      match Store.loc_of_loc_name store loc_name with
      | Some _ -> store
      | None ->
          let ty = Store.LocCtx.lookup_exn loc_ctx loc_name in
          let (loc, store') = Store.loc_allocate store Unit in
          let (loc_name', store'') = Store.disclose_loc store' loc ty in
          if loc_name' <> loc_name then
            failwith
              ("Error: the bound location name "
              ^ Names.LocNames.string_of_name loc_name
              ^ " is not the one fresh for the store. Please report.");
          store'' in
    List.fold_left allocate store (Store.LocCtx.get_names loc_ctx)

  let concretize_store ienv store storectx values =
    let loc_ctx = Store.loc_ctx storectx in
    let store' = allocate_disclosed_locs store loc_ctx in
    let concretize_value heap loc_name nup =
      let ty = Store.LocCtx.lookup_exn loc_ctx loc_name in
      let value = subst_pnames ienv store' ty nup in
      Heap.modify heap (loc_of_loc_name_exn store' loc_name) value in
    let heap =
      List.fold_left2 concretize_value Heap.emptyheap
        (Store.LocCtx.get_names loc_ctx)
        values in
    Store.update_store store' { Store.empty_store with heap }
end
