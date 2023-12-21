open Syntax
open Types

type pure_cont = (id * computation) list

type frame = pure_cont * handler

module DelCont = Map.Make(String)

type continuation = frame list

type config = {
  term: computation; 
  del_cont: continuation DelCont.t; 
  cont: continuation; 
  fwd: continuation }

let init_cont = [([], id_handler)]
let inj_config comp = { 
  term = comp; 
  del_cont = DelCont.empty; 
  cont = init_cont; 
  fwd = init_cont }


(* Substitution *)
module VarSubst = Map.Make( 
  struct 
    type t = id
    let compare = String.compare
  end)

let rec subst_var_in_val value id svalue =
  match value with   
   | Var x when x=id -> svalue 
   | Constructor (cons, l_val) -> 
      let l_val' = List.map (fun v -> subst_var_in_val v id svalue) l_val in 
      Constructor(cons, l_val')
   | Binop (op, v1, v2) -> 
    Binop (op, subst_var_in_val v1 id svalue, subst_var_in_val v2 id svalue)
   | Record labelled_vals -> 
     let l = List.map (fun (lab, v) -> (lab, subst_var_in_val v id svalue)) labelled_vals in 
      Record l
   | Variant (lab, v) -> 
      Variant (lab, subst_var_in_val v id svalue)
   | Lambda (abs, comp) -> 
      Lambda (abs, subst_var comp id svalue)
   | _ -> value
  
and subst_var comp id svalue = 
  match comp with   
  | Return v -> Return (subst_var_in_val v id svalue)
  | Let (id, comp1, comp2) -> 
     let comp1' = subst_var comp1 id svalue in 
     let comp2' = subst_var comp2 id svalue in 
     Let (id, comp1', comp2')
  | Match (v, match_cases) -> 
    let v' = subst_var_in_val v id svalue in 
    let mcs = List.map 
     (fun (pat, comp) -> 
       let bound_vars = get_vars_pat pat in 
       match VarSet.mem id bound_vars with 
         | false ->  (pat, subst_var comp id svalue)
         | true -> (pat, comp))
      match_cases in 
    Match (v', mcs)
  | App (f, v) -> 
     let f' = subst_var_in_val f id svalue in  
     let v' = subst_var_in_val v id svalue in  
     App (f', v')
  | Handle (comp, h) -> 
     let comp' = subst_var comp id svalue in 
     let ocs = List.map 
      (fun (op, (id1, id2, comp)) -> (op, (id1, id2, subst_var comp id svalue)))
       h.ocs in 
     let ret = (fst h.ret, subst_var (snd h.ret) id svalue) in 
     Handle (comp', {ret=ret; ocs=ocs})
  | Perform (opsym, v) -> 
     let v' = subst_var_in_val v id svalue in 
     Perform (opsym, v')

let subst_gen comp vsubst = 
  VarSubst.fold 
   (fun id svalue comp -> subst_var comp id svalue) 
    vsubst comp

let rec unify_with_pattern_aux value pat subst = 
  match subst with
  | None -> None
  | Some vsubst -> begin  
     match (value, pat) with 
     | (_, PatVar id') -> Some (VarSubst.add id' value vsubst)
     | (Constructor (cons, lval), PatCons (cons', lpat)) when cons=cons' -> 
         List.fold_left2 
          (fun acc v p -> unify_with_pattern_aux v p acc)
            subst lval lpat 
     | (Name n, PatName n') when n=n' -> subst
     | (Unit, PatUnit) -> subst
     | (Int i, PatInt i') when i=i' -> subst
     | (Bool b, PatBool b') when b=b'-> subst
     | (Record lab_vals, PatRecord lab_pats) -> 
         List.fold_left2
          (fun acc (lab, v) (lab', p) -> 
            if lab=lab' then unify_with_pattern_aux v p acc 
            else None) subst lab_vals lab_pats 
     | (Variant (lab, v), PatVariant (lab', p)) when lab=lab' -> unify_with_pattern_aux v p subst
     | _ -> None
    end

let unify_with_pattern value pat = 
  unify_with_pattern_aux value pat (Some (VarSubst.empty))

(* handler functions *)
let apply_hret hret value =
  subst_var (snd hret) (fst hret) value

let get_clause opsym h = 
  match List.find_opt (fun (op, _) -> op=opsym) h.ocs with
  | None -> None
  | Some clause -> Some (snd clause)


(* Abstract Machine *)
let rec abstract_machine conf =
  match conf.term with 
   | Return value -> begin 
     match conf.cont with 
      | ([], h)::k -> 
         let term = apply_hret h.ret value in
         let cont = k in
         abstract_machine { conf with term; cont }
      | ((id, comp)::pure_k, h)::k -> 
         let term = subst_var comp id value in
         let cont = (pure_k, h)::k in 
         abstract_machine { conf with term; cont }
      | [] -> conf 
     end
   | Let (id, comp1, comp2) -> begin
     match conf.cont with 
      | (pk, h)::k ->  
         abstract_machine {conf with term=comp1; cont=((id, comp2)::pk, h)::k}
      | _ -> conf
      (*| _ -> failwith "The term " 
              ^ string_of_computation conf.term ^ "is not a normal form." *)
     end
   | Match (v, match_cases) -> begin
      let term_opt = List.fold_left 
        (fun acc (pat, comp) -> match acc with 
          | None -> begin 
            match unify_with_pattern v pat with 
              | None -> None
              | Some vsubst -> Some (subst_gen comp vsubst)
            end
          | _ -> acc ) None match_cases in
      match term_opt with 
        | Some term -> 
           abstract_machine { conf with term }
        | None -> conf
     end
   | App (f, v) -> begin
    match f with 
     | Var id -> begin 
        match DelCont.find_opt id conf.del_cont with 
          | Some k -> abstract_machine {conf with cont= k @ conf.cont}
          | _ -> conf
        end
     | Lambda ((id, _), comp) -> 
        abstract_machine {conf with term=subst_var comp id v}
     | _ -> conf
     end
   | Handle (comp, h) -> 
      abstract_machine {conf with term=comp; cont=([], h)::conf.cont}
   | Perform (opsym, v) -> begin
     match conf.cont with 
      | (pk, h)::k -> begin
        match get_clause opsym h with 
        | Some (x, r, comp) -> 
           let term = subst_var comp x v in 
           let del_cont = 
             DelCont.add r (conf.fwd @ [(pk, h)]) conf.del_cont in 
           let fwd = [([], id_handler)] in
           abstract_machine {conf with term; del_cont; fwd}
        | None -> 
           let fwd = conf.fwd @ [(pk, h)] in 
           abstract_machine {conf with fwd; cont=k}
        end
      | [] -> conf
      end


      

       