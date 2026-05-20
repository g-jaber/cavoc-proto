module Sat = Msat_sat
module E = Sat.Int_lit
module F = Msat_tseitin.Make(E)

type id = int [@@deriving to_yojson]
(* type id = E.t *)

(* TODO: Unfortunate name, this is just a symbolic expression.
         It only becomes a constraint once added to pathcond field
         in a branch *)
type konstraint =
  | Kvar  of id
  | Kbool of bool
  | Knot  of konstraint
  | Keq   of konstraint * konstraint
  | Kneq  of konstraint * konstraint
  | Kand  of konstraint * konstraint
  | Kor   of konstraint * konstraint

let neg = function
  | Knot k -> k
  | k -> Knot k

type branch_ctx = (id * Types.typ) list [@@deriving to_yojson]

type branch =
  (* All symbolic variables declared in this branch *)
  { pathdecl : branch_ctx
  (* All constraints accumulated in this branch *)
  ; pathcond : konstraint list
  }

let formula_of_konstraint branch =
  (* TODO: handle types *)
  let vmap = List.map (fun (id, _) -> (id, E.fresh ())) branch.pathdecl in
  let rec aux = function
    | Kvar s ->
        let id =
          try List.assoc s vmap with
            Not_found -> failwith "Unbound symbolic variable, please report."
        in
        F.make_atom id
    | Kbool b -> if b then F.f_true else F.f_false
    | Knot a ->
        let a = aux a in
        F.make_not a
    | Keq (a, b) ->
        let a = aux a in
        let b = aux b in
        F.make_equiv a b
    | Kneq (a, b) ->
        let a = aux a in
        let b = aux b in
        F.make_xor a b
    | Kand (a, b) ->
        let a = aux a in
        let b = aux b in
        F.make_and [ a ; b ]
    | Kor (a, b) ->
        let a = aux a in
        let b = aux b in
        F.make_or [ a ; b ]
  in
  aux

let check_sat branch =
  let solver = Sat.create () in

  List.iter (fun k ->
    let cnf = F.make_cnf (formula_of_konstraint branch k) in
    Sat.assume solver cnf ()) branch.pathcond ;

  match Sat.solve solver with Sat.Sat _ -> true | _ -> false

let string_of_id id = "s" ^ string_of_int id
let rec string_of_constraint =
  let print op a b =
    Printf.sprintf "(%s %s %s)" op
      (string_of_constraint a)
      (string_of_constraint b)
  in
  function
  | Kvar id -> string_of_id id
  | Kbool b -> if b then "true" else "false"
  | Knot a -> Printf.sprintf "(not %s)" (string_of_constraint a)
  | Keq (a, b) -> print "=" a b
  | Kneq (a, b) -> print "<>" a b
  | Kand (a, b) -> print "and" a b
  | Kor (a, b) -> print "or" a b

let pp_id fmt = Format.fprintf fmt "s%a" Format.pp_print_int
let pp_decl fmt (id, _) = pp_id fmt id
let pp_constraint fmt e = Format.fprintf fmt "%s" (string_of_constraint e)

let rec pp_list pp_sep pp_elem fmt = function
  | [] -> ()
  | x :: [] -> Format.fprintf fmt "%a" pp_elem x
  | x :: xs -> Format.fprintf fmt "%a%a%a"
                pp_elem x
                pp_sep ()
                (pp_list pp_sep pp_elem) xs

let pp_sep fmt () = Format.fprintf fmt " @ "

let pp_pathdecl fmt = Format.fprintf fmt "%a" (pp_list pp_sep pp_decl)
let pp_pathcond fmt = Format.fprintf fmt "%a" (pp_list pp_sep pp_constraint)

let branch_to_yojson { pathdecl ; pathcond ; _ } =
  `Assoc [
    "decl" , branch_ctx_to_yojson pathdecl ;
    "cond" , `List (List.map (fun k -> `String (string_of_constraint k)) pathcond)
  ]

let fresh_symbolic =
  let i = ref 0 in
  (fun () -> i := !i + 1 ; !i)

(* Returns the union of the symbolic variables declared in ctx1 and ctx2 *)
let union_ctx ctx1 ctx2 =
  (* Since symbolic variables are uniquely generated,
     ctx1 and ctx2 are disjoint *)
  ctx1 @ ctx2

(* Extends a branch with all the declarations from ctx *)
let extend_branch_ctx branch ctx =
  { branch with pathdecl = branch.pathdecl @ ctx }

let empty_branch_ctx = []
let empty =
  { pathdecl = empty_branch_ctx
  ; pathcond = []
  }

let unconstrained branch_ctx =
  let sym = fresh_symbolic () in
  sym, (sym, Types.TBool) :: branch_ctx

let add_constraint branch konstraint =
  { branch with pathcond = konstraint :: branch.pathcond }
