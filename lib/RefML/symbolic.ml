(**
  The [Symbolic] module contains the definition of symbolic expression as well
  as the facilities to call some solver.
 *)

module Sat = Msat_sat
module E = Sat.Int_lit
module F = Msat_tseitin.Make(E)

type id = int [@@deriving to_yojson]

(** Symbolic expression *)
type symbolic_expr =
  | Kvar  of id
  | Kbool of bool
  | Knot  of symbolic_expr
  | Keq   of symbolic_expr * symbolic_expr
  | Kneq  of symbolic_expr * symbolic_expr
  | Kand  of symbolic_expr * symbolic_expr
  | Kor   of symbolic_expr * symbolic_expr

let neg = function
  | Knot k -> k
  | k -> Knot k

(** typing context for symbolic variables *)
type symbolic_ctx = (id * Types.typ) list [@@deriving to_yojson]

type branch =
  (** All symbolic variables declared in this branch *)
  { pathdecl : symbolic_ctx
  (** All constraints accumulated in this branch *)
  ; pathcond : symbolic_expr list
  }

let formula_of_konstraint vmap =
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

  let vmap = List.map (fun (id, _) -> (id, E.fresh ())) branch.pathdecl in

  List.iter (fun k ->
    let cnf = F.make_cnf (formula_of_konstraint vmap k) in
    Sat.assume solver cnf ()) branch.pathcond ;

  match Sat.solve solver with Sat.Sat _ -> true | _ -> false

(** Equality under both paths, always true when their conjunction is infeasible. *)
let equiv_under branch1 branch2 expr1 expr2 =
  let pathdecl =
    List.sort_uniq
      (fun (id1, _) (id2, _) -> Int.compare id1 id2)
      (branch1.pathdecl @ branch2.pathdecl)
  in
  let pathcond =
    Kneq (expr1, expr2) :: (branch1.pathcond @ branch2.pathcond)
  in
  not (check_sat { pathdecl; pathcond })

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
    "decl" , symbolic_ctx_to_yojson pathdecl ;
    "cond" , `List (List.map (fun k -> `String (string_of_constraint k)) pathcond)
  ]

let fresh_symbolic =
  let i = ref 0 in
  (fun () -> i := !i + 1 ; !i)

(**
  Returns the union of the symbolic variables declared in two symbolic
  contexts, assuming the context are disjoint
 *)
let union_ctx ctx1 ctx2 =
  ctx1 @ ctx2

(**
  Same as {!val: union_ctx} but operates on the symbolic_ctx of a branch instead.
 *)
let extend_symbolic_ctx branch ctx =
  { branch with pathdecl = branch.pathdecl @ ctx }

let empty_symbolic_ctx = []
let empty =
  { pathdecl = empty_symbolic_ctx
  ; pathcond = []
  }

(** Adds a fresh symbolic variable to a context *)
let unconstrained symbolic_ctx =
  let sym = fresh_symbolic () in
  sym, (sym, Types.TBool) :: symbolic_ctx

(** Adds a constraint to a branch *)
let add_constraint branch konstraint =
  { branch with pathcond = konstraint :: branch.pathcond }
