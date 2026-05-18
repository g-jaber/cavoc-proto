module Sat = Msat_sat
module E = Sat.Int_lit
module F = Msat_tseitin.Make(E)

(* type id = int [@@deriving to_yojson] *)
type id = E.t

type konstraint = (* constraint is a keyword in ocaml *)
  | Kvar  of id
  | Kbool of bool
  | Knot  of konstraint
  | Keq   of konstraint * konstraint
  | Kneq  of konstraint * konstraint
  | Kand  of konstraint * konstraint
  | Kor   of konstraint * konstraint

let rec formula_of_konstraint = function
  | Kvar sym -> F.make_atom sym
  | Kbool b -> if b then F.f_true else F.f_false
  | Knot a ->
      let a = formula_of_konstraint a in
      F.make_not a
  | Keq (a, b) ->
      let a = formula_of_konstraint a in
      let b = formula_of_konstraint b in
      F.make_equiv a b
  | Kneq (a, b) ->
      let a = formula_of_konstraint a in
      let b = formula_of_konstraint b in
      F.make_xor a b
  | Kand (a, b) ->
      let a = formula_of_konstraint a in
      let b = formula_of_konstraint b in
      F.make_and [ a ; b ]
  | Kor (a, b) ->
      let a = formula_of_konstraint a in
      let b = formula_of_konstraint b in
      F.make_or [ a ; b ]

type branch =
  (* All symbolic variables declared in this branch *)
  { pathdecl : id list
  (* All constraints accumulated in this branch *)
  ; pathcond : konstraint list
  }

let check_sat branch =
  let solver = Sat.create () in

  List.iter (fun k ->
    let cnf = F.make_cnf (formula_of_konstraint k) in
    Sat.assume solver cnf ()) branch.pathcond ;

  match Sat.solve solver with Sat.Sat _ -> true | _ -> false

let string_of_id id = Format.asprintf "%a" E.pp id
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

let pp_id fmt = Format.fprintf fmt "%a" E.pp
let pp_constraint fmt e = Format.fprintf fmt "%s" (string_of_constraint e)

let rec pp_list pp_sep pp_elem fmt = function
  | [] -> ()
  | x :: [] -> Format.fprintf fmt "%a" pp_elem x
  | x :: xs -> Format.fprintf fmt "%a%a%a"
                pp_elem x
                pp_sep ()
                (pp_list pp_sep pp_elem) xs

let pp_sep fmt () = Format.fprintf fmt " @ "

let pp_pathdecl fmt = Format.fprintf fmt "%a" (pp_list pp_sep pp_id)
let pp_pathcond fmt = Format.fprintf fmt "%a" (pp_list pp_sep pp_constraint)

let branch_to_yojson { pathdecl ; pathcond ; _ } =
  `Assoc [
    "decl" , `List (List.map (fun id -> `String (string_of_id id)) pathdecl) ;
    "cond" , `List (List.map (fun k -> `String (string_of_constraint k)) pathcond)
  ]

(*
let fresh_symbolic =
  let i = ref 0 in
  (fun () -> i := !i + 1 ; !i)
*)

let empty =
  { pathdecl = []
  ; pathcond = []
  }

let unconstrained branch =
  let sym = E.fresh () in
  sym, { branch with pathdecl = sym :: branch.pathdecl }

let add_constraint branch konstraint =
  { branch with pathcond = konstraint :: branch.pathcond }
