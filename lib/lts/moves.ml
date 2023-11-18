module type MOVES = sig
  (* to be instantiated *)
  type kind
  type data
  type name
  (* *)

  type direction = Input | Output | None
  type move

  val build : direction * kind * data -> move
  val string_of_move : move -> string
  val get_kind : move -> kind
  val get_data : move -> data
  val get_direction : move -> direction
  val switch_direction : move -> move
  val get_subject_names : move -> name list
  val get_transmitted_names : move -> name list

  val unify_move :
    name Util.Namespan.namespan ->
    move ->
    move ->
    name Util.Namespan.namespan option
end

module Make (IntLang : Lang.Interactive.LANG) :
  MOVES
    with type name = IntLang.name
     and type data = IntLang.abstract_val
     and type kind = IntLang.kind_interact = struct
  type name = IntLang.name
  type kind = IntLang.kind_interact
  type data = IntLang.abstract_val
  type direction = Input | Output | None

  let string_of_direction = function
    | Input -> "?"
    | Output -> "!"
    | None -> "."

  let switch = function Input -> Output | Output -> Input | None -> None

  type move = direction * kind * data

  let build move = move

  let string_of_move (direction, kind, aval) =
    IntLang.string_of_kind_interact kind
    ^ string_of_direction direction
    ^ IntLang.string_of_abstract_val aval

  let get_data (_, _, d) = d
  let get_kind (_, k, _) = k
  let get_direction (p, _, _) = p
  let switch_direction (p, k, d) = (switch p, k, d)
  let get_transmitted_names (_, _, aval) = IntLang.names_of_abstract_val aval

  let get_subject_names (_, k, _) =
    match IntLang.name_of_kind_interact k with Some n -> [ n ] | None -> []

  let unify_move span move1 move2 =
    match (move1, move2) with
    | ((Output, kind1, aval1), (Output, kind2, aval2))
    | ((Input, kind1, aval1), (Input, kind2, aval2)) ->
        if IntLang.is_equiv_kind_interact span kind1 kind2 then
          IntLang.unify_abstract_val span aval1 aval2
        else None
    | _ -> None
end
