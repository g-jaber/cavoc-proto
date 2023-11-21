module type MOVES = sig
  (* to be instantiated *)
  type kdata
  type name
  (* *)

  type direction = Input | Output | None
  type move

  val build : direction * kdata -> move
  val string_of_move : move -> string
  val get_kdata : move -> kdata
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
     and type kdata = IntLang.abstract_normal_form
     = struct
  type name = IntLang.name
  type kdata = IntLang.abstract_normal_form
  type direction = Input | Output | None

  let string_of_direction = function
    | Input -> "?"
    | Output -> "!"
    | None -> "."

  let switch = function Input -> Output | Output -> Input | None -> None

  type move = direction * kdata

  let build move = move

  let string_of_move (dir, kdata) =
    IntLang.string_of_a_nf (string_of_direction dir) kdata

  let get_kdata (_, d) = d
  let get_direction (p, _) = p
  let switch_direction (p, d) = (switch p, d)
  let get_transmitted_names (_, kdata) = IntLang.get_support kdata

  let get_subject_names (_, kdata) =
    match IntLang.get_subject_names kdata with Some n -> [ n ] | None -> []

  let unify_move span move1 move2 =
    match (move1, move2) with
    | ((Output, kdata1), (Output, kdata2))
    | ((Input, kdata1), (Input, kdata2)) ->
      IntLang.is_equiv_a_nf span kdata1 kdata2
    | _ -> None
end
