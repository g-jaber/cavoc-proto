module type MOVES = sig
  (* to be instantiated *)
  type kdata
  type name
  (* *)

  type direction = Input | Output | None
  type move

  val build : direction * kdata -> move
  val pp_move : Format.formatter -> move -> unit
  val string_of_move : move -> string
  val get_kdata : move -> kdata
  val get_direction : move -> direction
  val switch_direction : move -> move
  val get_subject_name : move -> name list
  val get_transmitted_names : move -> name list

  val unify_move :
    name Util.Namespan.namespan ->
    move ->
    move ->
    name Util.Namespan.namespan option
end

module type A_NF = sig
  module Names : Lang.Names.NAMES
  type abstract_normal_form

  val pp_a_nf : pp_dir:(Format.formatter -> unit) -> Format.formatter -> abstract_normal_form -> unit
  val string_of_a_nf : string -> abstract_normal_form -> string
  val get_subject_name : abstract_normal_form -> Names.name option
  val get_support : abstract_normal_form -> Names.name list

  val is_equiv_a_nf :
    Names.name Util.Namespan.namespan ->
    abstract_normal_form ->
    abstract_normal_form ->
    Names.name Util.Namespan.namespan option
end

module Make (A_nf : A_NF) :
  MOVES with type name = A_nf.Names.name and type kdata = A_nf.abstract_normal_form =
struct
  type name = A_nf.Names.name
  type kdata = A_nf.abstract_normal_form
  type direction = Input | Output | None

  let string_of_direction = function
    | Input -> "?"
    | Output -> "!"
    | None -> "."

  let switch = function Input -> Output | Output -> Input | None -> None

  type move = direction * kdata

  let build move = move

  let pp_move fmt (dir, kdata) =
    let pp_dir fmt = Format.pp_print_string fmt (string_of_direction dir) in
    A_nf.pp_a_nf ~pp_dir fmt kdata

  let string_of_move (dir, kdata) =
    A_nf.string_of_a_nf (string_of_direction dir) kdata

  let get_kdata (_, d) = d
  let get_direction (p, _) = p
  let switch_direction (p, d) = (switch p, d)
  let get_transmitted_names (_, kdata) = A_nf.get_support kdata

  let get_subject_name (_, kdata) =
    match A_nf.get_subject_name kdata with Some n -> [ n ] | None -> []

  let unify_move span move1 move2 =
    match (move1, move2) with
    | ((Output, kdata1), (Output, kdata2)) | ((Input, kdata1), (Input, kdata2))
      ->
        A_nf.is_equiv_a_nf span kdata1 kdata2
    | _ -> None
end
