module type NAMES = sig
  type name [@@deriving to_yojson]

  val string_of_name : name -> string
  val pp_name : Format.formatter -> name -> unit
  val is_callable : name -> bool
  val is_cname : name -> bool
end

module type NAMES_GEN = sig
  include NAMES

  val fresh_name : unit -> name
  val from_string : string -> name
end

module type MODE = sig
  val is_callable : bool val is_cname : bool
end

module type PREFIX = sig
  val prefix : string
end

(* A generative functor to create a new NAMES_GEN module *)
module MakeGen (Mode : MODE) (Prefix : PREFIX) () : NAMES_GEN = struct
  type name = int * string

  let string_of_name (i, s) =
    if s = "" then Prefix.prefix ^ string_of_int i else s

  let name_to_yojson (_, s) = `String s

  let pp_name fmt (i, s) =
    if s = "" then Format.fprintf fmt "%s%i" Prefix.prefix i
    else Format.fprintf fmt "%s" s

  let is_callable _ = Mode.is_callable
  let is_cname _ = Mode.is_cname
  let count_name = ref 0

  let fresh_name () =
    let nn = !count_name in
    count_name := !count_name + 1;
    (nn, Prefix.prefix ^ string_of_int nn)

  let from_string str =
    let nn = !count_name in
    count_name := !count_name + 1;
    (nn, str)
end
