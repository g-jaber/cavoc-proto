module type NAMES = sig
  type name [@@deriving to_yojson]

  val string_of_name : name -> string
  val pp_name : Format.formatter -> name -> unit
  val is_callable : name -> bool
  val is_cname : name -> bool
end

module type NAMES_INT = sig
  include NAMES with type name = int * string
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

(* A generative functor to create a new NAMES module based on de Bruijn indices *)
module MakeInt (Mode : MODE) (Prefix : PREFIX) () : NAMES_INT = struct
  type name = int * string

  let string_of_name (i, s) =
    if s = "" then Prefix.prefix ^ string_of_int i else s

  let name_to_yojson (_, s) = `String s

  let pp_name fmt (i, s) =
    if s = "" then Format.fprintf fmt "%s%i" Prefix.prefix i
    else Format.fprintf fmt "%s" s

  let is_callable _ = Mode.is_callable
  let is_cname _ = Mode.is_cname
end

module MakeAggregate (Names1 : NAMES) (Names2 : NAMES) :
  NAMES with type name = (Names1.name, Names2.name) Either.t = struct
  type name = (Names1.name, Names2.name) Either.t

  let name_to_yojson = function
    | Either.Left fn -> Names1.name_to_yojson fn
    | Either.Right pn -> Names2.name_to_yojson pn

  let string_of_name = function
    | Either.Left f -> Names1.string_of_name f
    | Either.Right p -> Names2.string_of_name p

  let pp_name fmt = function
    | Either.Left fn -> Names1.pp_name fmt fn
    | Either.Right pn -> Names2.pp_name fmt pn

  let is_callable = function
    | Either.Left nn1 -> Names1.is_callable nn1
    | Either.Right nn2 -> Names2.is_callable nn2

  let is_cname = function
    | Either.Left nn1 -> Names1.is_cname nn1
    | Either.Right nn2 -> Names2.is_cname nn2
end
