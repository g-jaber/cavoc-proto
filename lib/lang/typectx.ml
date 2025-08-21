module type TYPECTX = sig
  type name
  type typ
  type t [@@deriving to_yojson]

  val empty : t
  val concat : t -> t -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val get_names : t -> name list
  val lookup_exn : t -> name -> typ
  val add : t -> name -> typ -> t
  val to_pmap : t -> (name, typ) Util.Pmap.pmap
end

module type TYPECTX_PMAP = sig
  type n
  type ty

  include
    TYPECTX
      with type name = n
       and type typ = ty
       and type t = (n, ty) Util.Pmap.pmap
end

module type TYPECTX_LIST = sig
  type ty

  include TYPECTX with type name = int and type typ = ty and type t = ty list
end

module Make_PMAP
    (Names : Names.NAMES)
    (Types : sig
      type t [@@deriving to_yojson]

      val pp : Format.formatter -> t -> unit
    end) :
  TYPECTX
    with type name = Names.name
     and type typ = Types.t
     and type t = (Names.name, Types.t) Util.Pmap.pmap = struct
  type name = Names.name
  type typ = Types.t
  type t = (name, Types.t) Util.Pmap.pmap

  let empty = Util.Pmap.empty
  let concat = Util.Pmap.concat

  let pp fmt name_ctx =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_empty fmt () = Format.fprintf fmt "⋅" in
    let pp_pair fmt (n, ty) =
      Format.fprintf fmt "%a : %a" Names.pp_name n Types.pp ty in
    Util.Pmap.pp_pmap ~pp_empty ~pp_sep pp_pair fmt name_ctx

  let to_string = Format.asprintf "%a" pp
  let get_names = Util.Pmap.dom

  let to_yojson name_ctx =
    let to_string (nn, ty) = (Names.string_of_name nn, Types.to_yojson ty) in
    `Assoc (Util.Pmap.to_list @@ Util.Pmap.map to_string name_ctx)

  let lookup_exn name_ctx nn = Util.Pmap.lookup_exn nn name_ctx
  let add name_ctx nn ty = Util.Pmap.add (nn, ty) name_ctx
  let to_pmap = Fun.id
end

module Make_List (Types : sig
  type t [@@deriving to_yojson]

  val pp : Format.formatter -> t -> unit
end) :
  TYPECTX with type name = int and type typ = Types.t and type t = Types.t list =
struct
  type name = int
  type typ = Types.t
  type t = Types.t list

  let empty = []
  let concat = List.append

  let pp fmt = function
    | [] -> Format.fprintf fmt "⋅"
    | name_ctx ->
        let pp_sep fmt () = Format.fprintf fmt ", " in
        Format.pp_print_list ~pp_sep Types.pp fmt name_ctx

  let to_string = Format.asprintf "%a" pp
  let get_names = List.mapi (fun i _ -> i)
  let to_yojson nctx = `List (List.map Types.to_yojson nctx)
  let lookup_exn = List.nth

  (* The add function always add at the end of the list !!!*)
  let add name_ctx _ ty = name_ctx @ [ ty ]

  let to_pmap name_ctx =
    Util.Pmap.list_to_pmap @@ List.mapi (fun i ty -> (i, ty)) name_ctx
end
