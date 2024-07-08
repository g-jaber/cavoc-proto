type ('a, 'b) pmap

val empty : ('a, 'b) pmap
val is_empty : ('a, 'b) pmap -> bool
val singleton : 'a * 'b -> ('a, 'b) pmap
val concat : ('a, 'b) pmap -> ('a, 'b) pmap -> ('a, 'b) pmap
val list_to_pmap : ('a * 'b) list -> ('a, 'b) pmap
val to_list : ('a, 'b) pmap -> ('a * 'b) list
val dom : ('a, 'b) pmap -> 'a list
val codom : ('a, 'b) pmap -> 'b list
val mem : 'a -> ('a, 'b) pmap -> bool
val lookup : 'a -> ('a, 'b) pmap -> 'b option
val lookup_exn : 'a -> ('a, 'b) pmap -> 'b
val is_in_dom_im : 'a * 'b -> ('a, 'b) pmap -> bool
val add : 'a * 'b -> ('a, 'b) pmap -> ('a, 'b) pmap
val add_span : 'a * 'b -> ('a, 'b) pmap -> ('a, 'b) pmap option
val modadd : 'a * 'b -> ('a, 'b) pmap -> ('a, 'b) pmap

(* The first argument of string_of_pmap is the string for the empty map,
   the second is the string for the separation symbol between the index and its value *)
val string_of_pmap :
  string ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('a, 'b) pmap ->
  string

val pp_pmap :
  ?pp_empty:(Format.formatter -> unit -> unit) ->
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a * 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) pmap ->
  unit

val map_dom : ('a -> 'b) -> ('a, 'c) pmap -> ('b, 'c) pmap
val map_im : ('a -> 'b) -> ('c, 'a) pmap -> ('c, 'b) pmap
val map : ('a * 'b -> 'c * 'd) -> ('a, 'b) pmap -> ('c, 'd) pmap
val map_list : ('a * 'b -> 'c) -> ('a, 'b) pmap -> 'c list
val filter_map : ('a * 'b -> ('c * 'd) option) -> ('a, 'b) pmap -> ('c, 'd) pmap
val filter_map_im : ('b -> 'c option) -> ('a, 'b) pmap -> ('a, 'c) pmap
val fold : ('a -> 'b * 'c -> 'a) -> 'a -> ('b, 'c) pmap -> 'a
val disjoint : ('a, 'b) pmap -> ('a, 'b) pmap -> bool
val select_im : 'b -> ('a, 'b) pmap -> 'a list
val filter_dom : ('a -> bool) -> ('a, 'b) pmap -> ('a, 'b) pmap
