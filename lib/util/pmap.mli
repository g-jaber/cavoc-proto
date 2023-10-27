type ('a,'b) pmap

val empty : ('a,'b) pmap

val is_empty : ('a,'b) pmap -> bool

val singleton : ('a*'b) -> ('a,'b) pmap

val concat :  ('a,'b) pmap -> ('a,'b) pmap -> ('a,'b) pmap

val list_to_pmap : ('a*'b) list -> ('a,'b) pmap

val dom : ('a,'b) pmap -> 'a list
val codom : ('a,'b) pmap -> 'b list

val mem : 'a -> ('a,'b) pmap -> bool

val lookup : 'a -> ('a,'b) pmap -> 'b option

val lookup_exn : 'a -> ('a,'b) pmap -> 'b

val is_in_dom_im : ('a*'b) -> ('a,'b) pmap -> bool

val add : ('a*'b) -> ('a,'b) pmap -> ('a,'b) pmap

val add_span : ('a*'b) -> ('a,'b) pmap -> ('a,'b) pmap option

val modadd_pmap : ('a*'b) -> ('a,'b) pmap -> ('a,'b) pmap

val modadd_pmap2 : ('a*'b) -> ('a*'b) -> ('a,'b) pmap -> ('a,'b) pmap

(* first argument is the string for the empty map, second is the string for the symbol between the index and its value *)
val string_of_pmap : string -> string -> ('a -> string) -> ('b -> string) -> ('a,'b) pmap -> string

val map_dom : ('a -> 'b) -> ('a,'c) pmap -> ('b,'c) pmap

val map_im : ('a -> 'b) -> ('c,'a) pmap -> ('c,'b) pmap

val map : (('a*'b) -> ('c*'d)) -> ('a,'b) pmap -> ('c,'d) pmap

val map_list : (('a*'b) -> 'c) -> ('a,'b) pmap -> 'c list

val filter_map : (('a*'b) -> ('c*'d) option) -> ('a,'b) pmap -> ('c,'d) pmap

val fold : ('a -> ('b*'c) -> 'a) -> 'a -> ('b,'c) pmap -> 'a 

val disjoint : ('a,'b) pmap -> ('a,'b) pmap -> bool