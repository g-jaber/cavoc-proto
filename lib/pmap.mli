type ('a,'b) pmap

val empty : ('a,'b) pmap

val singleton : ('a*'b) -> ('a,'b) pmap

val concat :  ('a,'b) pmap -> ('a,'b) pmap -> ('a,'b) pmap

val list_to_pmap : ('a*'b) list -> ('a,'b) pmap

val lookup_pmap : 'a -> ('a,'b) pmap -> 'b option

val add : ('a*'b) -> ('a,'b) pmap -> ('a,'b) pmap

val modadd_pmap : ('a*'b) -> ('a,'b) pmap -> ('a,'b) pmap

val modadd_pmap2 : ('a*'b) -> ('a*'b) -> ('a,'b) pmap -> ('a,'b) pmap

val string_of_pmap : string -> ('a -> string) -> string -> (string,'a) pmap -> string

val map_im : ('a -> 'b) -> ('c,'a) pmap -> ('c,'b) pmap

val map : (('a*'b) -> ('c*'d)) -> ('a,'b) pmap -> ('c,'d) pmap

val filter_map : (('a*'b) -> ('c*'d) option) -> ('a,'b) pmap -> ('c,'d) pmap

val fold : ('a -> ('b*'c) -> 'a) -> 'a -> ('b,'c) pmap -> 'a 