type 'a namespan = ('a,'a) Pmap.pmap
let empty_nspan = Pmap.empty
let add_nspan = Pmap.add_span
let is_in_dom_im = Pmap.is_in_dom_im