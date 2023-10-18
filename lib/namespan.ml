type 'a namespan = ('a,'a) Pmap.pmap
let empty_nspan = Pmap.empty
let id_nspan l = Pmap.list_to_pmap @@ List.map (fun x -> (x,x)) l 
let add_nspan = Pmap.add_span
let is_in_dom_im = Pmap.is_in_dom_im