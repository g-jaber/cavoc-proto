let debug_mode = ref true

let print_debug str =
  if !debug_mode then print_endline str else ()