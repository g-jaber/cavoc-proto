let failwithf fmt = Format.kasprintf failwith fmt

let fail_error msg =
  prerr_endline msg;
  exit 0
