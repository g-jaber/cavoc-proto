(*http://localhost:8000/front/index.html*)
(*http://localhost:8000/front/indextuto.html*)

open Lwt.Infix
open Cohttp_lwt_unix

(*Allows you to find the path to the project's root folder on the computer*)
let project_root =
  let cwd = Unix.realpath (Sys.getcwd ()) in
  let base = Filename.basename cwd in
  let root =
    if base = "bin" then Filename.dirname cwd
    else cwd
  in
  Printf.printf "Dossier racine du projet détecté : %s\n%!" root;
  root
let () = Printf.printf "Dossier racine du projet : %s\n%!" project_root

(*Create the list of files that can be load*)
let list_files_in_dir dir =
  try
    let files = Sys.readdir dir in
    let file_list = Array.to_list files in
      (*Printf.printf "Fichiers dans %s : %s\n%!" dir (String.concat ", " file_list);*)
      file_list
  with e ->
      Printf.printf "Erreur lors de la lecture du dossier %s : %s\n%!" dir (Printexc.to_string e);
      []

(* Handles the API request to list files: reads "tuto" or "test" based on query param *)
let list_files_handler _conn req _body =
  let uri = Request.uri req in
  let dir_name = 
    match Uri.get_query_param uri "dir" with
    | Some "test" -> "test"
    | _ -> "tuto" (* Par défaut, on sert le tuto pour la sécurité *)
  in
  let dir = Filename.concat project_root dir_name in
  
  let files = list_files_in_dir dir in
  let json = `List (List.map (fun f -> `String f) files) in
  let response_body = Yojson.Safe.to_string json in
  Server.respond_string ~status:`OK ~body:response_body ()

(* Main router: analyzes the requested URL to route to the API (/list_files) 
  or to the static file service. *)
let callback _conn (req : Request.t) (body : Cohttp_lwt.Body.t) =
  let uri_path = Request.uri req |> Uri.path in
  match uri_path with
  | "/list_files" -> list_files_handler _conn req body
  | _ ->
      (* Logic to serve static files (HTML, JS, CSS) *)
      let file_path =
        let path =
          (* Redirects root "/" to "front/index.html" by default *)
          if uri_path = "/" then "front/index.html"
          (* Cleans the path (removes the leading slash) *)
          else if String.length uri_path > 0 && uri_path.[0] = '/' then
            String.sub uri_path 1 (String.length uri_path - 1)
          else uri_path
        in
        Filename.concat project_root path
      in
      (* Attempts to read the file, returns 404 if not found *)
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Input file_path Lwt_io.read >>= fun body ->
          Server.respond_string ~status:`OK ~body ())
        (fun _ -> Server.respond_string ~status:`Not_found ~body:"File not found" ())
        

(* Initializes and launches the server on port 8000 with error handling 
  (e.g., port already in use). *)
let start_server () =
  let mode = `TCP (`Port 8000) in
  let server = Server.make ~callback () in
  Lwt.catch
    (fun () ->
      Printf.printf " Serveur démarré sur le port 8000, site de tuto sur : http://localhost:8000/front/indextuto.html\n%!";
      Printf.printf " Serveur démarré sur http://localhost:8000/front/index.html\n%!";
      Server.create ~mode server)
    (function
      (* Specific handling if port 8000 is blocked *)
      | Unix.Unix_error (Unix.EADDRINUSE, "bind", _) ->
          Printf.eprintf " Erreur : le port 8000 est déjà utilisé.\n";
          Printf.eprintf " Fermez l'autre instance du serveur avant de relancer.\n";
          exit 1
      | e ->
          Printf.eprintf " Erreur inattendue : %s\n" (Printexc.to_string e);
          exit 1)

let () = Lwt_main.run (start_server ())





