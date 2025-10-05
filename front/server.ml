(*http://localhost:8000/front/index.html*)

open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

let project_root = Filename.dirname (Sys.getcwd ())

let server =
  let callback _conn (req : Request.t) (_body : Cohttp_lwt.Body.t) =
    let uri_path = Request.uri req |> Uri.path in
    let file_path =
      let path =
        if uri_path = "/" then "front/index.html"
        else if String.length uri_path > 0 && uri_path.[0] = '/' then String.sub uri_path 1 (String.length uri_path - 1)
        else uri_path
      in
      Filename.concat project_root path
    in
    Lwt.catch
      (fun () ->
        Lwt_io.with_file ~mode:Lwt_io.Input file_path Lwt_io.read >>= fun body ->
        Server.respond_string ~status:`OK ~body ())
      (fun _ -> Server.respond_string ~status:`Not_found ~body:"File not found" ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = Lwt_main.run server






