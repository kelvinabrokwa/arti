open Lwt
open Cohttp
open Cohttp_lwt_unix

(* Models *)

type tag = string

type artifact = {
  url : string;
  tags : tag list;
  annotation : string;
};;

type view = {
  name : string;
  tags : tag list;
}


(* Utilities *)

let rec is_subset l = function
  | [] -> true
  | h :: t -> (List.mem h l) && (is_subset t l);;


(* Art *)

let tags = []
let artifacts = []


let add_artifact artifact =
  artifact :: artifacts


let list_artifacts t =
  List.filter (fun a -> is_subset t a.tags) artifacts


let list_tags =
  tags


(* Server *)

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders: %s\nBody: %s"
        uri meth headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())


let () = ignore (Lwt_main.run server)
