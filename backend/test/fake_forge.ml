open! Core
open! Prigh

(* A git host answering [Forge_http] requests from a handler, recording
   them. *)

type t =
  { handler : Forge_http.Request.t -> int * string
  ; requests : Forge_http.Request.t Queue.t
  }

let create handler = { handler; requests = Queue.create () }

let http t : Forge_http.t =
  fun ?cancel:_ request ->
  Queue.enqueue t.requests request;
  let status, body = t.handler request in
  Ok { Forge_http.Response.status; body }
;;

let meth_string : Forge_http.Meth.t -> string = function
  | Get -> "GET"
  | Post -> "POST"
;;

(* What was asked: method, URL, Authorization and Accept when not the
   usual, and the body. *)
let print_requests t =
  Queue.iter t.requests ~f:(fun (r : Forge_http.Request.t) ->
    let header name =
      List.find_map r.headers ~f:(fun (k, v) ->
        Option.some_if (String.Caseless.equal k name) v)
    in
    printf "%s %s\n" (meth_string r.meth) r.url;
    Option.iter (header "Authorization") ~f:(printf "  Authorization: %s\n");
    Option.iter (header "Accept") ~f:(fun accept ->
      if
        not
          (List.mem
             [ "application/vnd.github+json"; "application/json" ]
             accept
             ~equal:String.equal)
      then printf "  Accept: %s\n" accept);
    Option.iter r.body ~f:(printf "  %s\n"));
  Queue.clear t.requests
;;

let path_of url =
  let uri = Uri.of_string url in
  Uri.path uri
;;

(* GitHub's and Forgejo's JSON for a pull request. *)
let pr_json
      ?(state = "open")
      ?(merged = false)
      ?(draft = false)
      ?(head_owner = "o")
      ~host
      ~number
      ~title
      ()
  =
  sprintf
    {|{"number":%d,"title":%S,"state":%S,"merged":%b,"merged_at":%s,"draft":%b,"user":{"login":"alice"},"head":{"ref":"feat/x","sha":"abc123","repo":{"owner":{"login":%S}}},"base":{"ref":"main","repo":{"owner":{"login":"o"}}},"html_url":"https://%s/o/r/pull/%d","body":"Does x.","created_at":"2026-10-20T10:00:00Z","updated_at":"2026-10-21T10:00:00Z"}|}
    number
    title
    state
    merged
    (if merged then {|"2026-10-22T10:00:00Z"|} else "null")
    draft
    head_owner
    host
    number
;;
