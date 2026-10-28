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
      ?(head = "feat/x")
      ~host
      ~number
      ~title
      ()
  =
  sprintf
    {|{"number":%d,"title":%S,"state":%S,"merged":%b,"merged_at":%s,"draft":%b,"user":{"login":"alice"},"head":{"ref":%S,"sha":"abc123","repo":{"owner":{"login":%S}}},"base":{"ref":"main","repo":{"owner":{"login":"o"}}},"html_url":"https://%s/o/r/pull/%d","body":"Does x.","created_at":"2026-10-20T10:00:00Z","updated_at":"2026-10-21T10:00:00Z"}|}
    number
    title
    state
    merged
    (if merged then {|"2026-10-22T10:00:00Z"|} else "null")
    draft
    head
    head_owner
    host
    number
;;

(* A real repository whose origin is a GitHub URL rewritten (insteadOf) to a
   local bare repository, so fetches and pushes stay on disk: [main] is
   pushed, the pull request #7 is published at [refs/pull/7/head], and the
   current branch [feat/add-things] has two unpushed commits. *)
let sh dir script =
  let command =
    sprintf
      "cd %s && export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 \
       GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t \
       GIT_COMMITTER_EMAIL=t@t && (%s) > /dev/null 2>&1"
      (Filename.quote dir)
      script
  in
  match Sys_unix.command command with
  | 0 -> ()
  | n -> raise_s [%message "setup failed" (script : string) (n : int)]
;;

let set_up_repos (t : Tool_test_helpers.t) =
  let bare = Filename.concat t.dir "bare.git" in
  let repo = Filename.concat t.dir "r" in
  Core_unix.mkdir_p repo;
  sh t.dir "git init -q --bare -b main bare.git";
  sh
    repo
    (sprintf
       "git init -q -b main && git config url.%s.insteadOf \
        https://github.com/o/r.git && git remote add origin \
        https://github.com/o/r.git && echo a > a && git add a && git commit \
        -qm Initial && git push -q origin main && git checkout -qb pr-src && \
        echo b > b && git add b && git commit -qm 'From a contributor' && git \
        push -q origin HEAD:refs/pull/7/head && git checkout -q main && git \
        branch -qD pr-src && git checkout -qb feat/add-things && echo c > c && \
        git add c && git commit -qm 'Add c' -m 'Because c.' && echo d > d && \
        git add d && git commit -qm 'Add d'"
       (Filename.quote bare));
  repo
;;

(* What [POST /repos/o/r/pulls] answers. *)
let create_reply = ref (201, "")

(* GitHub for [o/r]: pull requests #7 (from a fork, with a diff and checks)
   and #5 (a draft). *)
let github (r : Forge_http.Request.t) =
  let accept =
    List.Assoc.find r.headers ~equal:String.Caseless.equal "Accept"
    |> Option.value ~default:""
  in
  match r.meth, path_of r.url with
  | Get, "/user" -> 200, {|{"login":"octocat"}|}
  | Get, "/repos/o/r" -> 200, {|{"full_name":"o/r","default_branch":"main"}|}
  | Get, "/repos/o/r/pulls" ->
    ( 200
    , sprintf
        "[%s,%s]"
        (pr_json
           ~host:"github.com"
           ~number:7
           ~title:"From a contributor"
           ~head_owner:"fork"
           ())
        (pr_json
           ~host:"github.com"
           ~number:5
           ~title:"Old one"
           ~draft:true
           ()) )
  | Get, "/repos/o/r/pulls/7"
    when String.equal accept "application/vnd.github.diff" ->
    ( 200
    , "diff --git a/b b/b\n\
       new file mode 100644\n\
       --- /dev/null\n\
       +++ b/b\n\
       @@ -0,0 +1 @@\n\
       +b\n" )
  | Get, "/repos/o/r/pulls/7" ->
    ( 200
    , pr_json
        ~host:"github.com"
        ~number:7
        ~title:"From a contributor"
        ~head_owner:"fork"
        () )
  | Get, "/repos/o/r/commits/abc123/check-runs" ->
    ( 200
    , {|{"check_runs":[{"name":"build","status":"completed","conclusion":"success","html_url":"https://ci/1"}]}|}
    )
  | Get, "/repos/o/r/commits/abc123/status" ->
    200, {|{"state":"pending","statuses":[]}|}
  | Post, "/repos/o/r/pulls" -> !create_reply
  | _ -> 404, {|{"message":"Not Found"}|}
;;


let git_out dir args =
  let ic =
    Core_unix.open_process_in
      (sprintf
         "GIT_CONFIG_GLOBAL=/dev/null git -C %s %s 2>&1"
         (Filename.quote dir)
         args)
  in
  let out = In_channel.input_all ic in
  ignore (Core_unix.close_process_in ic : Core_unix.Exit_or_signal.t);
  String.strip out
;;
