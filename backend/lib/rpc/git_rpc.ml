open! Core
open! Import

let methods =
  [ "git_status"; "git_stage"; "git_unstage"; "git_commit"; "git_push" ]
;;

let param params name =
  match params with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let string_opt params name =
  match param params name with
  | None | Some `Null -> Ok None
  | Some (`String s) -> Ok (Option.some_if (not (String.is_empty s)) s)
  | Some _ -> Or_error.errorf "param %S must be a string" name
;;

let bool params name =
  match param params name with
  | None | Some `Null | Some `False -> Ok false
  | Some `True -> Ok true
  | Some _ -> Or_error.errorf "param %S must be a boolean" name
;;

let paths params =
  match param params "paths" with
  | None | Some `Null -> Ok []
  | Some (`Array items) ->
    Or_error.all
      (List.map items ~f:(function
         | `String s -> Ok s
         | _ -> Or_error.error_string "param \"paths\" must hold only strings"))
  | Some _ -> Or_error.error_string "param \"paths\" must be an array of paths"
;;

let call agent ?dir request =
  let result =
    Agent.host_call
      agent
      ~name:Git_actions.op
      (Git_actions.Request.to_json ?dir request)
  in
  if result.is_error
  then Or_error.error_string result.text
  else Json.parse result.text
;;

let dispatch agent ~meth ~params =
  let open Or_error.Let_syntax in
  let run f =
    Some
      (let%bind dir = string_opt params "dir" in
       let%bind request = f () in
       call agent ?dir request)
  in
  match meth with
  | "git_status" -> run (fun () -> Ok Git_actions.Request.Status)
  | "git_stage" ->
    run (fun () ->
      let%bind tracked = bool params "tracked" in
      let%map paths = paths params in
      Git_actions.Request.Stage { paths; tracked })
  | "git_unstage" ->
    run (fun () -> paths params >>| fun ps -> Git_actions.Request.Unstage ps)
  | "git_commit" ->
    run (fun () ->
      let%bind message = string_opt params "message" in
      let%bind amend = bool params "amend" in
      let%map paths = paths params in
      Git_actions.Request.Commit
        { message = Option.value message ~default:""; amend; paths })
  | "git_push" -> run (fun () -> Ok Git_actions.Request.Push)
  | _ -> None
;;
