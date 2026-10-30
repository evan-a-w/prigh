open! Core
open! Import

let refine_after = 3
let max_chars = 8_000
let user_chars = 1_500
let assistant_chars = 400

(* What the user typed, not what the harness wrote in their place. *)
let typed (u : Message.User.t) =
  match Injected.of_message ~origin:u.origin u.text with
  | Some { source = Some _; _ } -> None
  | Some _ | None ->
    (match String.strip (Message.User.body u) with
     | "" -> None
     | text -> Some text)
;;

(* The user's messages in the session's entries, newest first, and how
   many of them come before its latest description. *)
let users_and_described_at session =
  let users_after, described =
    List.fold_until
      (Session.entries session)
      ~init:(0, false)
      ~f:(fun (n, _) (e : Session.Entry.t) ->
        match e.payload with
        | Description _ -> Stop (n, true)
        | Message (User u) when Option.is_some (typed u) -> Continue (n + 1, false)
        | _ -> Continue (n, false))
      ~finish:Fn.id
  in
  let total =
    List.count (Session.entries session) ~f:(fun (e : Session.Entry.t) ->
      match e.payload with
      | Message (User u) -> Option.is_some (typed u)
      | _ -> false)
  in
  total, Option.some_if described (total - users_after)
;;

let wanted session =
  match users_and_described_at session with
  | 0, _ -> false
  | _, None -> true
  | total, Some at -> at < refine_after && total >= refine_after
;;

let instructions =
  "You name the sessions of a coding agent for the user's session list. \
   Write a title of two to six words saying what the user is working on, \
   specific enough to tell it apart from their other sessions: name the \
   feature, bug, file, command or tool, e.g. \"Fix flaky session reload \
   test\", \"Add OAuth login to the CLI\", \"Review menu keyboard \
   navigation\". Sentence case. Do not use vague words such as help, \
   question, conversation, session, task or request, and do not describe \
   the assistant. If a current title is given and still fits, reply with it \
   unchanged. Reply with the title only: no quotes, no trailing period, no \
   preamble."
;;

let cut text n =
  let text = String.strip text in
  if String.length text <= n then text else String.prefix text (n - 1) ^ "…"
;;

let conversation ?current messages =
  let turns =
    List.filter_map messages ~f:(function
      | Message.User u -> Option.map (typed u) ~f:(fun t -> "User: " ^ cut t user_chars)
      | Assistant a ->
        (match String.strip (Message.Assistant.text a) with
         | "" -> None
         | text -> Some ("Assistant: " ^ cut text assistant_chars))
      | Tool_result _ -> None)
  in
  (* The first request says what the session set out to do, the latest
     turns where it went. *)
  let turns =
    match turns with
    | [] -> []
    | first :: rest ->
      let budget = max_chars - String.length first in
      let _, latest =
        List.fold_until
          (List.rev rest)
          ~init:(budget, [])
          ~f:(fun (left, kept) turn ->
            let left = left - String.length turn - 2 in
            if left < 0 then Stop (left, kept) else Continue (left, turn :: kept))
          ~finish:Fn.id
      in
      let omitted = List.length rest - List.length latest in
      (first :: (if omitted > 0 then [ sprintf "[%d messages omitted]" omitted ] else []))
      @ latest
  in
  String.concat ~sep:"\n\n" turns
  ^
  match current with
  | None -> ""
  | Some title -> "\n\nCurrent title: " ^ title
;;

let describe ~provider ~model ?cancel session =
  let user =
    conversation ?current:(Session.description session) (Session.messages session)
  in
  Or_error.map
    (Naming.ask ~provider ~model ?cancel ~what:"naming the session" ~system:instructions user)
    ~f:(fun text ->
      let (_ : Session.Entry.t) = Session.set_description session ~text in
      text)
;;
