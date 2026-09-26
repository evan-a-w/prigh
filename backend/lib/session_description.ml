open! Core
open! Import

let min_user_messages = 2
let min_tokens = 1500
let max_length = 80
let transcript_chars = 12_000

let wanted session =
  Option.is_none (Session.description session)
  &&
  let messages = Session.messages session in
  let users =
    List.count messages ~f:(function
      | Message.User _ -> true
      | Assistant _ | Tool_result _ -> false)
  in
  users >= min_user_messages
  || Compaction.estimate_tokens messages >= min_tokens
;;

let instructions =
  "Describe the conversation below in one short line (at most eight words) \
   saying what the user is working on, e.g. \"Fixing flaky session tests\" or \
   \"Adding OAuth login to the CLI\". Reply with the line only: no quotes, no \
   trailing period, no preamble."
;;

let clean text =
  let line =
    String.split_lines text
    |> List.map ~f:String.strip
    |> List.find ~f:(Fn.non String.is_empty)
    |> Option.value ~default:""
  in
  let line =
    String.strip line ~drop:(fun c -> Char.equal c '"' || Char.equal c '\'')
  in
  let line =
    String.rstrip line ~drop:(fun c -> Char.equal c '.' || Char.is_whitespace c)
  in
  if String.length line > max_length
  then String.rstrip (String.prefix line (max_length - 1)) ^ "…"
  else line
;;

let describe
      ~(provider : Provider.t)
      ~model
      ?(cancel = Cancellation.never)
      session
  =
  let transcript =
    Compaction.render_transcript (Session.messages session)
    |> Fn.flip String.prefix transcript_chars
  in
  let request =
    { Provider.Request.model
    ; system = Some instructions
    ; messages = [ Message.user transcript ]
    ; tools = []
    ; thinking = Off
    ; max_tokens = Some 60
    }
  in
  let reply = provider.stream request ~cancel ~on_event:ignore in
  match reply.stop_reason with
  | Error e -> Or_error.error_string ("description failed: " ^ e)
  | Aborted -> Or_error.error_string "description aborted"
  | End_turn | Tool_use | Length ->
    let text = clean (Message.Assistant.text reply) in
    if String.is_empty text
    then Or_error.error_string "description failed: empty reply"
    else (
      let (_ : Session.Entry.t) = Session.set_description session ~text in
      Ok text)
;;
