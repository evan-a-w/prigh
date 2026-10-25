open! Core

module Kind = struct
  type t =
    | Host
    | Cwd
    | Tools
    | Plan_mode
    | Delivery
    | Handover
    | Goal
    | Goal_continuation
    | Reminder
    | Resume
    | Shell
    | Summary
    | Review
    | Init
    | Review_comments
    | Other of string
  [@@deriving sexp_of, equal]

  let of_string = function
    | "host" -> Host
    | "cwd" -> Cwd
    | "tools" | "editor" -> Tools
    | "plan_mode" -> Plan_mode
    | "delivery" -> Delivery
    | "handover" -> Handover
    | "goal" -> Goal
    | "goal_continuation" -> Goal_continuation
    | "reminder" -> Reminder
    | "resume" -> Resume
    | "shell" -> Shell
    | "summary" -> Summary
    | "review" -> Review
    | "init" -> Init
    | "review_comments" -> Review_comments
    | other -> Other other
  ;;
end

module Note = struct
  type t =
    { kind : Kind.t
    ; title : string
    ; text : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind kind = Json.string_field j "kind" in
    let%bind title = Json.string_field j "title" in
    let%map text = Json.string_field j "text" in
    { kind = Kind.of_string kind; title; text }
  ;;
end

module Source = struct
  type t =
    { kind : Kind.t
    ; title : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind kind = Json.string_field j "kind" in
    let%map title = Json.string_field j "title" in
    { kind = Kind.of_string kind; title }
  ;;
end

type t =
  { body : string option [@sexp.option]
  ; notes : Note.t list [@sexp.list]
  ; attachments : string list [@sexp.list]
  ; source : Source.t option [@sexp.option]
  }
[@@deriving sexp_of, equal]

let empty = { body = None; notes = []; attachments = []; source = None }

let of_json j =
  let open Or_error.Let_syntax in
  let%bind body = Json.string_opt_field j "body" in
  let%bind notes = Json.optional_list_field j "notes" ~f:Note.of_json in
  let%bind attachments =
    Json.optional_list_field j "attachments" ~f:(fun a ->
      Json.string_field a "path")
  in
  let%map source =
    match Json.field j "source" with
    | None -> Ok None
    | Some s -> Source.of_json s >>| Option.some
  in
  { body; notes; attachments; source }
;;
