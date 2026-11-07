open! Core
open! Import

let name = "memory"
let changes action = not (String.equal action "recall")
let list_limit = 50
let list_width = 200
let search_limit = 10

let spec =
  { Tool_spec.name
  ; parallel_safe = false
  ; on_host = false
  ; freeform = None
  ; destructive =
      Depends
        (fun args ->
          Option.value_map
            (Tool_args.string_opt args "action")
            ~default:true
            ~f:changes)
  ; description =
      "The user's persistent memory, kept across sessions. Use it VERY RARELY. \
       save (text, optional title) only when the user explicitly asks you to \
       remember something, or states a lasting preference and asks that it be \
       kept; saving with an existing title replaces that note. Never save \
       routine task details, facts you can find in the code or the repository, \
       or anything inferred or speculative. recall (optional query; without \
       one it lists the notes) only when the user refers to something they \
       told you before or the request obviously depends on it (\"use my usual \
       X\", \"what did I say about Y\"); never recall by default or at the \
       start of a task. forget (id or title) removes a wrong or unwanted note."
  ; parameters =
      Tool_args.schema
        ~required:[ "action" ]
        [ "action", `String, "save, recall or forget"
        ; "text", `String, "save: the note, short and self-contained"
        ; ( "title"
          , `String
          , "save: a few words naming the note (an existing title replaces \
             that note)" )
        ; "query", `String, "recall: words to look for (default: list all)"
        ; "id", `String, "forget: the note's id (from recall) or title"
        ]
  }
;;

let date saved =
  Date.to_string (Time_float.to_date saved ~zone:Time_float.Zone.utc)
;;

let header (note : Memory.Note.t) =
  String.concat
    ~sep:" · "
    ((note.id :: Option.to_list note.title) @ [ date note.saved ])
;;

let full note = sprintf "%s\n%s" (header note) note.text

let short (note : Memory.Note.t) =
  let text = String.concat ~sep:" " (String.split_lines note.text) in
  let text =
    if String.length text <= list_width
    then text
    else
      fst (Utf8.split_incomplete_suffix (String.prefix text list_width)) ^ "…"
  in
  sprintf "%s: %s" (header note) text
;;

let plural n what = sprintf "%d %s%s" n what (if n = 1 then "" else "s")

let recall memory query =
  let%map.Or_error notes = Memory.notes memory in
  let total = List.length notes in
  match notes, query with
  | [], _ -> "Memory is empty."
  | _, None ->
    let newest = List.take (List.rev notes) list_limit in
    let more = total - List.length newest in
    String.concat
      ~sep:"\n"
      ((sprintf "%s, newest first:" (plural total "note")
        :: List.map newest ~f:short)
       @
       if more > 0
       then [ sprintf "(%d older not shown: recall with a query)" more ]
       else [])
  | _, Some query ->
    (match Memory.search notes query with
     | [] ->
       sprintf
         "No note matches %S (%s in memory; recall without a query lists them)."
         query
         (plural total "note")
     | found ->
       let shown = List.take found search_limit in
       String.concat
         ~sep:"\n\n"
         (List.map shown ~f:full
          @
          if List.length found > search_limit
          then
            [ sprintf
                "(%d more match: narrow the query)"
                (List.length found - search_limit)
            ]
          else []))
;;

let create ?now ~memory () =
  let run (context : Tool.Context.t) args =
    let str key =
      Option.bind (Tool_args.string_opt args key) ~f:(fun s ->
        let s = String.strip s in
        Option.some_if (not (String.is_empty s)) s)
    in
    let now () =
      match now with
      | Some now -> now ()
      | None ->
        Time_float.of_span_since_epoch
          (Time_float.Span.of_sec (Eio.Time.now (Eio.Stdenv.clock context.env)))
    in
    let required key ~why =
      match str key with
      | Some s -> s
      | None ->
        raise
          (Tool_args.Invalid
             (sprintf
                "%s needs %s (%s)"
                (Tool_args.string args "action")
                key
                why))
    in
    let result =
      match Tool_args.string args "action" with
      | "save" ->
        let text = required "text" ~why:"the note" in
        Or_error.map
          (Memory.save memory ?title:(str "title") ~now:(now ()) text)
          ~f:(function
            | Added note -> "Saved " ^ header note
            | Replaced note -> "Replaced " ^ header note)
      | "recall" -> recall memory (str "query")
      | "forget" ->
        let id = required "id" ~why:"the note's id or title" in
        Or_error.map (Memory.forget memory id) ~f:(fun note ->
          "Forgot " ^ short note)
      | action ->
        Or_error.errorf "unknown action %S: use save, recall or forget" action
    in
    match result with
    | Ok text -> Tool.Result.ok text
    | Error e -> Tool.Result.error (Error.to_string_hum e)
  in
  { Tool.spec; run }
;;
