open! Core
open! Import

let name = "custom_instructions"
let changes action = not (String.equal action "list")

let spec =
  { Tool_spec.name
  ; parallel_safe = false
  ; on_host = false
  ; freeform = None
  ; native = None
  ; destructive =
      Depends
        (fun args ->
          Option.value_map
            (Tool_args.string_opt args "action")
            ~default:true
            ~f:changes)
  ; description =
      "Change the user's custom instructions: standing ones in every session's \
       context, shown with their ids. Use it more than memory, but sparingly: \
       when the user states a lasting preference, a fact about themselves or a \
       standing instruction that should hold in every session and is not \
       covered yet, add it, or edit the note that covers it (prefer editing \
       over adding a near-duplicate). Never for task details or one-off \
       requests. Tell the user briefly when you change one. add (text, \
       optional title); edit (id, text and/or title); remove (id); list (when \
       they are no longer in your context)."
  ; parameters =
      Tool_args.schema
        ~required:[ "action" ]
        [ "action", `String, "add, edit, remove or list"
        ; ( "text"
          , `String
          , "add, edit: the instruction, short, self-contained, in the user's \
             voice where natural" )
        ; "title", `String, "add, edit: a few words naming it (optional)"
        ; "id", `String, "edit, remove: its id (ci1, ...)"
        ]
  }
;;

let header (note : Custom_instructions.Note.t) =
  String.concat ~sep:" · " (note.id :: Option.to_list note.title)
;;

let off =
  "the user does not let you change their custom instructions (Settings, \
   Custom instructions in prigh-web; /instructions on in the terminal): \
   suggest the change to them instead."
;;

let create ?now ~custom_instructions () =
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
    let told (note : Custom_instructions.Note.t) instruction =
      context.emit (Custom_instruction_set { id = note.id; instruction })
    in
    let set note =
      told note (Some (Custom_instructions.to_model note));
      note
    in
    let result =
      match Tool_args.string args "action" with
      | _ when not (Custom_instructions.enabled custom_instructions) ->
        Or_error.error_string off
      | "add" ->
        let text = required "text" ~why:"the instruction" in
        Or_error.map
          (Custom_instructions.add
             custom_instructions
             ?title:(str "title")
             ~now:(now ())
             text)
          ~f:(fun note -> "Added " ^ header (set note))
      | "edit" ->
        let id = required "id" ~why:"the instruction's id" in
        (match
           Tool_args.string_opt args "text", Tool_args.string_opt args "title"
         with
         | None, None ->
           Or_error.error_string "edit needs text or title (or both)"
         | text, title ->
           Or_error.map
             (Custom_instructions.edit
                custom_instructions
                ~id
                ?title
                ?text
                ~now:(now ())
                ())
             ~f:(fun note -> "Edited " ^ header (set note)))
      | "remove" ->
        let id = required "id" ~why:"the instruction's id" in
        Or_error.map
          (Custom_instructions.remove custom_instructions id)
          ~f:(fun note ->
            told note None;
            "Removed " ^ header note)
      | "list" ->
        Or_error.map
          (Custom_instructions.notes custom_instructions)
          ~f:(function
          | [] -> "There are no custom instructions."
          | notes ->
            Option.value
              (Custom_instruction.section
                 (List.map notes ~f:Custom_instructions.to_model))
              ~default:"")
      | action ->
        Or_error.errorf
          "unknown action %S: use add, edit, remove or list"
          action
    in
    match result with
    | Ok text -> Tool.Result.ok text
    | Error e -> Tool.Result.error (Error.to_string_hum e)
  in
  { Tool.spec; run }
;;
