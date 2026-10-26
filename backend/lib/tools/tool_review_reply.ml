open! Core
open! Import

let name = "reply_to_comment"

let spec =
  { Tool_spec.name
  ; description =
      "Reply to one of the user's review comments (sent to you as <comment> \
       blocks); the reply shows threaded under the comment. To propose a \
       change for the user to accept or reject instead of making it yourself, \
       give edits on the comment's file (exact text replacements, as for the \
       edit tool); it is applied only if they accept it. Set resolve when the \
       comment needs nothing more."
  ; parameters =
      Tool_args.schema
        ~required:[ "id"; "text" ]
        [ "id", `String, "The comment's id, e.g. c3"
        ; "text", `String, "Your reply (markdown)"
        ; ( "edits"
          , `Array
              (Tool_args.schema
                 ~required:[ "old_text"; "new_text" ]
                 [ ( "old_text"
                   , `String
                   , "Exact text to replace (must be unique in the file)" )
                 ; "new_text", `String, "Replacement text"
                 ])
          , "A change to propose on the comment's file" )
        ; "resolve", `Boolean, "Mark the comment resolved"
        ]
  ; parallel_safe = true
  ; destructive = Never
  ; on_host = false
  ; freeform = None
  }
;;

let create ~reply =
  { Tool.spec
  ; run =
      (fun _context args ->
        let id = String.strip (Tool_args.string args "id") in
        let text = Tool_args.string args "text" in
        let edits =
          List.map
            (Option.value (Tool_args.list_opt args "edits") ~default:[])
            ~f:(fun e ->
              { Review_comment.Edit.old_text = Tool_args.string e "old_text"
              ; new_text = Tool_args.string e "new_text"
              })
        in
        let resolve =
          Option.value (Tool_args.bool_opt args "resolve") ~default:false
        in
        match reply ~id ~text ~edits ~resolve with
        | Error e -> Tool.Result.error (Error.to_string_hum e)
        | Ok (c : Review_comment.t) ->
          Tool.Result.ok
            (sprintf
               "replied to %s%s%s"
               c.id
               (if List.is_empty edits
                then ""
                else ", proposing the change for the user to accept")
               (match c.status with
                | Resolved -> "; it is resolved"
                | Open -> "")))
  }
;;
