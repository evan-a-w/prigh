open! Core
open! Import

let spec =
  { Tool_spec.name = Goal.tool_name
  ; description =
      "Mark the session's goal (set by the user with /goal) as achieved, \
       ending the automatic continuations. Call it only when current evidence \
       proves every requirement of the goal is met, not because you are \
       stopping or stuck."
  ; parameters =
      Tool_args.schema
        ~required:[ "summary" ]
        [ "summary", `String, "One or two sentences on what was achieved" ]
  ; parallel_safe = true
  ; destructive = false
  ; on_host = false
  ; freeform = None
  }
;;

let create ~complete =
  { Tool.spec
  ; run =
      (fun _context args ->
        let summary = String.strip (Tool_args.string args "summary") in
        match complete ~summary with
        | Ok () -> Tool.Result.ok "goal marked complete"
        | Error e -> Tool.Result.error (Error.to_string_hum e))
  }
;;
