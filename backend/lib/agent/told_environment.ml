open! Core
open! Import

type t =
  { host : string
  ; cwd : string
  ; tools : string list
  ; plan_mode : bool
  ; client : Client_kind.t option [@sexp.option] [@jsonaf.option]
  ; user_info : string option [@sexp.option] [@jsonaf.option]
  ; custom_instructions : Custom_instruction.t list
        [@sexp.list] [@jsonaf.default []] [@jsonaf_drop_default.equal]
  }
[@@deriving sexp, jsonaf, equal]

let notes ~told now ~host_name =
  let note (kind : Injected.Note.Kind.t) text =
    Some { Injected.Note.kind; text }
  in
  let host =
    if String.equal told.host now.host || String.is_empty now.host
    then None
    else
      note
        (Host { name = host_name })
        (System_prompt.host_changed_note ~host:host_name)
  in
  let cwd =
    if String.equal told.cwd now.cwd
    then None
    else
      note (Cwd { dir = now.cwd }) (System_prompt.cwd_changed_note ~cwd:now.cwd)
  in
  let tools =
    let before = String.Set.of_list told.tools
    and after = String.Set.of_list now.tools in
    let added = Set.to_list (Set.diff after before)
    and removed = Set.to_list (Set.diff before after) in
    if List.is_empty added && List.is_empty removed
    then None
    else
      note
        (Tools { added; removed })
        (System_prompt.tools_changed_note ~added ~removed)
  in
  let plan_mode =
    if Bool.equal told.plan_mode now.plan_mode
    then None
    else
      note
        (Plan_mode { on = now.plan_mode })
        (System_prompt.plan_mode_note ~on:now.plan_mode)
  in
  let client =
    match now.client with
    | Some client
      when not (Option.equal Client_kind.equal told.client now.client) ->
      note (Client { client }) (System_prompt.client_note client)
    | Some _ | None -> None
  in
  let user_info =
    if Option.equal String.equal told.user_info now.user_info
    then None
    else
      note
        (User_info { cleared = Option.is_none now.user_info })
        (System_prompt.user_info_note now.user_info)
  in
  let custom_instructions =
    let told = told.custom_instructions
    and now = now.custom_instructions in
    Option.map (Custom_instruction.note ~told now) ~f:(fun text ->
      let changes = Custom_instruction.changes ~told now in
      let count f = List.count changes ~f in
      { Injected.Note.kind =
          Custom_instructions
            { added =
                count (function
                  | Added _ -> true
                  | _ -> false)
            ; edited =
                count (function
                  | Edited _ -> true
                  | _ -> false)
            ; removed =
                count (function
                  | Removed _ -> true
                  | _ -> false)
            }
      ; text
      })
  in
  List.filter_opt
    [ host; cwd; tools; plan_mode; client; user_info; custom_instructions ]
;;
