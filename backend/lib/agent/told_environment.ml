open! Core
open! Import

type t =
  { host : string
  ; cwd : string
  ; tools : string list
  ; plan_mode : bool
  ; client : Client_kind.t option [@sexp.option] [@jsonaf.option]
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
  List.filter_opt [ host; cwd; tools; plan_mode; client ]
;;
