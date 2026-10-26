open! Core
open! Import

module Key = struct
  let theme = "web.theme"
  let accent = "web.accent"
  let notify = "web.notify"
  let verbosity = "web.verbosity"
  let terminal_open = "web.terminal-open"
  let folded = "web.sidebar-folded"
  let review = "web.review"
  let history = "history"
  let migrated = "web.migrated"

  let all =
    [ theme
    ; accent
    ; notify
    ; verbosity
    ; terminal_open
    ; folded
    ; review
    ; history
    ; migrated
    ]
    @ List.map Resize.Handle.all ~f:Resize.Handle.pref_key
  ;;
end

let history_max = 500

module Remote = struct
  type t =
    | Unknown
    | Kept
    | Local
  [@@deriving sexp_of, equal]
end

type t =
  { remote : Remote.t
  ; legacy : Prefs.Change.t list
  }
[@@deriving sexp_of]

let init = { remote = Unknown; legacy = [] }

let saving t =
  match t.remote with
  | Unknown | Kept -> true
  | Local -> false
;;

let loaded t (server : Prefs.Change.t list) =
  let has key =
    List.exists server ~f:(fun (c : Prefs.Change.t) ->
      String.equal c.key key && Option.is_some c.value)
  in
  let upload =
    if has Key.migrated
    then []
    else (
      (* Keys the backend has (the TUI's history) stay its. *)
      let legacy =
        List.filter t.legacy ~f:(fun (c : Prefs.Change.t) ->
          Option.is_some c.value && not (has c.key))
      in
      legacy @ [ { key = Key.migrated; value = Some `True } ])
  in
  (* Everything we know of: what the backend lacks is the default. *)
  let applied =
    List.map Key.all ~f:(fun key ->
      match
        List.find server ~f:(fun (c : Prefs.Change.t) -> String.equal c.key key)
      with
      | Some c -> c
      | None ->
        (match
           List.find upload ~f:(fun (c : Prefs.Change.t) ->
             String.equal c.key key)
         with
         | Some c -> c
         | None -> { Prefs.Change.key; value = None }))
  in
  { remote = Kept; legacy = [] }, applied, upload
;;
