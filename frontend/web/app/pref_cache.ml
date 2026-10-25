open! Core
module Prefs = Prigh_protocol.Prefs
module Key = Prigh_web.Pref_sync.Key

let key = "prigh-web.prefs"
let theme_key = "prigh-web.theme"

module Stored = struct
  type t =
    { owner : string
    ; legacy : bool
    ; prefs : (string * Jsonaf.t) list
    }

  let of_string text =
    match Jsonaf.parse text with
    | Ok json ->
      let field name = Jsonaf.member name json in
      (match field "owner", field "prefs" with
       | Some (`String owner), Some (`Object prefs) ->
         Some
           { owner
           ; legacy =
               (match field "legacy" with
                | Some `True -> true
                | _ -> false)
           ; prefs
           }
       | _ -> None)
    | Error _ -> None
  ;;

  let to_string t =
    Jsonaf.to_string
      (`Object
          [ "owner", `String t.owner
          ; "legacy", if t.legacy then `True else `False
          ; "prefs", `Object t.prefs
          ])
  ;;
end

(* What pages from before kept, by its key then, as preferences. *)
let legacy_keys =
  [ "prigh-web.notify", Key.notify, (function
      | "off" -> Some `False
      | _ -> Some `True)
  ; "prigh-web.history", Key.history, (fun text ->
      match Jsonaf.parse text with
      | Ok (`Array items) ->
        Some
          (`Array
              (List.filter items ~f:(function
                 | `String _ -> true
                 | _ -> false)))
      | _ -> None)
  ; "prigh-web.terminal-open", Key.terminal_open, (fun _ -> Some `True)
  ]
  @ List.map Prigh_web.Resize.Handle.all ~f:(fun h ->
    let pref = Prigh_web.Resize.Handle.pref_key h in
    ( "prigh-web." ^ String.chop_prefix_exn pref ~prefix:"web."
    , pref
    , fun text -> Option.map (Int.of_string_opt text) ~f:(fun n -> `Number (Int.to_string n)) ))
;;

let changes prefs =
  List.map prefs ~f:(fun (key, value) -> { Prefs.Change.key; value = Some value })
;;

let load (storage : Login.Storage.t) ~owner =
  match Option.bind (storage.get key) ~f:Stored.of_string with
  | Some stored when String.equal stored.owner owner ->
    changes stored.prefs, stored.legacy
  | Some _ -> [], false
  | None ->
    let prefs =
      List.filter_map legacy_keys ~f:(fun (old, pref, decode) ->
        let value = Option.bind (storage.get old) ~f:decode in
        storage.remove old;
        Option.map value ~f:(fun value -> pref, value))
    in
    let legacy = not (List.is_empty prefs) in
    storage.set key (Stored.to_string { owner; legacy; prefs });
    changes prefs, legacy
;;

let save (storage : Login.Storage.t) ~owner (changes : Prefs.Change.t list) =
  let prefs =
    match Option.bind (storage.get key) ~f:Stored.of_string with
    | Some stored when String.equal stored.owner owner -> stored.prefs
    | Some _ | None -> []
  in
  let prefs =
    List.fold changes ~init:prefs ~f:(fun prefs { key; value } ->
      let rest = List.Assoc.remove prefs key ~equal:String.equal in
      match value with
      | Some value -> rest @ [ key, value ]
      | None -> rest)
  in
  storage.set key (Stored.to_string { owner; legacy = false; prefs })
;;

let save_theme (storage : Login.Storage.t) theme =
  let obj pairs = `Object (List.map pairs ~f:(fun (k, v) -> k, `String v)) in
  storage.set
    theme_key
    (Jsonaf.to_string
       (`Object
           [ "attrs", obj (Prigh_web.Theme.attributes theme)
           ; "vars", obj (Prigh_web.Theme.vars theme)
           ]))
;;
