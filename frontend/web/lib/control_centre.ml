open! Core
open! Import
module Fuzzy = Prigh_ui.Fuzzy
module Session_search = Prigh_ui.Session_search

module Group = struct
  type t =
    | This_session
    | Recent
    | Settings
    | Sessions
    | Commands
  [@@deriving sexp_of, equal]

  let title = function
    | This_session -> "This session"
    | Recent -> "Recent sessions"
    | Settings -> "Settings"
    | Sessions -> "Sessions"
    | Commands -> "Commands"
  ;;
end

module Target = struct
  type t =
    | Setting of Control_setting.t
    | Value of
        { setting : Control_setting.t
        ; id : string
        }
    | Session of string
    | Command of string
  [@@deriving sexp_of, equal]
end

module Row = struct
  type t =
    { target : Target.t
    ; group : Group.t
    ; label : string
    ; matched : (int * int) list
    ; detail : string
    ; keys : string option
    ; marked : bool
    ; snippet : Session_hits.Snippet.t option
    }
  [@@deriving sexp_of, equal]
end

type t =
  { query : string
  ; selected : int
  }
[@@deriving sexp_of, equal]

let empty = { query = ""; selected = 0 }
let set_query _ query = { query; selected = 0 }

let move t delta ~count =
  { t with selected = Int.max 0 (Int.min (count - 1) (t.selected + delta)) }
;;

module Setting_entry = struct
  type t =
    { setting : Control_setting.t
    ; value : string
    ; values : Picker.Item.t list
    }
end

let ranges positions =
  List.fold positions ~init:[] ~f:(fun acc p ->
    match acc with
    | (start, len) :: rest when start + len = p -> (start, len + 1) :: rest
    | acc -> (p, 1) :: acc)
  |> List.rev
;;

(* A close enough match: the letters typed in at most a run per word typed,
   and one more (so "thnk" finds "thinking" but "old" not "approval
   model"). *)
let close ~query label =
  match Fuzzy.positions ~query label with
  | None -> false
  | Some positions ->
    List.length (ranges positions)
    <= List.length
         (String.split query ~on:' ' |> List.filter ~f:(Fn.non String.is_empty))
       + 1
;;

let common_commands = [ "new"; "settings"; "review"; "files"; "edit"; "help" ]
let recent = 5
let most_settings = 12
let most_sessions = 30
let most_commands = 8

let row
      ?keys
      ?(marked = false)
      ?snippet
      ?(matched = [])
      ~group
      ~detail
      target
      label
  =
  { Row.target; group; label; matched; detail; keys; marked; snippet }
;;

let setting_row ~group (e : Setting_entry.t) ~query =
  let label = Control_setting.name e.setting in
  row
    ~group
    ?keys:(Control_setting.keys e.setting)
    ~matched:
      (Option.value_map (Fuzzy.positions ~query label) ~default:[] ~f:ranges)
    ~detail:e.value
    (Setting e.setting)
    label
;;

let command_row ~query (spec : Slash.Spec.t) =
  let label = "/" ^ spec.name in
  row
    ~group:Commands
    ~matched:
      (Option.value_map (Fuzzy.positions ~query label) ~default:[] ~f:ranges)
    ~detail:spec.help
    (Command spec.name)
    label
;;

(* Each word typed starts a word of [text]: "new def" finds "default model
   for new sessions". *)
let word_prefixes ~query text =
  let words s =
    String.split_on_chars (String.lowercase s) ~on:[ ' '; '-'; '/'; '\'' ]
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  let text = words text in
  match words query with
  | [] -> false
  | query ->
    List.for_all query ~f:(fun q ->
      List.exists text ~f:(fun w -> String.is_prefix w ~prefix:q))
;;

(* The name by the fuzzy filter, or each word typed starting one of its
   words; the keywords only by their words' starts (so that a long list of
   them does not match every few letters typed), and below the name. *)
let setting_score ?(keywords = "") ~query label =
  let by_words =
    if word_prefixes ~query label
    then Some (1500 - String.length label)
    else
      Option.some_if
        (word_prefixes ~query (label ^ " " ^ keywords))
        (1100 - String.length label)
  in
  Option.merge
    (Option.filter (Fuzzy.score ~query label) ~f:(fun _ -> close ~query label))
    by_words
    ~f:Int.max
;;

let settings_found ~query (settings : Setting_entry.t list) =
  let rows =
    List.concat_map settings ~f:(fun e ->
      let label = Control_setting.name e.setting in
      let own =
        setting_score
          ~keywords:(Control_setting.keywords e.setting)
          ~query
          label
        |> Option.map ~f:(fun score ->
          score, setting_row ~group:Settings e ~query)
      in
      (* A value ranks under its setting when both match: Enter on "verbos"
         opens the setting, "verbosity verbose" picks the value. *)
      let below =
        Option.map (setting_score ~query label) ~f:(fun score -> score - 1)
      in
      let values =
        if Control_setting.direct e.setting
        then
          List.filter_map e.values ~f:(fun (item : Picker.Item.t) ->
            let label = Control_setting.short e.setting ^ ": " ^ item.label in
            Option.map (setting_score ~query label) ~f:(fun score ->
              ( Option.value_map below ~default:score ~f:(Int.min score)
              , row
                  ~group:Settings
                  ~marked:item.marked
                  ~matched:
                    (Option.value_map
                       (Fuzzy.positions ~query label)
                       ~default:[]
                       ~f:ranges)
                  ~detail:(if item.marked then "current" else item.detail)
                  (Value { setting = e.setting; id = item.id })
                  label )))
        else []
      in
      Option.to_list own @ values)
  in
  List.stable_sort rows ~compare:(fun (a, _) (b, _) -> Int.compare b a)
  |> Fn.flip List.take most_settings
  |> List.map ~f:snd
;;

let session_row ~group ~describe (r : Session_search.Result.t) =
  row
    ~group
    ~matched:r.title
    ?snippet:r.snippet
    ~detail:(describe r.session)
    (Session r.session.path)
    (Session_search.title r.session)
;;

let rows t ~settings ~sessions ?content ~current ~describe ~commands () =
  let query = String.strip t.query in
  if String.is_empty query
  then
    List.filter_map settings ~f:(fun (e : Setting_entry.t) ->
      Option.some_if
        (Control_setting.session e.setting)
        (setting_row ~group:This_session e ~query))
    @ (List.filter sessions ~f:(fun (s : Session_summary.t) ->
         not (Option.equal String.equal current (Some s.path) || s.archived))
       |> Fn.flip List.take recent
       |> List.map ~f:(fun session ->
         row
           ~group:Recent
           ~detail:(describe session)
           (Session session.path)
           (Session_search.title session)))
    @ List.filter_map common_commands ~f:(fun name ->
      List.find commands ~f:(fun (c : Slash.Spec.t) -> String.equal c.name name)
      |> Option.map ~f:(command_row ~query))
  else
    settings_found ~query settings
    @ (Session_search.sessions ?content ~query sessions
       |> Fn.flip List.take most_sessions
       |> List.map ~f:(session_row ~group:Sessions ~describe))
    @
    let by_name =
      Fuzzy.rank ~query commands ~key:(fun (c : Slash.Spec.t) -> "/" ^ c.name)
      |> List.filter ~f:(fun (c : Slash.Spec.t) -> close ~query ("/" ^ c.name))
    in
    (* Then those whose description has the words: "rename" finds /name. *)
    by_name
    @ List.filter commands ~f:(fun (c : Slash.Spec.t) ->
      word_prefixes ~query c.help
      && not (List.mem by_name c ~equal:Slash.Spec.equal))
    |> Fn.flip List.take most_commands
    |> List.map ~f:(command_row ~query)
;;
