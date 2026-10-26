open! Core
open! Import

module Source = struct
  type t =
    | Command
    | Argument of Commands.Spec.t
    | Path
    | Directory of { host : string option }
    | Skill
    | Snippet
  [@@deriving sexp_of, equal]
end

type t =
  { source : Source.t
  ; prefix : string
  ; line : int
  ; start : int (* byte offset of [prefix] within [line] *)
  ; items : Picker.Item.t list
  ; selected : int
  ; navigated : bool
  }
[@@deriving sexp_of]

let source t = t.source
let prefix t = t.prefix
let items t = t.items
let selected t = t.selected
let navigated t = t.navigated
let set_items t items = { t with items; selected = 0 }

let accepts_on_enter t =
  match t.source with
  | Source.Command -> true
  | Argument { argument = Some Commands.Argument.Server; _ } ->
    (* A partly typed address is one too ([10.0.0.5:7] is not
       [10.0.0.5:7777]): Tab completes it. *)
    t.navigated
  | Argument _ | Path | Directory _ | Skill | Snippet ->
    t.navigated || not (String.is_empty t.prefix)
;;

let clamp t =
  let n = List.length t.items in
  { t with selected = Int.max 0 (Int.min t.selected (n - 1)) }
;;

let up t = clamp { t with selected = t.selected - 1; navigated = true }
let down t = clamp { t with selected = t.selected + 1; navigated = true }
let selected_item t = List.nth t.items t.selected

let rank ~prefix items =
  Fuzzy.rank ~query:prefix items ~key:(fun i -> i.Picker.Item.search)
;;

let command_items ~prefix =
  Commands.all
  |> Fuzzy.rank ~query:prefix ~key:(fun (c : Commands.Spec.t) -> c.name)
  |> List.map ~f:(fun (c : Commands.Spec.t) ->
    Picker.Item.create ~id:c.name ~detail:c.help ~search:c.name c.name)
;;

let has_whitespace s = String.exists s ~f:Char.is_whitespace

let compute_command ~line ~line_index =
  match String.chop_prefix line ~prefix:"/" with
  | Some rest when not (has_whitespace rest) ->
    let prefix = rest in
    let items = command_items ~prefix in
    Option.some_if
      (not (List.is_empty items))
      { source = Source.Command
      ; prefix
      ; line = line_index
      ; start = 1
      ; items
      ; selected = 0
      ; navigated = false
      }
  | _ -> None
;;

let skill_prefix = "/skill:"

let skill_items ~prefix (skills : P.Skill.t list) =
  Fuzzy.rank ~query:prefix skills ~key:(fun (s : P.Skill.t) -> s.name)
  |> List.map ~f:(fun (s : P.Skill.t) ->
    Picker.Item.create ~id:s.name ~detail:s.description ~search:s.name s.name)
;;

(* [skills] is [None] while they are being fetched: the completion opens empty
   and the app fills it in. It stays open, empty, when there are none, so that
   Enter runs [/skill:] (the picker, which says where to add skills). *)
let compute_skill ~line ~line_index ~skills =
  match String.chop_prefix line ~prefix:skill_prefix with
  | Some prefix when not (has_whitespace prefix) ->
    let t =
      { source = Source.Skill
      ; prefix
      ; line = line_index
      ; start = String.length skill_prefix
      ; items = []
      ; selected = 0
      ; navigated = false
      }
    in
    (match skills with
     | None -> Some t
     | Some skills ->
       let items = skill_items ~prefix skills in
       Option.some_if
         ((not (List.is_empty items)) || String.is_empty prefix)
         { t with items })
  | _ -> None
;;

let thinking_items () =
  List.map Commands.thinking_levels ~f:(fun level ->
    Picker.Item.create ~id:level level)
;;

let verbosity_items () =
  List.map [ "quiet"; "normal"; "verbose" ] ~f:(fun name ->
    Picker.Item.create ~id:name (String.capitalize name))
;;

let confirm_items () =
  List.map [ "on"; "off" ] ~f:(fun name ->
    Picker.Item.create ~id:name (String.capitalize name))
;;

let mode_items () =
  List.map Session_mode.names ~f:(fun name -> Picker.Item.create ~id:name name)
;;

let notify_items () =
  List.map
    [ "off", "no notifications"
    ; "bell", "ring the terminal bell"
    ; "desktop", "a desktop notification (OSC 9 or 777)"
    ]
    ~f:(fun (name, detail) ->
      Picker.Item.create ~id:name ~detail (String.capitalize name))
;;

let provider_items (auth : P.Auth_status.t list) =
  List.map auth ~f:(fun (s : P.Auth_status.t) ->
    Picker.Item.create
      ~id:s.provider
      ~detail:s.provider
      ~search:(s.name ^ " " ^ s.provider)
      s.name)
;;

let model_items ~logged_in (models : P.Model.t list) =
  List.map models ~f:(fun (model : P.Model.t) ->
    Picker.Item.create
      ~id:model.key
      ~detail:model.key
      ~search:(model.name ^ " " ^ model.key)
      ~dimmed:(not (logged_in model.provider))
      model.name)
;;

(* By name (else the first prompt), active ones first; accepting inserts the
   id, which every session command takes. *)
let session_items (sessions : P.Session_summary.t list) =
  let active, archived =
    List.partition_tf sessions ~f:(fun (s : P.Session_summary.t) ->
      not s.archived)
  in
  List.map (active @ archived) ~f:(fun (s : P.Session_summary.t) ->
    let first = Text_width.truncate (P.Session_summary.blurb s) ~width:60 in
    let label = Option.value s.name ~default:first in
    let detail =
      String.concat
        ~sep:" · "
        ((if s.archived then [ "archived" ] else [])
         @ Option.value_map s.name ~default:[] ~f:(fun _ -> [ first ])
         @ [ s.cwd ])
    in
    Picker.Item.create
      ~id:s.id
      ~detail
      ~dimmed:s.archived
      ~search:
        (String.concat ~sep:" " [ label; first; s.id; s.cwd; s.created_at ])
      label)
;;

(* The sessions a command can act on: [/archive] the active ones,
   [/unarchive] the archived ones, [/delete] all but the current one. *)
let sessions_for (spec : Commands.Spec.t) ~current_session sessions =
  List.filter sessions ~f:(fun (s : P.Session_summary.t) ->
    match spec.name with
    | "archive" -> not s.archived
    | "unarchive" -> s.archived
    | "delete" -> not (Option.exists current_session ~f:(String.equal s.path))
    | _ -> true)
;;

let mcp_items () =
  [ Picker.Item.create
      ~id:"reconnect"
      ~search:"reconnect"
      ~detail:"restart failed or stopped servers"
      "reconnect"
  ]
;;

let snippet_items (snippets : P.Snippet.t list) =
  List.map snippets ~f:(fun (s : P.Snippet.t) ->
    Picker.Item.create ~id:s.name ~detail:(P.Snippet.first_line s) s.name)
  @ [ Picker.Item.create
        ~id:"add"
        ~detail:"add NAME [TEXT]: save a snippet"
        "add"
    ; Picker.Item.create ~id:"rm" ~detail:"rm NAME: delete a snippet" "rm"
    ]
;;

let server_items (server : Server_link.t option) =
  let address detail = function
    | Some address -> [ Picker.Item.create ~id:address ~detail address ]
    | None -> []
  in
  match server with
  | None -> []
  | Some server ->
    address "the current server" server.address
    @ (if Option.equal String.equal server.saved server.address
       then []
       else address "the saved server" server.saved)
    @ [ Picker.Item.create
          ~id:"local"
          ~detail:"a backend started here, like prigh-tui -local"
          "local"
      ]
;;

let pairs items =
  List.map items ~f:(fun (id, detail) -> Picker.Item.create ~id ~detail id)
;;

let argument_items
      ~kind
      ~models
      ~auth
      ~forge
      ~sessions
      ~snippets
      ~logged_in
      ~server
  =
  match (kind : Commands.Argument.t) with
  | Model | Models -> Some (model_items ~logged_in models)
  | Thinking -> Some (thinking_items ())
  | Verbosity -> Some (verbosity_items ())
  | Confirm | Plan -> Some (confirm_items ())
  | Mode -> Some (mode_items ())
  | Notify -> Some (notify_items ())
  | Login ->
    Some
      (provider_items auth
       @ [ Picker.Item.create
             ~id:"custom"
             ~detail:"add an OpenAI-compatible endpoint"
             "custom"
         ]
       @ pairs (Forge_accounts.login_completions forge))
  | Logout ->
    Some (provider_items auth @ pairs (Forge_accounts.logout_completions forge))
  | Pr -> Some (pairs Pr_command.subcommands)
  | Sessions | Session_list ->
    Some (Option.value_map sessions ~default:[] ~f:session_items)
  | Mcp -> Some (mcp_items ())
  | Snippet -> Some (snippet_items snippets)
  | Remind | Worktree | Review | Agents -> None
  | Server -> Some (server_items server)
  | Budget ->
    Some
      [ Picker.Item.create
          ~id:"off"
          ~detail:"no budget: turns start whatever the session has cost"
          "off"
      ]
  | Goal ->
    Some
      [ Picker.Item.create ~id:"off" ~detail:"clear the goal" "off"
      ; Picker.Item.create
          ~id:"resume"
          ~detail:"make a stopped goal active again"
          "resume"
      ]
  | Resume ->
    Some
      [ Picker.Item.create ~id:"now" ~detail:"retry the run at once" "now"
      ; Picker.Item.create
          ~id:"cancel"
          ~detail:"stay stopped until you send a message"
          "cancel"
      ]
  | Path | Directory | Default_directory | Skill -> None
;;

let argument_start ~line ~name =
  let after = String.length name + 1 in
  let rec skip i =
    if i < String.length line && Char.is_whitespace line.[i]
    then skip (i + 1)
    else i
  in
  skip after
;;

let words s =
  String.split_on_chars s ~on:[ ' '; '\t' ]
  |> List.filter ~f:(Fn.non String.is_empty)
;;

(* Lists of words ([/fallback]'s models, [/delete]'s sessions): the word under
   the cursor, among the [candidates] not listed yet ([first], e.g. [off],
   only as the only word). *)
let compute_words ~spec ~line ~col ~line_index ~start ~first ~candidates =
  let col = Int.max start (Int.min col (String.length line)) in
  let word_start =
    match
      String.rfindi (String.prefix line col) ~f:(fun _ c ->
        Char.is_whitespace c)
    with
    | Some i -> Int.max start (i + 1)
    | None -> start
  in
  let word_end =
    match String.lfindi line ~pos:col ~f:(fun _ c -> Char.is_whitespace c) with
    | Some i -> i
    | None -> String.length line
  in
  (* The whole word, so that accepting replaces all of it. *)
  let prefix = String.sub line ~pos:word_start ~len:(word_end - word_start) in
  let others =
    words (String.sub line ~pos:start ~len:(word_start - start))
    @ words (String.drop_prefix line word_end)
  in
  let first = if List.is_empty others then first else [] in
  let listed key = List.mem others key ~equal:String.equal in
  let items =
    first
    @ List.filter candidates ~f:(fun (item : Picker.Item.t) ->
      not (listed item.id))
    |> rank ~prefix
  in
  Option.some_if
    (not (List.is_empty items))
    { source = Source.Argument spec
    ; prefix
    ; line = line_index
    ; start = word_start
    ; items
    ; selected = 0
    ; navigated = false
    }
;;

let compute_argument
      ~line
      ~col
      ~line_index
      ~models
      ~auth
      ~forge
      ~sessions
      ~snippets
      ~reminders
      ~agents
      ~worktrees
      ~current_session
      ~logged_in
      ~default_dir_host
      ~server
  =
  match String.chop_prefix line ~prefix:"/" with
  | None -> None
  | Some body ->
    (match String.lsplit2 body ~on:' ' with
     | None -> None
     | Some (name, rest) ->
       (match Commands.find name with
        | None -> None
        | Some spec ->
          let sessions =
            Option.map sessions ~f:(sessions_for spec ~current_session)
          in
          (match spec.argument with
           | None -> None
           | Some kind ->
             let prefix = String.strip rest in
             let start = argument_start ~line ~name in
             (match kind with
              | Models ->
                compute_words
                  ~spec
                  ~line
                  ~col
                  ~line_index
                  ~start
                  ~first:
                    [ Picker.Item.create
                        ~id:"off"
                        ~search:"off"
                        ~detail:
                          "no fallback: a model whose usage runs out stops the \
                           run"
                        "off"
                    ]
                  ~candidates:(model_items ~logged_in models)
              | Session_list when Option.is_some sessions ->
                compute_words
                  ~spec
                  ~line
                  ~col
                  ~line_index
                  ~start
                  ~first:[]
                  ~candidates:
                    (Option.value_map sessions ~default:[] ~f:session_items)
              | Commands.Argument.Path | Directory | Default_directory ->
                (* Filled in asynchronously by the platform. *)
                Some
                  { source =
                      (match kind with
                       | Directory -> Source.Directory { host = None }
                       | Default_directory ->
                         Source.Directory { host = default_dir_host }
                       | _ -> Source.Path)
                  ; prefix
                  ; line = line_index
                  ; start
                  ; items = []
                  ; selected = 0
                  ; navigated = false
                  }
              | (Remind | Agents) as kind ->
                let rest = String.drop_prefix line start in
                Option.bind
                  (match kind with
                   | Agents -> Agents_command.complete ~agents rest
                   | _ -> Remind_command.complete ~reminders rest)
                  ~f:(fun (offset, candidates) ->
                    let prefix = String.drop_prefix rest offset in
                    let items =
                      List.map candidates ~f:(fun (id, detail) ->
                        Picker.Item.create ~id ~search:id ~detail id)
                      |> rank ~prefix
                    in
                    Option.some_if
                      (not (List.is_empty items))
                      { source = Source.Argument spec
                      ; prefix
                      ; line = line_index
                      ; start = start + offset
                      ; items
                      ; selected = 0
                      ; navigated = false
                      })
              | (Worktree | Review) as kind ->
                (* While [worktrees] are fetched, a completion after the
                   subcommand opens empty and the app fills it in. *)
                let rest = String.drop_prefix line start in
                let known = Option.value worktrees ~default:[] in
                Option.bind
                  (match kind with
                   | Review ->
                     Review_command.complete ~worktrees:known ~branches:[] rest
                   | _ -> Worktree_command.complete ~worktrees:known rest)
                  ~f:(fun (offset, candidates) ->
                    let prefix = String.drop_prefix rest offset in
                    let items =
                      List.map candidates ~f:(fun (id, detail) ->
                        Picker.Item.create ~id ~search:id ~detail id)
                      |> rank ~prefix
                    in
                    Option.some_if
                      ((not (List.is_empty items))
                       || (Option.is_none worktrees && offset > 0))
                      { source = Source.Argument spec
                      ; prefix
                      ; line = line_index
                      ; start = start + offset
                      ; items
                      ; selected = 0
                      ; navigated = false
                      })
              | (Sessions | Session_list) when Option.is_none sessions ->
                (* Loading; the app fetches the session list. *)
                Some
                  { source = Source.Argument spec
                  ; prefix
                  ; line = line_index
                  ; start
                  ; items = []
                  ; selected = 0
                  ; navigated = false
                  }
              | _ ->
                let items =
                  Option.value
                    (argument_items
                       ~kind
                       ~models
                       ~auth
                       ~forge
                       ~sessions
                       ~snippets
                       ~logged_in
                       ~server)
                    ~default:[]
                in
                let items =
                  match kind with
                  | Snippet when String.exists prefix ~f:Char.is_whitespace ->
                    []
                  | Goal | Budget ->
                    (* Free text: only an exact start of a keyword offers
                       it, so Enter sends what was typed. *)
                    List.filter items ~f:(fun (i : Picker.Item.t) ->
                      String.is_prefix i.id ~prefix)
                  | _ -> rank ~prefix items
                in
                Option.some_if
                  (not (List.is_empty items))
                  { source = Source.Argument spec
                  ; prefix
                  ; line = line_index
                  ; start
                  ; items
                  ; selected = 0
                  ; navigated = false
                  }))))
;;

let compute_at ~line ~col ~line_index =
  let before = String.prefix line col in
  let start = ref 0 in
  String.iteri before ~f:(fun i c ->
    if Char.is_whitespace c then start := i + 1);
  let after = String.drop_prefix line !start in
  let word =
    match String.lfindi after ~f:(fun _ c -> Char.is_whitespace c) with
    | Some j -> String.prefix after j
    | None -> after
  in
  match String.chop_prefix word ~prefix:"@" with
  | Some path ->
    Some
      { source = Source.Path
      ; prefix = path
      ; line = line_index
      ; start = !start + 1
      ; items = []
      ; selected = 0
      ; navigated = false
      }
  | None -> None
;;

(* [#name] at the start of a word, the name starting with a letter or digit
   so that [# Title] and [##] stay plain text; nothing when no snippet
   matches (e.g. [#123]). *)
let compute_hash ~line ~col ~line_index ~snippets =
  let start =
    match
      String.rfindi (String.prefix line col) ~f:(fun _ c ->
        Char.is_whitespace c)
    with
    | Some i -> i + 1
    | None -> 0
  in
  let word =
    let after = String.drop_prefix line start in
    match String.lfindi after ~f:(fun _ c -> Char.is_whitespace c) with
    | Some j -> String.prefix after j
    | None -> after
  in
  match String.chop_prefix word ~prefix:"#" with
  | Some name when String.length name > 0 && Char.is_alphanum name.[0] ->
    let items =
      Fuzzy.rank ~query:name snippets ~key:(fun (s : P.Snippet.t) -> s.name)
      |> List.map ~f:(fun (s : P.Snippet.t) ->
        Picker.Item.create
          ~id:s.name
          ~detail:(P.Snippet.first_line s)
          ~search:s.name
          ("#" ^ s.name))
    in
    Option.some_if
      (not (List.is_empty items))
      { source = Source.Snippet
      ; prefix = name
      ; line = line_index
      ; start = start + 1
      ; items
      ; selected = 0
      ; navigated = false
      }
  | _ -> None
;;

let directory ~host ~text =
  { source = Source.Directory { host = Some host }
  ; prefix = text
  ; line = 0
  ; start = 0
  ; items = []
  ; selected = 0
  ; navigated = false
  }
;;

let compute
      ~current_session
      ~line
      ~col
      ~line_index
      ~models
      ~auth
      ~forge
      ~sessions
      ~skills
      ~snippets
      ~reminders
      ~agents
      ~worktrees
      ~logged_in
      ~default_dir_host
      ~server
  =
  match
    Option.first_some
      (compute_skill ~line ~line_index ~skills)
      (compute_command ~line ~line_index)
  with
  | Some _ as t -> t
  | None ->
    (match
       compute_argument
         ~line
         ~col
         ~line_index
         ~models
         ~auth
         ~forge
         ~sessions
         ~snippets
         ~reminders
         ~agents
         ~worktrees
         ~current_session
         ~logged_in
         ~default_dir_host
         ~server
     with
     | Some _ as t -> t
     | None ->
       Option.first_some
         (compute_at ~line ~col ~line_index)
         (compute_hash ~line ~col ~line_index ~snippets))
;;

let accept t ~editor_text =
  let lines = String.split editor_text ~on:'\n' in
  let line = Option.value (List.nth lines t.line) ~default:"" in
  let replacement =
    match selected_item t with
    | None -> t.prefix
    | Some item ->
      (match t.source with
       | Source.Command ->
         (* [skill:] continues with the skill's name *)
         if String.is_suffix item.id ~suffix:":" then item.id else item.id ^ " "
       | Skill -> item.id ^ " "
       | Argument _ | Path | Directory _ | Snippet -> item.id)
  in
  let before = String.prefix line t.start in
  let after = String.drop_prefix line (t.start + String.length t.prefix) in
  let line = before ^ replacement ^ after in
  String.concat
    ~sep:"\n"
    (List.mapi lines ~f:(fun i text -> if i = t.line then line else text))
;;

let insert_snippet t editor (snippet : P.Snippet.t) =
  let line = Option.value (List.nth (Editor.lines editor) t.line) ~default:"" in
  let chars s = List.length (Text_width.uchars s) in
  Editor.replace
    editor
    ~at:{ line = t.line; col = chars (String.prefix line (t.start - 1)) }
    ~len:(1 + chars t.prefix)
    snippet.text
;;
