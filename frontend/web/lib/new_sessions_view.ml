open! Core
open! Import
open Html
module Action = App.Action
open Settings_row

let waiting = "muted", "Waiting for the backend's settings…"

let model_name (m : App.Model.t) key =
  match
    List.find m.models ~f:(fun (model : Llm.t) -> String.equal model.key key)
  with
  | Some model -> model.name
  | None -> key
;;

let host_name (m : App.Model.t) id =
  Prigh_ui.Default_host_command.name
    ~hosts:(Option.value_map m.state ~default:[] ~f:(fun s -> s.hosts))
    id
;;

(* Back to the built-in, on a row whose default is set. *)
let reset ~name ~builtin ~set ~on_click =
  if set
  then
    button
      ~cls:"small ghost default-reset"
      ~title:(sprintf "Back to the built-in: %s" builtin)
      ~label:(sprintf "Reset %s" name)
      ~on_click
      [ icon Undo ]
  else Node.none
;;

let set_default ~inject change = inject (Action.Set_default change)

let model (m : App.Model.t) (config : Config.t option) ~inject =
  let key = Option.bind config ~f:(fun c -> c.default_model) in
  let current =
    Option.value_map key ~default:"The backend's choice" ~f:(model_name m)
  in
  let this =
    Option.map m.state ~f:(fun s ->
      if s.model.supports_thinking && not (String.equal s.thinking "off")
      then sprintf "%s · thinking %s" s.model.name (Thinking.label s.thinking)
      else s.model.name)
  in
  row
    ~cls:"default-model"
    ~name:"Model"
    ~about:
      (match key with
       | Some _ -> sprintf "New sessions start with %s." current
       | None ->
         "New sessions start with the backend's choice: a model of a provider \
          you are logged in to.")
    ?note:(Option.value_map config ~default:(Some waiting) ~f:(fun _ -> None))
    [ select
        ~cls:"default-model-choice"
        ~title:"Choose the model new sessions start with"
        ~label:("New sessions' model: " ^ current)
        ~disabled:(Option.is_none config)
        ~on_click:(inject (Action.Pick_default Model))
        current
    ; button
        ~cls:"small ghost default-model-this"
        ~title:
          (match this with
           | Some this ->
             sprintf
               "Start new sessions with %s, its thinking level too \
                (/change_default)"
               this
           | None -> "/change_default")
        ~disabled:(Option.is_none config || Option.is_none m.state)
        ~on_click:(inject (Action.Run "/change_default"))
        [ Node.text "This session’s" ]
    ; reset
        ~name:"Model"
        ~builtin:"the backend's choice"
        ~set:(Option.is_some key)
        ~on_click:(set_default ~inject (Model None))
    ]
;;

let thinking (config : Config.t option) ~inject =
  let set = Option.bind config ~f:(fun c -> c.default_thinking) in
  let current =
    Thinking.normalize (Option.value set ~default:New_default.builtin_thinking)
  in
  row
    ~cls:"default-thinking"
    ~name:"Thinking"
    ~about:
      "How hard new sessions think, on models that can. A model without the \
       level takes its nearest one."
    ?note:(Option.value_map config ~default:(Some waiting) ~f:(fun _ -> None))
    [ choice_list
        ~disabled:(Option.is_none config)
        ~label:"New sessions' thinking"
        (List.map Thinking.all ~f:(fun level ->
           ( String.capitalize (Thinking.label level)
           , Thinking.description level
           , sprintf "New sessions think at %s" (Thinking.label level)
           , Option.is_some config && String.equal level current
           , set_default ~inject (Thinking (Some level)) )))
    ; reset
        ~name:"Thinking"
        ~builtin:(Thinking.label New_default.builtin_thinking)
        ~set:(Option.is_some set)
        ~on_click:(set_default ~inject (Thinking None))
    ]
;;

let mode_label : Approval_mode.t -> string = function
  | Ask -> "Ask"
  | Auto_edit -> "Auto-edit"
  | Auto -> "Auto"
;;

(* The mode changes only for new sessions here; /confirm changes this
   session's too. Plan is a session's, not a default: the composer's chip. *)
let mode (config : Config.t option) ~inject =
  let current = Option.map config ~f:(fun c -> c.approval_mode) in
  row
    ~cls:"default-mode"
    ~name:"Mode"
    ~about:
      "What asks before it runs: everything (Ask), all but file edits \
       (Auto-edit) or nothing (Auto)."
    ?note:(Option.value_map current ~default:(Some waiting) ~f:(fun _ -> None))
    [ segmented
        ~disabled:(Option.is_none current)
        ~label:"New sessions' mode"
        (List.map Approval_mode.all ~f:(fun mode ->
           ( mode_label mode
           , sprintf "New sessions start in %s mode" (mode_label mode)
           , Option.equal Approval_mode.equal current (Some mode)
           , set_default ~inject (Mode mode) )))
    ; reset
        ~name:"Mode"
        ~builtin:(mode_label New_default.builtin_mode)
        ~set:
          (Option.exists current ~f:(fun mode ->
             not (Approval_mode.equal mode New_default.builtin_mode)))
        ~on_click:(set_default ~inject (Mode New_default.builtin_mode))
    ]
;;

let host (m : App.Model.t) (config : Config.t option) ~inject =
  let id = Option.bind config ~f:(fun c -> c.default_host) in
  let connected id =
    Option.exists m.state ~f:(fun s ->
      List.exists s.hosts ~f:(fun h -> String.equal h.id id))
  in
  let current = Option.value_map id ~default:"Where you are" ~f:(host_name m) in
  row
    ~cls:"default-host"
    ~name:"Tool host"
    ~about:
      (match id with
       | Some _ ->
         sprintf
           "New sessions you start here run their tools on %s, while it is \
            connected."
           current
       | None ->
         "New sessions you start here run their tools where the session you \
          are on does.")
    ?note:
      (match config, id with
       | None, _ -> Some waiting
       | Some _, Some id when not (connected id) ->
         Some
           ( "warm"
           , sprintf
               "%s is not connected: until it is, new sessions start where you \
                are."
               current )
       | Some _, _ -> None)
    [ select
        ~cls:"default-host-choice"
        ~title:"Choose the tool host new sessions start on (/default-host)"
        ~label:("New sessions' tool host: " ^ current)
        ~disabled:(Option.is_none config)
        ~on_click:(inject (Action.Pick_default Host))
        current
    ; reset
        ~name:"Tool host"
        ~builtin:"where you are"
        ~set:(Option.is_some id)
        ~on_click:(set_default ~inject (Host None))
    ]
;;

let dir (m : App.Model.t) (config : Config.t option) ~inject =
  let which = Default_fields.Which.Dir in
  let set = Option.bind config ~f:(fun c -> c.default_cwd) in
  let error = Default_fields.error m.default_fields which in
  let value =
    match Default_fields.draft m.default_fields which with
    | Some draft -> draft
    | None -> Option.value set ~default:""
  in
  row
    ~cls:"default-dir"
    ~name:"Starting directory"
    ~about:
      (match set with
       | Some dir -> sprintf "New sessions start in %s, on their tool host." dir
       | None ->
         "New sessions start in their tool host's working directory. ~/ is its \
          home.")
    ?note:
      (match error, config with
       | Some e, _ -> Some ("warm", e)
       | None, None -> Some waiting
       | None, Some _ -> None)
    [ Node.input
        ~attrs:
          [ Attr.id "default-dir"
          ; Attr.class_ "text-input default-dir-input"
          ; Attr.type_ "text"
          ; Attr.value value
          ; Attr.placeholder "The host's working directory"
          ; Attr.title "An absolute path, or ~/… under the host's home"
          ; Attr.create "aria-label" "New sessions' starting directory"
          ; Attr.create "aria-invalid" (Bool.to_string (Option.is_some error))
          ; Attr.create "autocomplete" "off"
          ; Attr.create "autocapitalize" "off"
          ; Attr.create "spellcheck" "false"
          ; (if Option.is_none config then Attr.disabled else Attr.empty)
          ; Attr.on_input (fun _ text ->
              inject (Action.Default_field_input (which, text)))
          ; Attr.on_change (fun _ _ ->
              inject (Action.Default_field_commit which))
          ]
        ()
    ; reset
        ~name:"Starting directory"
        ~builtin:"the host's working directory"
        ~set:(Option.is_some set)
        ~on_click:
          (Effect.Many
             [ inject (Action.Default_field_input (which, ""))
             ; inject (Action.Default_field_commit which)
             ])
    ]
;;

let fallback (m : App.Model.t) (config : Config.t option) ~inject =
  let models =
    Option.value_map config ~default:[] ~f:(fun c -> c.fallback_models)
  in
  let chip i key =
    Node.li
      ~attrs:[ Attr.class_ "fallback-model" ]
      [ span ~cls:"fallback-index" (Int.to_string (i + 1))
      ; span ~cls:"fallback-name" (model_name m key)
      ; button
          ~cls:"small ghost fallback-remove"
          ~title:(sprintf "Take %s out of the chain" (model_name m key))
          ~label:(sprintf "Remove %s" (model_name m key))
          ~on_click:
            (set_default
               ~inject
               (Fallback (List.filteri models ~f:(fun j _ -> j <> i))))
          [ icon Close ]
      ]
  in
  row
    ~cls:"default-fallback"
    ~wide:true
    ~name:"Fallback models"
    ~about:
      "When a model's usage runs out, the next in this list takes over the \
       conversation. Every session follows it, open ones too."
    ?note:
      (match config, models with
       | None, _ -> Some waiting
       | Some _, [] ->
         Some ("muted", "None: a model whose usage runs out stops.")
       | Some _, _ -> None)
    [ (match models with
       | [] -> Node.none
       | models ->
         Node.ol
           ~attrs:[ Attr.class_ "fallback-list" ]
           (List.mapi models ~f:chip))
    ; div
        ~cls:"fallback-actions"
        [ button
            ~cls:"small fallback-add"
            ~title:"Add a model to the end of the chain (/fallback MODEL…)"
            ~disabled:(Option.is_none config)
            ~on_click:(inject (Action.Pick_default Fallback))
            [ icon Plus; Node.text "Add" ]
        ; reset
            ~name:"Fallback models"
            ~builtin:"none"
            ~set:(not (List.is_empty models))
            ~on_click:(set_default ~inject (Fallback []))
        ]
    ]
;;

let budget (m : App.Model.t) (config : Config.t option) ~inject =
  let which = Default_fields.Which.Budget in
  let set = Option.bind config ~f:(fun c -> c.max_budget_usd) in
  let error = Default_fields.error m.default_fields which in
  let on = Option.is_some set in
  let value =
    match Default_fields.draft m.default_fields which, set with
    | Some draft, _ -> draft
    | None, Some usd -> sprintf "%.2f" usd
    | None, None -> ""
  in
  row
    ~cls:"default-budget"
    ~name:"Budget"
    ~about:
      "Sessions without their own /budget start no turn once they have cost \
       this much, open ones too."
    ?note:
      (match error, config with
       | Some e, _ -> Some ("warm", e)
       | None, None -> Some waiting
       | None, Some _ when not on -> Some ("muted", "Off: no limit.")
       | None, Some _ -> None)
    [ number_switch
        ~disabled:(Option.is_none config)
        ~id:"default-budget"
        ~label:"Budget"
        ~unit:"USD"
        ~value
        ~placeholder:"5.00"
        ~title:"Dollars, e.g. 5 or 2.50 (/default-budget)"
        ~switch_title:
          (if on
           then "No budget, the built-in (/default-budget off)"
           else "Stop sessions starting turns past a cost (/default-budget)")
        ~invalid:(Option.is_some error)
        ~on
        ~on_input:(fun text ->
          inject (Action.Default_field_input (which, text)))
        ~on_commit:(inject (Action.Default_field_commit which))
        ~on_switch:(fun on -> inject (Action.Switch_number (Budget, on)))
        ()
    ]
;;

let groups (m : App.Model.t) ~inject =
  let config = m.config in
  [ group
      ~title:"Start with"
      ~note:"From the next new session"
      [ model m config ~inject
      ; thinking config ~inject
      ; mode config ~inject
      ; host m config ~inject
      ; dir m config ~inject
      ]
  ; group
      ~title:"Limits"
      ~note:"Every session without its own"
      [ fallback m config ~inject; budget m config ~inject ]
  ]
;;
