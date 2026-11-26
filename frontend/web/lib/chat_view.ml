open! Core
open! Import
open Chat_html

let image_src = Image_view.src
let message_attribute = "data-message-entry"
let message_id n = sprintf "msg-%d" n
let hidden_attribute = "data-hidden"
let hidden_action_attribute = "data-hidden-action"
let run_attribute = "data-run"

module Divider = struct
  type t =
    { after : int
    ; hidden : Hidden.t
    ; shown : Trimmed.Shown.t
    ; restoring : bool
    }

  let equal a b =
    a.after = b.after
    && Hidden.equal a.hidden b.hidden
    && Bool.equal a.restoring b.restoring
    &&
    match a.shown, b.shown with
    | Folded, Folded | Loading, Loading -> true
    | Shown a, Shown b -> phys_equal a b
    | (Folded | Loading | Shown _), _ -> false
  ;;
end

module Menu = struct
  type t =
    { entry : int
    ; highlighted : Context_menu.Item.t option
    }
  [@@deriving equal]
end

(* Whether a message has a menu, and what its open menu has highlighted. *)
module More = struct
  type t =
    | Closed
    | Open of Context_menu.Item.t option
  [@@deriving equal]
end

let tokens n =
  if n < 1000
  then Int.to_string n
  else if n < 100_000
  then sprintf "%.1fk" (Float.of_int n /. 1000.)
  else sprintf "%dk" (n / 1000)
;;

let time times at =
  Node.create
    "time"
    ~attrs:
      [ Attr.class_ "time"
      ; Attr.title (Message_time.full times at)
      ; Attr.create
          "datetime"
          (let date, ofday = Time_ns.to_date_ofday at ~zone:Time_ns.Zone.utc in
           sprintf
             "%sT%sZ"
             (Date.to_string date)
             (Time_ns.Ofday.to_sec_string ofday))
      ]
    [ Node.text (Message_time.short times at) ]
;;

(* A skill's instructions, folded under its name, above the user's text. *)
let skill_card (skill : Skill_message.t) =
  folded
    ~cls:"skill-card"
    ~label:("skill " ^ skill.name)
    ~preview:skill.location
    (div
       "skill-body"
       [ div "skill-location" [ Node.text skill.location ]
       ; Markdown_view.render skill.body
       ])
;;

(* A note of the harness's in the transcript's margin: a glyph and a line,
   unfolding to exactly what the model saw. *)
let note ?at times ~(kind : Origin.Kind.t) ~title text =
  Node.details
    ~attrs:
      [ Attr.class_ "note"
      ; Attr.create
          "data-kind"
          (match kind with
           | Other other -> other
           | kind ->
             String.lowercase (Sexp.to_string [%sexp (kind : Origin.Kind.t)]))
      ]
    [ Node.summary
        (present
           [ Node.span
               ~attrs:[ Attr.class_ "glyph"; Attr.create "aria-hidden" "true" ]
               [ Node.text (Injected_note.glyph kind) ]
           ; span "title" title
           ; Option.value_map at ~default:Node.none ~f:(time times)
           ])
    ; Node.pre ~attrs:[ Attr.class_ "note-text" ] [ Node.text text ]
    ]
;;

let margin_note ?at times (n : Origin.Note.t) =
  note ?at times ~kind:n.kind ~title:n.title n.text
;;

(* The open menu's highlighted item previews on its message what it would
   do. *)
let menu_classes (more : More.t) =
  match more with
  | Closed -> []
  | Open highlighted ->
    "menu-open"
    ::
    (match highlighted with
     | Some Trim_before -> [ "preview-trim-before" ]
     | Some Trim_after -> [ "preview-trim-after" ]
     | Some Delete -> [ "preview-delete" ]
     | Some _ | None -> [])
;;

(* A pasted text's fold: by the message's time, which only its own
   messages share. *)
let pasted_key (u : Message.User.t) i =
  sprintf
    "pasted:%s:%d"
    (Option.value_map u.at ~default:"" ~f:(fun at ->
       sprintf "%.0f" (Time_ns.Span.to_ms (Time_ns.to_span_since_epoch at))))
    i
;;

(* A chip each, its text only in the page once opened: it may be long. *)
let pasted_texts ~fold (u : Message.User.t) (pasted : Origin.Pasted.t list) =
  match pasted with
  | [] -> Node.none
  | pasted ->
    div
      "pasted-texts"
      (List.mapi pasted ~f:(fun i (p : Origin.Pasted.t) ->
         let key = pasted_key u i in
         let open_ = Fold.toggled fold key in
         let summary =
           Prigh_ui.Pasted_text.summary
             ~lines:p.lines
             ~bytes:(String.length p.text)
         in
         Node.div
           ~attrs:
             [ Attr.classes
                 ([ "pasted"; "copyable" ] @ if open_ then [ "open" ] else [])
             ]
           [ Node.button
               ~attrs:
                 [ Attr.class_ "pasted-head"
                 ; Attr.type_ "button"
                 ; Fold.toggle_attr fold key
                 ; Attr.create "aria-expanded" (Bool.to_string open_)
                 ; Attr.title (if open_ then "Fold it" else "Show it all")
                 ]
               [ Node.span ~attrs:[ Attr.class_ "chev" ] []
               ; span "pasted-name" p.name
               ; span "pasted-detail" summary
               ; (if open_
                  then Node.none
                  else
                    span "pasted-preview" (Prigh_ui.Pasted_text.preview p.text))
               ]
           ; (if open_
              then
                Node.div
                  ~attrs:[ Attr.class_ "pasted-body" ]
                  [ Node.button
                      ~attrs:
                        [ Attr.class_ "copy"
                        ; Attr.type_ "button"
                        ; Attr.title "Copy"
                        ]
                      [ Node.text "Copy" ]
                  ; Node.pre
                      ~attrs:[ Attr.class_ "pasted-text copy-text" ]
                      [ Node.text p.text ]
                  ]
              else Node.none)
           ]))
;;

(* Which client the user is on matters to the model, not to the user; in a
   subagent's chat, what the user sent it is theirs like any prompt (only
   the main agent's messages to it keep their note). *)
let shown_notes (o : Origin.t) =
  List.filter o.notes ~f:(fun (n : Origin.Note.t) ->
    match n.kind with
    | Client -> false
    | Other "steer" -> not (String.is_prefix n.title ~prefix:"You")
    | _ -> true)
;;

let user ?skill ~fold times (u : Message.User.t) =
  let notes, pasted, attachments =
    match u.origin with
    | None -> [], [], []
    | Some origin -> shown_notes origin, origin.pasted, origin.attachments
  in
  let text =
    Option.value_map
      skill
      ~default:(Message.User.body u)
      ~f:(fun (s : Skill_message.t) -> s.args)
  in
  let prompt =
    div
      (if Option.is_some skill then "msg user skill" else "msg user")
      [ Option.value_map skill ~default:Node.none ~f:skill_card
      ; Image_view.thumbs u.images
      ; (if String.is_empty (String.strip text)
         then Node.none
         else
           Node.div
             ~attrs:[ Attr.class_ "bubble"; Attr.create "dir" "auto" ]
             [ Node.text text ])
      ; pasted_texts ~fold u pasted
      ; (match attachments with
         | [] -> Node.none
         | paths ->
           div
             "attachments"
             (List.map paths ~f:(fun path ->
                Node.span
                  ~attrs:
                    [ Attr.class_ "attachment"; Attr.title "Attached file" ]
                  [ Node.text ("@" ^ path) ])))
      ; Option.value_map u.at ~default:Node.none ~f:(time times)
      ]
  in
  match notes with
  | [] -> prompt
  | notes
    when String.is_empty (String.strip text)
         && List.is_empty u.images
         && List.is_empty pasted
         && List.is_empty attachments ->
    (* Notes sent between turns, with nothing of the user's. *)
    div "msg noted" (List.map notes ~f:(margin_note ?at:u.at times))
  | notes -> div "msg noted" (List.map notes ~f:(margin_note times) @ [ prompt ])
;;

let summary_prefix = "Summary of the conversation so far:\n"

let compaction summary =
  folded
    ~cls:"msg compaction"
    ~label:"Context compacted"
    ~preview:(Markdown.preview summary)
    (Markdown_view.render summary)
;;

let day_separator times date =
  Node.div
    ~attrs:[ Attr.class_ "day-sep"; Attr.create "role" "separator" ]
    [ Node.span [ Node.text (Message_time.day_label times date) ] ]
;;

let thinking ~live text =
  if live
  then
    div
      "thinking live"
      [ div "thinking-head" [ Html.lamp (); span "label" "Thinking…" ]
      ; (if String.is_empty (String.strip text)
         then Node.none
         else div "thinking-text" [ Node.text (String.strip text) ])
      ]
  else
    folded
      ~cls:"thinking"
      ~label:"Thought"
      ~preview:(Markdown.preview text)
      (div "thinking-text" [ Markdown_view.render text ])
;;

(* Quiet: thinking as a step of a run, a line that opens its text. *)
let thinking_step ~fold ~key ~live text =
  let opened = Fold.opened ~by_default:false fold key in
  let preview = Markdown.preview text in
  Node.div
    ~attrs:
      [ Attr.classes
          ([ "tool"; "tool-thinking"; (if live then "running" else "ok") ]
           @ if opened then [ "open" ] else [])
      ]
    [ Node.button
        ~attrs:
          [ Attr.class_ "tool-head"
          ; Attr.type_ "button"
          ; Fold.toggle_attr fold key
          ; Attr.create "aria-expanded" (Bool.to_string opened)
          ]
        (present
           [ (if live
              then Html.lamp ()
              else Html.mark ~cls:"done" ~label:"Done" Check)
           ; span "name" "Thinking"
           ; (if opened || String.is_empty preview
              then Node.none
              else span "arg" preview)
           ; span "chev" ""
           ])
    ; (if opened && not (String.is_empty (String.strip text))
       then
         div "tool-body" [ div "thinking-text" [ Markdown_view.render text ] ]
       else Node.none)
    ]
;;

let stopped (message : Message.Assistant.t) =
  match message.stop_reason with
  | Error e ->
    (* A loon Alert (destructive): what failed, then the way out. *)
    Node.div
      ~attrs:
        [ Attr.create "data-slot" "alert"
        ; Attr.class_
            (Loon.Alert.classes
               ~variant:Destructive
               ~class_:"stop error loon flex flex-col gap-1 px-3.5 py-2.5"
               ())
        ]
      ([ span "title" "The model returned an error"
       ; div "detail" [ Node.text e ]
       ]
       @
       if Prigh_ui.Image_error.too_large e
       then
         [ span
             "hint"
             "An image is too large for the model. prigh can send the \
              conversation's images smaller and try again (the originals stay \
              in the session)."
         ; Node.button
             ~attrs:
               [ Attr.class_
                   (Loon.Button.classes
                      ~variant:Outline
                      ~size:Default
                      ~class_:
                        "stop-action mt-1 self-start border-destructive/40 \
                         dark:border-destructive/40 text-destructive \
                         hover:bg-destructive/10 dark:hover:bg-destructive/10 \
                         hover:text-destructive pointer-coarse:h-11"
                      ())
               ; Attr.type_ "button"
               ; Attr.create run_attribute "/downsize-images"
               ]
             [ Node.text "Downsize images and retry" ]
         ]
       else [ span "hint" "Send a message to retry, or switch model." ])
  | Aborted ->
    div
      "stop aborted"
      [ Html.mark ~cls:"stopped" Square; Node.text "Interrupted" ]
  | Length ->
    div
      "stop length"
      [ Html.mark ~cls:"warn" Warning
      ; Node.text "Stopped at the output token limit: ask it to continue."
      ]
  | End_turn | Tool_use -> Node.none
;;

(* A reply's model (by its name in [names], an agent CLI's model's key
   being no name for people) and tokens, and when it ended; a step that goes
   on with tool calls has neither. *)
let footer ~names times (message : Message.Assistant.t) =
  let usage =
    match message.stop_reason with
    | End_turn when message.usage.output > 0 ->
      let { Usage.input; output; cache_read } = message.usage in
      [ Option.value (Map.find names message.model) ~default:message.model
      ; tokens (input + cache_read) ^ " in"
      ]
      @ (if cache_read > 0 then [ tokens cache_read ^ " cached" ] else [])
      @ [ tokens output ^ " out" ]
    | _ -> []
  in
  let at =
    match message.stop_reason with
    | Tool_use -> None
    | End_turn | Length | Aborted | Error _ -> message.at
  in
  match
    List.map usage ~f:(fun part -> Node.span [ Node.text part ])
    @ Option.to_list (Option.map at ~f:(time times))
  with
  | [] -> Node.none
  | parts -> div "meta" parts
;;

(* What [/filter] calls each part of an entry: a user's message with its
   notes; each block of a reply, then how it stopped. *)
let user_kind (u : Message.User.t) : Transcript_filter.Kind.t =
  match Message.User.source u with
  | Some _ -> Notes
  | None -> User
;;

let content_kind : Content.t -> Transcript_filter.Kind.t = function
  | Text _ -> Assistant
  | Thinking _ -> Thinking
  | Tool_call call -> Transcript_filter.Kind.of_tool call.name
  | Server_tool _ -> Tools
;;

let blank : Content.t -> bool = function
  | Text text | Thinking text -> String.is_empty (String.strip text)
  | Tool_call _ | Server_tool _ -> false
;;

let parts : Chat.Entry.t -> Transcript_filter.Kind.t list = function
  | User u -> [ user_kind u ]
  | Shell _ -> [ Tools ]
  | Compaction _ -> [ Notes ]
  | Assistant { message; streaming = _ } ->
    List.filter_map message.content ~f:(fun c ->
      Option.some_if (not (blank c)) (content_kind c))
    @
      (match message.stop_reason with
      | Error _ | Aborted | Length -> [ Notes ]
      | End_turn | Tool_use -> [])
;;

let hidden ?(deleting = Int.Set.empty) ~fold chat =
  if Transcript_filter.is_all (Fold.filter fold)
  then 0
  else
    List.foldi (Chat.entries chat) ~init:0 ~f:(fun i sum e ->
      if Set.mem deleting (Chat.first chat + i)
      then sum
      else
        sum + List.count (parts e) ~f:(fun kind -> not (Fold.shows fold kind)))
;;

(* An agent CLI was asked and is silent: what it waits for. *)
let agent_waiting chat =
  if not (Chat.running chat)
  then None
  else (
    match Chat.activity chat with
    | Starting agent -> Some (sprintf "Starting %s" agent)
    | Waiting_for agent -> Some (sprintf "Waiting for %s" agent)
    | _ -> None)
;;

module Quiet_deps = struct
  type t =
    { segments : Activity_runs.Segment.t list
    ; continues : bool
    ; tail : bool (** a box here is still taking steps *)
    ; chat_running : bool
    ; agent_waiting : bool
    }

  let equal a b =
    Activity_runs.equal_segments a.segments b.segments
    && Bool.equal a.continues b.continues
    && Bool.equal a.tail b.tail
    && Bool.equal a.chat_running b.chat_running
    && Bool.equal a.agent_waiting b.agent_waiting
  ;;
end

module Deps = struct
  type t =
    { n : int
    ; running : bool
    ; tools : Chat.Tool.t option list
    ; times : Message_time.t
    ; names : string String.Map.t
    ; expanded : bool list (** the entry's folds *)
    ; nested : Fold.t option (** all of them, with subagents' transcripts *)
    ; more : More.t option
    ; filter : Transcript_filter.t
    ; ui : Ui_context.Page.t option
      (** with a [render_ui] call: what its components show *)
    ; sessions : int (** {!Session_link_view.stamp} *)
    ; quiet : Quiet_deps.t option (** the boxes that start in it *)
    }

  let equal a b =
    a.n = b.n
    && a.sessions = b.sessions
    && Transcript_filter.equal a.filter b.filter
    && Option.equal More.equal a.more b.more
    && Bool.equal a.running b.running
    && List.equal (Option.equal phys_equal) a.tools b.tools
    && Message_time.equal a.times b.times
    && Map.equal String.equal a.names b.names
    && List.equal Bool.equal a.expanded b.expanded
    && Option.equal Fold.equal a.nested b.nested
    && Option.equal Quiet_deps.equal a.quiet b.quiet
    && Option.equal
         (fun (a : Ui_context.Page.t) (b : Ui_context.Page.t) ->
            phys_equal a.state b.state
            && phys_equal a.inject b.inject
            && Sexp.equal
                 [%sexp (a.sent : Ui_action.Sent.t list)]
                 [%sexp (b.sent : Ui_action.Sent.t list)])
         a.ui
         b.ui
  ;;
end

module Chat_deps = struct
  type t =
    { times : Message_time.t
    ; names : string String.Map.t
    ; fold : Fold.t
    ; menu : Menu.t option
    ; index : Message_index.t option
    ; deleting : Int.Set.t
    ; dividers : Divider.t list
    ; ui : Ui_context.Page.t
    ; sessions : int
    }

  let equal a b =
    a.sessions = b.sessions
    && Message_time.equal a.times b.times
    && Map.equal String.equal a.names b.names
    && Fold.equal a.fold b.fold
    && Option.equal Menu.equal a.menu b.menu
    && Option.equal phys_equal a.index b.index
    && Set.equal a.deleting b.deleting
    && List.equal Divider.equal a.dividers b.dividers
    && phys_equal a.ui.Ui_context.Page.state b.ui.state
    && phys_equal a.ui.inject b.ui.inject
  ;;
end

let ui_sent chat =
  Ui_context.sent_of_messages
    (List.filter_map (Chat.entries chat) ~f:(function
       | Chat.Entry.User u -> Some u
       | Assistant _ | Shell _ | Compaction _ -> None))
;;

let entries : (Chat.Entry.t, Deps.t) View_cache.t = View_cache.create ()
let chats : (Chat.t, Chat_deps.t) View_cache.t = View_cache.create ()

let rec nested ~ui ~names ~fold times (s : Chat.Subagent.t) =
  view ~ui ~names ~fold:(Fold.scoped fold s.agent_id) times s.chat

and assistant
      ?quiet
      ~ui
      ~names
      ~fold
      times
      chat
      ~n
      (message : Message.Assistant.t)
      ~streaming
  =
  let count = List.length message.content in
  let block i (content : Content.t) =
    let last = i = count - 1 in
    match content with
    | content when not (Fold.shows fold (content_kind content)) -> Node.none
    | Text text when String.is_empty (String.strip text) -> Node.none
    | Text text -> Markdown_view.render ~streaming:(streaming && last) text
    | Thinking text
      when String.is_empty (String.strip text) && not (streaming && last) ->
      Node.none
    | Thinking text -> thinking ~live:(streaming && last) text
    | Tool_call call ->
      Tool_view.view
        ~ui
        ?started:message.at
        ~nested:(nested ~ui ~names ~fold times)
        ~fold
        ~streaming:(streaming && last)
        ~running:(Chat.running chat)
        call
        (Chat.tool chat call.id)
    | Server_tool s -> Tool_view.server_tool ~fold ~running:streaming s
  in
  let blocks =
    match quiet with
    | None -> List.mapi message.content ~f:block
    | Some runs ->
      let content = Array.of_list message.content in
      List.concat_map (Activity_runs.segments runs n) ~f:(function
        | Whole -> List.mapi message.content ~f:block
        | Blocks is -> List.map is ~f:(fun i -> block i content.(i))
        | Box box -> [ activity ~ui ~names ~fold times chat runs box ])
  in
  let blocks = present blocks in
  (* Quiet: the box of the run it goes on with says what it does, unless
     the model is failing. *)
  let in_box =
    match quiet, Chat.activity chat with
    | _, (Retrying _ | Waiting (Some _)) | None, _ -> false
    | Some runs, _ -> Activity_runs.continues runs n
  in
  let pending =
    if streaming && count = 0 && not in_box then pending chat else Node.none
  in
  let stopped = if Fold.shows fold Notes then stopped message else Node.none in
  if
    List.is_empty blocks
    && phys_equal pending Node.none
    && phys_equal stopped Node.none
    && ((not streaming) || Option.is_some quiet)
  then Node.none
  else
    div
      (if streaming then "msg assistant streaming" else "msg assistant")
      ((pending :: blocks)
       @ [ stopped
         ; (if
              streaming
              || (List.is_empty blocks
                  && not (Transcript_filter.is_all (Fold.filter fold)))
            then Node.none
            else footer ~names times message)
         ])

(* The lamp of a reply nothing of is shown yet: what it waits for. *)
and pending chat =
  let label, detail =
    match Chat.activity chat with
    | Retrying { attempt; max_attempts; error; delay_ms } ->
      ( sprintf
          "Retrying in %s (attempt %d of %d)…"
          (Prigh_ui.Activity.seconds delay_ms)
          attempt
          max_attempts
      , Some error )
    | Waiting (Some { attempt; max_attempts; error; delay_ms = _ }) ->
      ( sprintf "Waiting for the model (attempt %d of %d)…" attempt max_attempts
      , Some error )
    | Starting agent -> sprintf "Starting %s…" agent, None
    | Waiting_for agent -> sprintf "Waiting for %s…" agent, None
    | _ -> "Waiting for the model…", None
  in
  div
    "pending"
    [ Html.lamp ()
    ; span "label" label
    ; Node.span
        ~attrs:[ Attr.class_ "stop-hint" ]
        [ Loon.Kbd.view [ Node.text "Esc" ]; Node.text " to stop" ]
    ; (match detail with
       | Some error -> div "detail" [ Node.text ("after: " ^ error) ]
       | None -> Node.none)
    ]

(* Quiet: a run of steps as one line saying what it is doing, or what it
   did and how long it took; the line opens the steps, each a line that
   opens its detail. Collapsed, the steps are not rendered at all. *)
and activity ~ui ~names ~fold times chat runs (box : Activity_runs.Box.t) =
  let steps =
    List.filter (Activity_runs.Box.steps box) ~f:(fun step ->
      Fold.shows fold (Activity_runs.Step.kind step))
  in
  if List.is_empty steps
  then Node.none
  else (
    let chat_running = Chat.running chat in
    let tail =
      Option.exists (Activity_runs.tail runs) ~f:(fun tail ->
        phys_equal tail box)
    in
    let status : Activity_runs.Step.t -> Tool_view.Status.t = function
      | Call { call; streaming; _ } ->
        Tool_view.status
          ~streaming
          ~running:chat_running
          (Chat.tool chat call.id)
      | Server { tool; streaming; _ } ->
        if Option.is_none (snd (Tool_summary.as_call tool)) && streaming
        then Running
        else Done
      | Thinking { live; _ } -> if live then Running else Done
      | Report _ -> Done
    in
    let statuses = List.map steps ~f:(fun step -> step, status step) in
    let busy : Tool_view.Status.t -> bool = function
      | Preparing | Running -> true
      | Done | Failed | Interrupted -> false
    in
    let current =
      List.rev statuses |> List.find ~f:(fun (_, status) -> busy status)
    in
    let running = Option.is_some current || (chat_running && tail) in
    let failed =
      List.count statuses ~f:(fun (_, status) ->
        match status with
        | Failed -> true
        | Preparing | Running | Done | Interrupted -> false)
    in
    let label =
      match current with
      | Some (step, _) ->
        Prigh_ui.Tool_phrase.doing (Activity_runs.Step.phrase step)
      | None when running ->
        (* An agent CLI gone silent says what it waits for. *)
        Option.value (agent_waiting chat) ~default:"Thinking"
      | None ->
        Prigh_ui.Tool_phrase.did (List.map steps ~f:Activity_runs.Step.phrase)
    in
    (* From the message before it to its last step's end. *)
    let took =
      if running
      then None
      else (
        let ends =
          List.filter_map steps ~f:(fun (step : Activity_runs.Step.t) ->
            match step with
            | Call { call; at; _ } ->
              Option.first_some
                (Option.bind (Chat.tool chat call.id) ~f:(fun t ->
                   Option.bind t.result ~f:(fun r -> r.at)))
                at
            | Report { user; _ } -> user.at
            | Thinking _ | Server _ -> None)
        in
        match
          ( Activity_runs.Box.since box
          , List.max_elt ends ~compare:Time_ns.compare )
        with
        | Some since, Some until when Time_ns.( >= ) until since ->
          Some (Prigh_ui.Tool_phrase.duration (Time_ns.diff until since))
        | _ -> None)
    in
    let key = Activity_runs.Box.key box in
    let opened = Fold.opened ~by_default:false fold key in
    let icon =
      if running
      then Html.lamp ()
      else if failed > 0
      then Html.mark ~cls:"failed" ~label:"A step failed" Cross
      else Html.mark ~cls:"done" ~label:"Done" Check
    in
    Node.div
      ~attrs:
        [ Attr.classes
            ([ "run"
             ; (if running
                then "running"
                else if failed > 0
                then "error"
                else "ok")
             ]
             @ if opened then [ "open" ] else [])
        ; Attr.create "data-slot" "collapsible"
        ; Attr.create "data-state" (if opened then "open" else "closed")
        ]
      [ Node.button
          ~attrs:
            [ Attr.class_ "run-head"
            ; Attr.create "data-slot" "collapsible-trigger"
            ; Attr.type_ "button"
            ; Fold.toggle_attr fold key
            ; Attr.create "aria-expanded" (Bool.to_string opened)
            ]
          (present
             ([ icon ]
              @ (match took with
                 (* Done: how long it took, then what it did. *)
                 | Some took ->
                   [ span "run-label" "Worked for"
                   ; span "run-time" took
                   ; span "run-did" ("· " ^ String.uncapitalize label)
                   ]
                 | None -> [ span "run-label" label ])
              @ [ (if failed > 0
                   then span "run-failed" (sprintf "%d failed" failed)
                   else Node.none)
                ; span "chev" ""
                ]))
      ; (if opened
         then
           Node.div
             ~attrs:
               [ Attr.class_ "run-steps"
               ; Attr.create "data-slot" "collapsible-content"
               ]
             (present
              @@ List.map steps ~f:(fun (step : Activity_runs.Step.t) ->
                match step with
                | Call { call; streaming; at; _ } ->
                  Tool_view.view
                    ~ui
                    ?started:at
                    ~nested:(nested ~ui ~names ~fold times)
                    ~fold
                    ~streaming
                    ~running:chat_running
                    call
                    (Chat.tool chat call.id)
                | Server { tool; streaming; _ } ->
                  Tool_view.server_tool ~fold ~running:streaming tool
                | Thinking { text; live; _ } ->
                  thinking_step
                    ~fold
                    ~key:(Activity_runs.Step.key step)
                    ~live
                    text
                | Report { user; _ } ->
                  render_entry
                    ~ui
                    ~names
                    ~fold
                    times
                    chat
                    ~n:0
                    (Chat.Entry.User user)))
         else Node.none)
      ])

and render_entry ?quiet ~ui ~names ~fold times chat ~n (entry : Chat.Entry.t) =
  let segments =
    Option.map quiet ~f:(fun runs -> runs, Activity_runs.segments runs n)
  in
  match entry, segments with
  | (User _ | Shell _ | Compaction _), Some (runs, segments)
    when not (Activity_runs.equal_segments segments [ Whole ]) ->
    (match
       List.filter_map segments ~f:(function
         | Box box -> Some (activity ~ui ~names ~fold times chat runs box)
         | Whole | Blocks _ -> None)
       |> present
     with
     | [] -> Node.none
     | boxes -> div "msg steps" boxes)
  | _ ->
    (match entry with
     | User u when not (Fold.shows fold (user_kind u)) -> Node.none
     | Shell _ when not (Fold.shows fold Tools) -> Node.none
     | Compaction _ when not (Fold.shows fold Notes) -> Node.none
     | User u ->
       let notes = Option.value_map u.origin ~default:[] ~f:shown_notes in
       (* Notes before a message of the harness's: plan mode turned on or off
       with a goal's message. *)
       let noted main =
         match notes with
         | [] -> main
         | notes ->
           div "msg noted" (List.map notes ~f:(margin_note times) @ [ main ])
       in
       (match Message.User.source u with
        | Some { kind = Delivery; title } ->
          noted
            (match Prigh_ui.Delivery.parse u.text with
             | Some sections ->
               div
                 "msg"
                 [ Delivery_view.view
                     ~name:(Tool_view.agent_name chat)
                     ~fold
                     sections
                 ]
             | None -> note ?at:u.at times ~kind:Delivery ~title u.text)
        | Some { kind = Summary; _ } ->
          compaction
            (Option.value
               (String.chop_prefix u.text ~prefix:summary_prefix)
               ~default:u.text)
        | Some { kind; title } ->
          noted (note ?at:u.at times ~kind ~title u.text)
        | None ->
          user ?skill:(Skill_message.parse (Message.User.body u)) ~fold times u)
     | Shell call ->
       let tool = Chat.tool chat call.id in
       div
         "msg shell"
         [ Tool_view.view
             ~ui
             ~nested:(nested ~ui ~names ~fold times)
             ~fold
             ~streaming:false
             ~running:
               (Option.value_map tool ~default:true ~f:(fun t ->
                  Option.is_none t.result))
             { call with name = "bash" }
             tool
         ]
     | Compaction summary -> compaction summary
     | Assistant { message; streaming } ->
       assistant ?quiet ~ui ~names ~fold times chat ~n message ~streaming)

(* An entry's card depends on the state of its tool calls, and on whether the
   agent is still running while one of them has no result. *)
and entry
      ?menu
      ?index
      ?quiet
      ~ui
      ~names
      ~fold
      times
      chat
      ~n
      (entry : Chat.Entry.t)
  =
  let span = Option.bind index ~f:(fun index -> Message_index.span index n) in
  let more : More.t option =
    match span, entry with
    | Some _, User u when Option.is_none (Message.User.source u) -> Some Closed
    | Some _, Assistant { streaming = false; _ } -> Some Closed
    | _ -> None
  in
  let more =
    match more, menu with
    | Some Closed, Some (menu : Menu.t) when menu.entry = n ->
      Some (More.Open menu.highlighted)
    | more, _ -> more
  in
  let ids =
    match entry with
    | Assistant { message; _ } ->
      List.filter_map message.content ~f:(function
        | Tool_call call -> Some call.id
        | Text _ | Thinking _ | Server_tool _ -> None)
    | Shell call -> [ call.id ]
    | User ({ origin = Some { pasted = _ :: _ as pasted; _ }; _ } as u) ->
      List.mapi pasted ~f:(fun i _ -> pasted_key u i)
    | User u when String.is_prefix u.text ~prefix:"[" ->
      (* A delivery's jobs fold. *)
      (match Prigh_ui.Delivery.parse u.text with
       | Some sections -> List.map sections ~f:Delivery_view.job_key
       | None -> [])
    | User _ | Compaction _ -> []
  in
  (* Quiet: the boxes that start here, with their steps from the entries
     after it. *)
  let quiet_deps, boxes =
    match quiet with
    | None -> None, []
    | Some runs ->
      let segments = Activity_runs.segments runs n in
      let boxes =
        List.filter_map segments ~f:(function
          | Box box -> Some box
          | Whole | Blocks _ -> None)
      in
      let tail =
        Option.exists (Activity_runs.tail runs) ~f:(fun tail ->
          List.exists boxes ~f:(phys_equal tail))
      in
      ( Some
          { Quiet_deps.segments
          ; continues = Activity_runs.continues runs n
          ; tail
          ; chat_running = tail && Chat.running chat
          ; agent_waiting = tail && Option.is_some (agent_waiting chat)
          }
      , boxes )
  in
  let steps = List.concat_map boxes ~f:Activity_runs.Box.steps in
  let box_calls =
    List.filter_map steps ~f:(function
      | Call { call; _ } -> Some call.id
      | Thinking _ | Server _ | Report _ -> None)
  in
  let tools =
    (match entry with
     | Assistant _ | Shell _ -> List.map ids ~f:(Chat.tool chat)
     | User _ | Compaction _ -> [])
    @ List.map box_calls ~f:(Chat.tool chat)
  in
  let running =
    Chat.running chat
    && List.exists tools ~f:(fun tool ->
      Option.for_all tool ~f:(fun tool -> Option.is_none tool.result))
  in
  View_cache.find
    entries
    entry
    ~deps:
      { Deps.n
      ; running
      ; tools
      ; times
      ; names
      ; expanded =
          List.concat_map ids ~f:(fun id ->
            [ Fold.expanded fold id
            ; Fold.expanded fold (Tool_view.open_key id)
            ])
          @ List.concat_map boxes ~f:(fun box ->
            Fold.expanded fold (Activity_runs.Box.key box)
            :: List.map (Activity_runs.Box.steps box) ~f:(fun step ->
              Fold.expanded fold (Activity_runs.Step.key step)))
          @ List.map box_calls ~f:(Fold.expanded fold)
      ; quiet = quiet_deps
      ; nested =
          Option.some_if
            (List.exists tools ~f:(fun tool ->
               Option.exists tool ~f:(fun t -> Option.is_some t.subagent)))
            fold
      ; more
      ; filter = Fold.filter fold
      ; ui =
          Option.some_if
            (List.exists tools ~f:(fun tool ->
               Option.exists tool ~f:(fun t ->
                 String.equal t.call.name "render_ui")))
            ui
      ; sessions = Session_link_view.stamp ()
      }
    ~equal:Deps.equal
    ~f:(fun () ->
      let node = render_entry ?quiet ~ui ~names ~fold times chat ~n entry in
      let places =
        (if Jump.landmark entry
         then [ Attr.create "data-entry" (Int.to_string n) ]
         else [])
        @
        if Option.is_some span
        then [ Attr.create message_attribute (Int.to_string n) ]
        else []
      in
      let attrs =
        places
        @
        (* Focusable, so that the keyboard can open its menu. *)
        match more with
        | None -> []
        | Some more ->
          [ Attr.id (message_id n); Attr.tabindex (-1) ]
          @
            (match menu_classes more with
            | [] -> []
            | classes -> [ Attr.classes classes ])
      in
      match node with
      | Element e when not (List.is_empty attrs) ->
        Element (Node.Element.map_attrs e ~f:(fun a -> Attr.many (a :: attrs)))
      | node when phys_equal node Node.none && not (List.is_empty places) ->
        (* Filtered out: the place stays for jumping and scrolling to. *)
        Node.div ~attrs:(Attr.class_ "filtered" :: places) []
      | node -> node)

(* Entries keep their keys as older ones are added before them, so that the
   page moves their nodes rather than redrawing every one. *)
and keyed key (node : Node.t) : Node.t =
  match node with
  | Lazy { t; key = _ } -> Lazy { key = Some key; t }
  | Element element -> Element (Node.Element.with_key element key)
  | node -> node

(* Days are marked where they change, not above the first message: its time
   already says which day it is. *)
and view
      ?(ui = Ui_context.Page.none)
      ?(fold = Fold.default)
      ?menu
      ?index
      ?(deleting = Int.Set.empty)
      ?(dividers = [])
      ?(names = String.Map.empty)
      times
      chat
  =
  View_cache.find
    chats
    chat
    ~deps:
      { Chat_deps.times
      ; names
      ; fold
      ; menu
      ; index
      ; deleting
      ; dividers
      ; ui
      ; sessions = Session_link_view.stamp ()
      }
    ~equal:Chat_deps.equal
    ~f:(fun () ->
      entries_view ?menu ?index ~ui ~names ~deleting ~dividers ~fold times chat)

(* What a trim after a message keeps out of the context: a divider where it
   was, with Show (the messages below it, dimmed and read-only) or Hide, and
   Undo; the page's click listener turns them into actions. *)
and divider ~ui ~names ~fold times (d : Divider.t) =
  let h = d.hidden in
  let label = Trim.label ~messages:h.messages ~tokens:h.tokens in
  let busy text =
    Node.span
      ~attrs:[ Attr.class_ "trim-busy"; Attr.role "status" ]
      [ Html.lamp (); Node.text text ]
  in
  let action ~title ?(attrs = []) ~what text =
    Node.button
      ~attrs:
        ([ Attr.class_ "trim-action"
         ; Attr.type_ "button"
         ; Attr.title title
         ; Attr.create hidden_attribute h.entry
         ; Attr.create hidden_action_attribute what
         ]
         @ attrs)
      [ Node.text text ]
  in
  let toggle =
    match d.shown with
    | Folded ->
      action
        ~title:"Show the trimmed messages below, read-only"
        ~attrs:[ Attr.create "aria-expanded" "false" ]
        ~what:"show"
        "Show"
    | Loading -> busy "Loading…"
    | Shown _ ->
      action
        ~title:"Fold the trimmed messages again"
        ~attrs:[ Attr.create "aria-expanded" "true" ]
        ~what:"hide"
        "Hide"
  in
  let undo =
    if d.restoring
    then busy "Restoring…"
    else
      action
        ~title:"Bring them back into the model's context"
        ~what:"undo"
        "Undo"
  in
  Node.div
    ~attrs:[ Attr.class_ "hidden-range" ]
    [ Node.div
        ~attrs:
          [ Attr.class_ "trim-divider after"
          ; Attr.role "group"
          ; Attr.create "aria-label" label
          ; Attr.create hidden_attribute h.entry
          ]
        [ Html.icon ~cls:"scissors" Scissors
        ; span "trim-label" label
        ; Node.span
            ~attrs:[ Attr.class_ "trim-actions" ]
            [ toggle
            ; Node.span
                ~attrs:
                  [ Attr.class_ "trim-sep"; Attr.create "aria-hidden" "true" ]
                [ Node.text "·" ]
            ; undo
            ]
        ]
    ; (match d.shown with
       | Shown chat ->
         Node.section
           ~attrs:
             [ Attr.class_ "trimmed-messages"
             ; Attr.create
                 "aria-label"
                 "Trimmed messages, not in the model's context"
             ]
           [ div
               "trimmed-head"
               [ span "trimmed-title" "Not in the model's context"
               ; span
                   "trimmed-note"
                   "Kept in the transcript only; the model no longer sees them."
               ]
           ; view ~ui ~names ~fold times chat
           ]
       | Folded | Loading -> Node.none)
    ]

and entries_view ?menu ?index ~ui ~names ~deleting ~dividers ~fold times chat =
  let ui = { ui with Ui_context.Page.sent = ui_sent chat } in
  let quiet =
    if Fold.quiet fold then Some (Activity_runs.of_chat chat) else None
  in
  let dividers_after n =
    List.filter_map dividers ~f:(fun (d : Divider.t) ->
      Option.some_if
        (d.after = n)
        (keyed ("hidden-" ^ d.hidden.entry) (divider ~ui ~names ~fold times d)))
  in
  let entry_time : Chat.Entry.t -> Time_ns.t option = function
    | User u -> u.at
    | Assistant { message; _ } -> message.at
    | Shell _ | Compaction _ -> None
  in
  let _, _, rev_nodes =
    List.fold
      (Chat.entries chat)
      ~init:
        (None, Chat.first chat, List.rev (dividers_after (Chat.first chat - 1)))
      ~f:(fun (last_day, i, rev_nodes) e ->
        if Set.mem deleting i
        then last_day, i + 1, List.rev_append (dividers_after i) rev_nodes
        else (
          let day = Option.map (entry_time e) ~f:(Message_time.date times) in
          let rev_nodes =
            match last_day, day with
            | Some last, Some day when not (Date.equal last day) ->
              keyed (sprintf "day%d" i) (day_separator times day) :: rev_nodes
            | _ -> rev_nodes
          in
          ( Option.first_some day last_day
          , i + 1
          , List.rev_append
              (dividers_after i)
              (keyed
                 (Int.to_string i)
                 (entry ?menu ?index ?quiet ~ui ~names ~fold times chat ~n:i e)
               :: rev_nodes) )))
  in
  (* An agent CLI asked for a reply has said nothing yet: no message of it
     streams, as the model's does from its asking. Quiet, a run it goes on
     with says so instead. *)
  let in_run =
    Option.exists quiet ~f:(fun runs ->
      Option.is_some (Activity_runs.tail runs))
  in
  let rev_nodes =
    match Chat.streaming chat with
    | None when Option.is_some (agent_waiting chat) && not in_run ->
      keyed "waiting" (div "msg assistant streaming" [ pending chat ])
      :: rev_nodes
    | _ -> rev_nodes
  in
  div "entries" (List.rev rev_nodes)
;;
