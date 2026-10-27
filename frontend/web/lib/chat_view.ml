open! Core
open! Import
open Chat_html

let image_src = Image_view.src
let message_attribute = "data-message-entry"
let menu_attribute = "data-message-menu"
let more_id n = sprintf "msg-more-%d" n
let hidden_attribute = "data-hidden"
let hidden_action_attribute = "data-hidden-action"

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

(* The ⋯ in a message's corner, shown on hover or focus: its menu, which
   the page's click listener opens. The item highlighted in the open menu
   previews on the message what it would do. *)
let more_button n (more : More.t) =
  let preview =
    match more with
    | Open (Some Trim_before) -> [ "preview-trim-before" ]
    | Open (Some Trim_after) -> [ "preview-trim-after" ]
    | Open (Some Delete) -> [ "preview-delete" ]
    | Open _ | Closed -> []
  in
  let open_ =
    match more with
    | Open _ -> true
    | Closed -> false
  in
  Node.button
    ~attrs:
      [ Attr.classes
          ([ "msg-more" ] @ (if open_ then [ "open" ] else []) @ preview)
      ; Attr.type_ "button"
      ; Attr.id (more_id n)
      ; Attr.create menu_attribute (Int.to_string n)
      ; Attr.title "More (right-click, Shift+F10)"
      ; Attr.create "aria-label" "More for this message"
      ; Attr.create "aria-haspopup" "menu"
      ; Attr.create "aria-expanded" (Bool.to_string open_)
      ]
    [ Html.icon More ]
;;

let user ?skill ?(more = Node.none) times (u : Message.User.t) =
  let notes, attachments =
    match u.origin with
    | None -> [], []
    | Some origin -> origin.notes, origin.attachments
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
      ; (match attachments with
         | [] -> Node.none
         | paths ->
           div
             "attachments"
             (List.map paths ~f:(fun path ->
                Node.span
                  ~attrs:[ Attr.class_ "attachment"; Attr.title "Attached file" ]
                  [ Node.text ("@" ^ path) ])))
      ; Option.value_map u.at ~default:Node.none ~f:(time times)
      ; more
      ]
  in
  match notes with
  | [] -> prompt
  | notes
    when String.is_empty (String.strip text)
         && List.is_empty u.images
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

let stopped (message : Message.Assistant.t) =
  match message.stop_reason with
  | Error e ->
    div
      "stop error"
      [ span "title" "The model returned an error"
      ; div "detail" [ Node.text e ]
      ; span "hint" "Send a message to retry, or switch model."
      ]
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

(* A reply's model and tokens, and when it ended; a step that goes on with
   tool calls has neither. *)
let footer times (message : Message.Assistant.t) =
  let usage =
    match message.stop_reason with
    | End_turn when message.usage.output > 0 ->
      let { Usage.input; output; cache_read } = message.usage in
      [ message.model; tokens (input + cache_read) ^ " in" ]
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

module Deps = struct
  type t =
    { n : int
    ; running : bool
    ; tools : Chat.Tool.t option list
    ; times : Message_time.t
    ; expanded : bool list (** the entry's folds *)
    ; nested : Fold.t option (** all of them, with subagents' transcripts *)
    ; more : More.t option
    ; filter : Transcript_filter.t
    }

  let equal a b =
    a.n = b.n
    && Transcript_filter.equal a.filter b.filter
    && Option.equal More.equal a.more b.more
    && Bool.equal a.running b.running
    && List.equal (Option.equal phys_equal) a.tools b.tools
    && Message_time.equal a.times b.times
    && List.equal Bool.equal a.expanded b.expanded
    && Option.equal Fold.equal a.nested b.nested
  ;;
end

module Chat_deps = struct
  type t =
    { times : Message_time.t
    ; fold : Fold.t
    ; menu : Menu.t option
    ; index : Message_index.t option
    ; deleting : Int.Set.t
    ; dividers : Divider.t list
    }

  let equal a b =
    Message_time.equal a.times b.times
    && Fold.equal a.fold b.fold
    && Option.equal Menu.equal a.menu b.menu
    && Option.equal phys_equal a.index b.index
    && Set.equal a.deleting b.deleting
    && List.equal Divider.equal a.dividers b.dividers
  ;;
end

let entries : (Chat.Entry.t, Deps.t) View_cache.t = View_cache.create ()
let chats : (Chat.t, Chat_deps.t) View_cache.t = View_cache.create ()

let rec nested ~fold times (s : Chat.Subagent.t) =
  view ~fold:(Fold.scoped fold s.agent_id) times s.chat

and assistant
      ?(more = Node.none)
      ~fold
      times
      chat
      (message : Message.Assistant.t)
      ~streaming
  =
  let count = List.length message.content in
  let blocks =
    List.mapi message.content ~f:(fun i content ->
      let last = i = count - 1 in
      match (content : Content.t) with
      | content when not (Fold.shows fold (content_kind content)) -> Node.none
      | Text text when String.is_empty (String.strip text) -> Node.none
      | Text text -> Markdown_view.render ~streaming:(streaming && last) text
      | Thinking text
        when String.is_empty (String.strip text) && not (streaming && last) ->
        Node.none
      | Thinking text -> thinking ~live:(streaming && last) text
      | Tool_call call ->
        Tool_view.view
          ~nested:(nested ~fold times)
          ~fold
          ~streaming:(streaming && last)
          ~running:(Chat.running chat)
          call
          (Chat.tool chat call.id)
      | Server_tool s -> Tool_view.server_tool ~fold ~running:streaming s)
  in
  let blocks = present blocks in
  let pending =
    if streaming && count = 0
    then (
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
          ( sprintf
              "Waiting for the model (attempt %d of %d)…"
              attempt
              max_attempts
          , Some error )
        | _ -> "Waiting for the model…", None
      in
      div
        "pending"
        [ Html.lamp ()
        ; span "label" label
        ; (match detail with
           | Some error -> div "detail" [ Node.text ("after: " ^ error) ]
           | None -> Node.none)
        ])
    else Node.none
  in
  let stopped = if Fold.shows fold Notes then stopped message else Node.none in
  if List.is_empty blocks && (not streaming) && phys_equal stopped Node.none
  then Node.none
  else
    div
      (if streaming then "msg assistant streaming" else "msg assistant")
      ((pending :: blocks)
       @ [ stopped
         ; (if streaming
               || (List.is_empty blocks
                   && not (Transcript_filter.is_all (Fold.filter fold)))
            then Node.none
            else footer times message)
         ; more
         ])

and render_entry ?more ~fold times chat (entry : Chat.Entry.t) =
  match entry with
  | User u when not (Fold.shows fold (user_kind u)) -> Node.none
  | Shell _ when not (Fold.shows fold Tools) -> Node.none
  | Compaction _ when not (Fold.shows fold Notes) -> Node.none
  | User u ->
    let notes =
      Option.value_map u.origin ~default:[] ~f:(fun (o : Origin.t) -> o.notes)
    in
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
          | Some sections -> div "msg" [ Delivery_view.view ~fold sections ]
          | None -> note ?at:u.at times ~kind:Delivery ~title u.text)
     | Some { kind = Summary; _ } ->
       compaction
         (Option.value
            (String.chop_prefix u.text ~prefix:summary_prefix)
            ~default:u.text)
     | Some { kind; title } -> noted (note ?at:u.at times ~kind ~title u.text)
     | None ->
       user ?skill:(Skill_message.parse (Message.User.body u)) ?more times u)
  | Shell call ->
    let tool = Chat.tool chat call.id in
    div
      "msg shell"
      [ Tool_view.view
          ~nested:(nested ~fold times)
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
    assistant ?more ~fold times chat message ~streaming

(* An entry's card depends on the state of its tool calls, and on whether the
   agent is still running while one of them has no result. *)
and entry ?menu ?index ~fold times chat ~n (entry : Chat.Entry.t) =
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
    | User u when String.is_prefix u.text ~prefix:"[" ->
      (* A delivery's jobs fold. *)
      (match Prigh_ui.Delivery.parse u.text with
       | Some sections -> List.map sections ~f:Delivery_view.job_key
       | None -> [])
    | User _ | Compaction _ -> []
  in
  let tools =
    match entry with
    | Assistant _ | Shell _ -> List.map ids ~f:(Chat.tool chat)
    | User _ | Compaction _ -> []
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
      ; expanded = List.map ids ~f:(Fold.expanded fold)
      ; nested =
          Option.some_if
            (List.exists tools ~f:(fun tool ->
               Option.exists tool ~f:(fun t -> Option.is_some t.subagent)))
            fold
      ; more
      ; filter = Fold.filter fold
      }
    ~equal:Deps.equal
    ~f:(fun () ->
      let node =
        render_entry
          ?more:(Option.map more ~f:(more_button n))
          ~fold
          times
          chat
          entry
      in
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
        (* Focusable, so that a tap on a phone, which has no hover, shows
           its ⋯, and the keyboard can open its menu. *)
        if Option.is_some more then [ Attr.tabindex (-1) ] else []
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
      ?(fold = Fold.default)
      ?menu
      ?index
      ?(deleting = Int.Set.empty)
      ?(dividers = [])
      times
      chat
  =
  View_cache.find
    chats
    chat
    ~deps:{ Chat_deps.times; fold; menu; index; deleting; dividers }
    ~equal:Chat_deps.equal
    ~f:(fun () ->
      entries_view ?menu ?index ~deleting ~dividers ~fold times chat)

(* What a trim after a message keeps out of the context: a divider where it
   was, with Show (the messages below it, dimmed and read-only) or Hide, and
   Undo; the page's click listener turns them into actions. *)
and divider ~fold times (d : Divider.t) =
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
        [ Node.span
            ~attrs:[ Attr.class_ "scissors"; Attr.create "aria-hidden" "true" ]
            [ Node.text "✂" ]
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
           ; view ~fold times chat
           ]
       | Folded | Loading -> Node.none)
    ]

and entries_view ?menu ?index ~deleting ~dividers ~fold times chat =
  let dividers_after n =
    List.filter_map dividers ~f:(fun (d : Divider.t) ->
      Option.some_if
        (d.after = n)
        (keyed ("hidden-" ^ d.hidden.entry) (divider ~fold times d)))
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
                 (entry ?menu ?index ~fold times chat ~n:i e)
               :: rev_nodes) )))
  in
  div "entries" (List.rev rev_nodes)
;;
