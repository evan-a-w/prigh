open! Core
open! Import
open Chat_html

let image_src = Image_view.src
let trim_attribute = "data-trim-entry"
let delete_attribute = "data-delete-entry"
let trim_after_attribute = "data-trim-after-entry"
let message_attribute = "data-message-entry"
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

module Actions = struct
  type t =
    { delete : bool
    ; trim_after : bool
    ; calls : int (** a reply's tool calls, deleted with it *)
    }
  [@@deriving equal]

  let none = { delete = false; trim_after = false; calls = 0 }
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

(* Revealed with the message on hover or focus; the page's click listener
   turns it into [Ask_trim]. *)
let trim_button n =
  Node.button
    ~attrs:
      [ Attr.class_ "trim-before"
      ; Attr.type_ "button"
      ; Attr.title "Trim everything before this from the model's context"
      ; Attr.create trim_attribute (Int.to_string n)
      ]
    [ Node.span
        ~attrs:[ Attr.class_ "scissors"; Attr.create "aria-hidden" "true" ]
        [ Node.text "✂" ]
    ; Node.text "Trim everything before this"
    ]
;;

(* "Delete" and "Trim everything after this", on the message's bottom edge,
   where the cut would be, revealed like [trim_button]. *)
let actions_bar n ~(actions : Actions.t) ~reply =
  let button ~cls ~attribute ~title ~glyph text =
    Node.button
      ~attrs:
        [ Attr.class_ cls
        ; Attr.type_ "button"
        ; Attr.title title
        ; Attr.create attribute (Int.to_string n)
        ]
      [ Node.span
          ~attrs:[ Attr.class_ "glyph"; Attr.create "aria-hidden" "true" ]
          [ Node.text glyph ]
      ; Node.text text
      ]
  in
  if not (actions.delete || actions.trim_after)
  then Node.none
  else
    div
      "msg-actions"
      [ (if actions.delete
         then
           button
             ~cls:"msg-delete"
             ~attribute:delete_attribute
             ~title:
               (match reply, actions.calls with
                | false, _ ->
                  "Delete this message from the transcript and the model's \
                   context"
                | true, 0 ->
                  "Delete this reply from the transcript and the model's \
                   context"
                | true, calls ->
                  sprintf
                    "Delete this reply with its %s and their results from the \
                     transcript and the model's context"
                    (if calls = 1
                     then "tool call"
                     else sprintf "%d tool calls" calls))
             ~glyph:"✕"
             "Delete"
         else Node.none)
      ; (if actions.trim_after
         then
           button
             ~cls:"trim-after"
             ~attribute:trim_after_attribute
             ~title:
               (if actions.calls > 0
                then
                  "Trim everything after this (and its tool results) from the \
                   model's context"
                else "Trim everything after this from the model's context")
             ~glyph:"✂"
             "Trim everything after this"
         else Node.none)
      ]
;;

let user ?skill ?trim ?(actions = Node.none) times (u : Message.User.t) =
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
      ; Option.value_map trim ~default:Node.none ~f:trim_button
      ; actions
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

module Deps = struct
  type t =
    { n : int
    ; running : bool
    ; tools : Chat.Tool.t option list
    ; times : Message_time.t
    ; expanded : bool list (** the entry's folds *)
    ; nested : Fold.t option (** all of them, with subagents' transcripts *)
    ; trim : bool (** it has "Trim everything before this" *)
    ; actions : Actions.t
    }

  let equal a b =
    a.n = b.n
    && Bool.equal a.trim b.trim
    && Actions.equal a.actions b.actions
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
    ; trim_after : int option
    ; index : Message_index.t option
    ; deleting : Int.Set.t
    ; dividers : Divider.t list
    }

  let equal a b =
    Message_time.equal a.times b.times
    && Fold.equal a.fold b.fold
    && Option.equal Int.equal a.trim_after b.trim_after
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
      ?(actions = Node.none)
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
  let stopped = stopped message in
  if List.is_empty blocks && (not streaming) && phys_equal stopped Node.none
  then Node.none
  else
    div
      (if streaming then "msg assistant streaming" else "msg assistant")
      ((pending :: blocks)
       @ [ stopped
         ; (if streaming then Node.none else footer times message)
         ; actions
         ])

and render_entry ?trim ?actions ~fold times chat (entry : Chat.Entry.t) =
  match entry with
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
       user
         ?skill:(Skill_message.parse (Message.User.body u))
         ?trim
         ?actions
         times
         u)
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
    assistant ?actions ~fold times chat message ~streaming

(* An entry's card depends on the state of its tool calls, and on whether the
   agent is still running while one of them has no result. *)
and entry ?trim_after ?index ~fold times chat ~n (entry : Chat.Entry.t) =
  let trim =
    match trim_after, entry with
    | Some after, User u
      when n > after && Option.is_none (Message.User.source u) ->
      Jump.landmark entry
    | _ -> false
  in
  let span = Option.bind index ~f:(fun index -> Message_index.span index n) in
  let actions : Actions.t =
    match span, index, entry with
    | Some _, Some index, User _ ->
      { delete = true
      ; trim_after =
          not ([%equal: int option] (Message_index.newest index) (Some n))
      ; calls = 0
      }
    | Some _, Some index, Assistant { message; streaming = false } ->
      { delete = true
      ; trim_after =
          not ([%equal: int option] (Message_index.newest index) (Some n))
      ; calls =
          List.count message.content ~f:(function
            | Tool_call _ -> true
            | Text _ | Thinking _ | Server_tool _ -> false)
      }
    | _ -> Actions.none
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
      ; trim
      ; actions
      }
    ~equal:Deps.equal
    ~f:(fun () ->
      let node =
        render_entry
          ?trim:(Option.some_if trim n)
          ~actions:
            (actions_bar
               n
               ~actions
               ~reply:
                 (match entry with
                  | Assistant _ -> true
                  | User _ | Shell _ | Compaction _ -> false))
          ~fold
          times
          chat
          entry
      in
      let attrs =
        (if Jump.landmark entry
         then [ Attr.create "data-entry" (Int.to_string n) ]
         else [])
        @
        if Option.is_some span
        then [ Attr.create message_attribute (Int.to_string n) ]
        else []
      in
      match node with
      | Element e when not (List.is_empty attrs) ->
        Element (Node.Element.map_attrs e ~f:(fun a -> Attr.many (a :: attrs)))
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
      ?trim_after
      ?index
      ?(deleting = Int.Set.empty)
      ?(dividers = [])
      times
      chat
  =
  View_cache.find
    chats
    chat
    ~deps:{ Chat_deps.times; fold; trim_after; index; deleting; dividers }
    ~equal:Chat_deps.equal
    ~f:(fun () ->
      entries_view ?trim_after ?index ~deleting ~dividers ~fold times chat)

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

and entries_view ?trim_after ?index ~deleting ~dividers ~fold times chat =
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
                 (entry ?trim_after ?index ~fold times chat ~n:i e)
               :: rev_nodes) )))
  in
  div "entries" (List.rev rev_nodes)
;;
