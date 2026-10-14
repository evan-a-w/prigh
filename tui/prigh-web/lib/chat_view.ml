open! Core
open! Import
open Chat_html

let image_src = Image_view.src

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

let user ?skill times (u : Message.User.t) =
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
      [ div
          "thinking-head"
          [ Html.lamp ()
          ; span "label" "Thinking…"
          ]
      ; div "thinking-text" [ Node.text (String.strip text) ]
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
    }

  let equal a b =
    a.n = b.n
    && Bool.equal a.running b.running
    && List.equal (Option.equal phys_equal) a.tools b.tools
    && Message_time.equal a.times b.times
    && List.equal Bool.equal a.expanded b.expanded
    && Option.equal Fold.equal a.nested b.nested
  ;;
end

module Chat_deps = struct
  type t = Message_time.t * Fold.t

  let equal (times, fold) (times', fold') =
    Message_time.equal times times' && Fold.equal fold fold'
  ;;
end

let entries : (Chat.Entry.t, Deps.t) View_cache.t = View_cache.create ()
let chats : (Chat.t, Chat_deps.t) View_cache.t = View_cache.create ()

let rec nested ~fold times (s : Chat.Subagent.t) =
  view ~fold:(Fold.scoped fold s.agent_id) times s.chat

and assistant ~fold times chat (message : Message.Assistant.t) ~streaming =
  let count = List.length message.content in
  let blocks =
    List.mapi message.content ~f:(fun i content ->
      let last = i = count - 1 in
      match (content : Content.t) with
      | Text text when String.is_empty (String.strip text) -> Node.none
      | Text text -> Markdown_view.render ~streaming:(streaming && last) text
      | Thinking text when String.is_empty (String.strip text) -> Node.none
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
    then div "pending" [ Html.lamp (); span "label" "Waiting for the model…" ]
    else Node.none
  in
  let stopped = stopped message in
  if List.is_empty blocks && (not streaming) && phys_equal stopped Node.none
  then Node.none
  else
    div
      (if streaming then "msg assistant streaming" else "msg assistant")
      ((pending :: blocks)
       @ [ stopped; (if streaming then Node.none else footer times message) ])

and render_entry ~fold times chat (entry : Chat.Entry.t) =
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
     | None -> user ?skill:(Skill_message.parse (Message.User.body u)) times u)
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
    assistant ~fold times chat message ~streaming

(* An entry's card depends on the state of its tool calls, and on whether the
   agent is still running while one of them has no result. *)
and entry ~fold times chat ~n (entry : Chat.Entry.t) =
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
      }
    ~equal:Deps.equal
    ~f:(fun () ->
      let node = render_entry ~fold times chat entry in
      match node with
      | Element e when Jump.landmark entry ->
        Element
          (Node.Element.map_attrs e ~f:(fun a ->
             Attr.many [ a; Attr.create "data-entry" (Int.to_string n) ]))
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
and view ?(fold = Fold.default) times chat =
  View_cache.find
    chats
    chat
    ~deps:(times, fold)
    ~equal:Chat_deps.equal
    ~f:(fun () -> entries_view ~fold times chat)

and entries_view ~fold times chat =
  let entry_time : Chat.Entry.t -> Time_ns.t option = function
    | User u -> u.at
    | Assistant { message; _ } -> message.at
    | Shell _ | Compaction _ -> None
  in
  let _, _, rev_nodes =
    List.fold
      (Chat.entries chat)
      ~init:(None, Chat.first chat, [])
      ~f:(fun (last_day, i, rev_nodes) e ->
        let day = Option.map (entry_time e) ~f:(Message_time.date times) in
        let rev_nodes =
          match last_day, day with
          | Some last, Some day when not (Date.equal last day) ->
            keyed (sprintf "day%d" i) (day_separator times day) :: rev_nodes
          | _ -> rev_nodes
        in
        ( Option.first_some day last_day
        , i + 1
        , keyed (Int.to_string i) (entry ~fold times chat ~n:i e) :: rev_nodes ))
  in
  div "entries" (List.rev rev_nodes)
;;
