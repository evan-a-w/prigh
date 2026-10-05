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

let user ({ text; images } : Message.User.t) =
  div
    "msg user"
    [ Image_view.thumbs images
    ; (if String.is_empty (String.strip text)
       then Node.none
       else div "bubble" [ Node.text text ])
    ]
;;

let thinking ~live text =
  if live
  then
    div
      "thinking live"
      [ div
          "thinking-head"
          [ Node.span ~attrs:[ Attr.class_ "spinner" ] []
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
  | Aborted -> div "stop aborted" [ Node.text "Interrupted" ]
  | Length ->
    div
      "stop length"
      [ Node.text "Stopped at the output token limit — ask it to continue." ]
  | End_turn | Tool_use -> Node.none
;;

let footer (message : Message.Assistant.t) =
  match message.stop_reason with
  | End_turn when message.usage.output > 0 ->
    let { Usage.input; output; cache_read } = message.usage in
    div
      "meta"
      [ Node.text
          (String.concat
             ~sep:" · "
             ([ message.model; tokens (input + cache_read) ^ " in" ]
              @ (if cache_read > 0
                 then [ tokens cache_read ^ " cached" ]
                 else [])
              @ [ tokens output ^ " out" ]))
      ]
  | _ -> Node.none
;;

module Deps = struct
  type t =
    { running : bool
    ; tools : Chat.Tool.t option list
    }

  let equal a b =
    Bool.equal a.running b.running
    && List.equal (Option.equal phys_equal) a.tools b.tools
  ;;
end

let entries : (Chat.Entry.t, Deps.t) View_cache.t = View_cache.create ()
let chats : (Chat.t, unit) View_cache.t = View_cache.create ()

let rec assistant chat (message : Message.Assistant.t) ~streaming =
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
          ~nested:view
          ~streaming:(streaming && last)
          ~running:(Chat.running chat)
          call
          (Chat.tool chat call.id))
  in
  let blocks = present blocks in
  let pending =
    if streaming && count = 0
    then div "pending" (List.init 3 ~f:(fun _ -> Node.span []))
    else Node.none
  in
  let stopped = stopped message in
  if List.is_empty blocks && (not streaming) && phys_equal stopped Node.none
  then Node.none
  else
    div
      (if streaming then "msg assistant streaming" else "msg assistant")
      ((pending :: blocks)
       @ [ stopped; (if streaming then Node.none else footer message) ])

and render_entry chat (entry : Chat.Entry.t) =
  match entry with
  | User u ->
    (match Prigh_ui.Delivery.parse u.text with
     | Some sections -> div "msg" [ Delivery_view.view sections ]
     | None -> user u)
  | Notice text -> div "msg notice" [ Node.text text ]
  | Shell call ->
    let tool = Chat.tool chat call.id in
    div
      "msg shell"
      [ Tool_view.view
          ~nested:view
          ~streaming:false
          ~running:
            (Option.value_map tool ~default:true ~f:(fun t ->
               Option.is_none t.result))
          { call with name = "bash" }
          tool
      ]
  | Compaction summary ->
    folded
      ~cls:"msg compaction"
      ~label:"Context compacted"
      ~preview:(Markdown.preview summary)
      (Markdown_view.render summary)
  | Assistant { message; streaming } -> assistant chat message ~streaming

(* An entry's card depends on the state of its tool calls, and on whether the
   agent is still running while one of them has no result. *)
and entry chat (entry : Chat.Entry.t) =
  let tools =
    match entry with
    | Assistant { message; _ } ->
      List.filter_map message.content ~f:(function
        | Tool_call call -> Some (Chat.tool chat call.id)
        | Text _ | Thinking _ -> None)
    | Shell call -> [ Chat.tool chat call.id ]
    | User _ | Notice _ | Compaction _ -> []
  in
  let running =
    Chat.running chat
    && List.exists tools ~f:(fun tool ->
      Option.for_all tool ~f:(fun tool -> Option.is_none tool.result))
  in
  View_cache.find
    entries
    entry
    ~deps:{ Deps.running; tools }
    ~equal:Deps.equal
    ~f:(fun () -> render_entry chat entry)

and view chat =
  View_cache.find chats chat ~deps:() ~equal:Unit.equal ~f:(fun () ->
    div "entries" (List.map (Chat.entries chat) ~f:(entry chat)))
;;
