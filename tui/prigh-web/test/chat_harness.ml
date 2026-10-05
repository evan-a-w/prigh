open! Core
open Prigh_web
open Prigh_protocol

let decode json =
  match Event.of_json (Jsonaf.of_string json) with
  | Ok event -> event
  | Error e -> raise_s [%message "bad event" json (e : Error.t)]
;;

let with_card chat (event : Event.t) =
  match event with
  | (Tool_start call | Tool_end { call; _ })
    when Option.is_none (Chat.tool chat call.id) ->
    Chat.apply
      chat
      (Message_end
         (Assistant
            { content = [ Tool_call call ]
            ; stop_reason = Tool_use
            ; usage = Usage.zero
            ; model = "m"
            }))
  | _ -> chat
;;

let apply chat json =
  let event = decode json in
  Chat.apply (with_card chat event) event
;;

let chat ?(running = false) events =
  let chat =
    List.fold events ~init:(Chat.apply Chat.empty Agent_start) ~f:apply
  in
  if running then chat else Chat.apply chat (Agent_end [])
;;

let show ?selector chat = Render.html ?selector (Chat_view.view chat)
let text ?selector chat = Render.text ?selector (Chat_view.view chat)
