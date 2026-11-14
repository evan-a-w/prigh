open! Core
open! Import
open Html

let reason (m : App.Model.t) =
  match m.connection, m.state with
  | Connected, Some s when not s.chat_mode -> s.no_host
  | _ -> None
;;

let view m ~inject =
  match reason m with
  | None -> Node.none
  | Some reason ->
    div
      ~cls:"no-host-banner"
      ~attrs:
        [ Attr.role "status"
        ; Attr.create "aria-live" "polite"
        ; Attr.title (String.capitalize reason)
        ]
      [ icon Server_off
      ; Node.span
          ~attrs:[ Attr.class_ "no-host-banner-text" ]
          [ Node.text (App.no_host_headline reason) ]
      ; button
          ~cls:"no-host-banner-chat"
          ~title:
            "Switch this session to chat mode (no files or commands) and send \
             your message"
          ~on_click:(inject App.Action.Chat_without_host)
          [ icon Message
          ; span ~cls:"long" "Chat without a host"
          ; span ~cls:"short" "Chat anyway"
          ]
      ]
;;
