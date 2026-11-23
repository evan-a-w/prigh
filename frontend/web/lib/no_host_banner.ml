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
    Node.div
      ~attrs:
        [ Attr.create "data-slot" "alert"
        ; Attr.role "status"
        ; Attr.create "aria-live" "polite"
        ; Attr.title (String.capitalize reason)
        ; Attr.class_
            (Loon.Alert.classes
               ~class_:
                 "no-host-banner loon flex items-center gap-2 border-primary/40 \
                  bg-primary/10 py-1 pr-1 pl-2.5 text-[13.5px] \
                  [&>.icon]:shrink-0 [&>.icon]:text-primary"
               ())
        ]
      [ icon Server_off
      ; Node.span
          ~attrs:
            [ Attr.class_
                "no-host-banner-text line-clamp-2 min-w-0 flex-1 leading-tight"
            ]
          [ Node.text (App.no_host_headline reason) ]
      ; button
          ~cls:"no-host-banner-chat shrink-0 gap-1.5 text-primary narrow:h-11"
          ~variant:Outline
          ~size:Sm
          ~title:
            "Switch this session to chat mode (no files or commands) and send \
             your message"
          ~on_click:(inject App.Action.Chat_without_host)
          [ icon Message
          ; span ~cls:"long max-[359px]:hidden" "Chat without a host"
          ; span ~cls:"short hidden max-[359px]:inline" "Chat anyway"
          ]
      ]
;;
