open! Core
open! Import
open Html

let reason (m : App.Model.t) =
  match m.connection, m.state with
  | Connected, Some s -> s.engine.unavailable
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
        ; Attr.title reason
        ; Attr.class_
            (Loon.Alert.classes
               ~class_:
                 "engine-banner loon flex flex-wrap items-center gap-2 \
                  border-destructive/40 bg-destructive/10 py-1 pr-1 pl-2.5 \
                  text-[13.5px] [&>.icon]:shrink-0 [&>.icon]:text-destructive"
               ())
        ]
      [ icon Server_off
      ; Node.span
          ~attrs:
            [ Attr.class_
                "engine-banner-text min-w-[12rem] flex-1 leading-tight"
            ]
          [ Node.text (String.capitalize reason) ]
      ; button
          ~cls:"engine-banner-new ml-auto shrink-0 gap-1.5 narrow:h-11"
          ~variant:Outline
          ~size:Sm
          ~title:"Start a new session on what this backend runs (/new)"
          ~on_click:(inject App.Action.New_session)
          [ icon Plus; span "New session" ]
      ]
;;
