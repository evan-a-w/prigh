open! Core
open! Import
open Html

let grip () =
  Node.div
    ~attrs:[ Attr.class_ "sheet-grip"; Attr.create "data-slot" "drawer-handle" ]
    []
;;

let row ?(on = false) ?id ?detail ?badge ~cls ~name ~glyph ~on_click () =
  Node.button
    ~attrs:
      [ classes ("sheet-row" :: String.split cls ~on:' ') [ "on", on ]
      ; Option.value_map id ~default:Attr.empty ~f:Attr.id
      ; Attr.type_ "button"
      ; (if on then Attr.create "aria-pressed" "true" else Attr.empty)
      ; Attr.on_click (fun _ -> on_click)
      ]
    [ icon glyph
    ; Node.span
        ~attrs:[ Attr.class_ "sheet-row-text" ]
        [ span ~cls:"sheet-row-name" name
        ; (match detail with
           | Some d -> span ~cls:"sheet-row-detail" d
           | None -> Node.none)
        ]
    ; (match badge with
       | Some b -> span ~cls:"badge" b
       | None -> Node.none)
    ]
;;
