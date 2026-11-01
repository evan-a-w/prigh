open! Core
open! Import
open Html

module Scope = struct
  type t =
    | Browser
    | Account
    | Backend

  let tag = function
    | Browser -> "This browser"
    | Account -> "Your account"
    | Backend -> "Backend"
  ;;

  let title = function
    | Browser -> "Kept in this browser only"
    | Account -> "Follows you to every browser you sign in on"
    | Backend -> "Kept in the backend's configuration"
  ;;
end

let row ?(cls = "") ?note ?scope ?(wide = false) ~name ~about controls =
  Node.div
    ~attrs:
      [ classes
          (String.split cls ~on:' '
           |> List.filter ~f:(Fn.non String.is_empty)
           |> List.cons "setting")
          [ "setting-wide", wide ]
      ]
    [ div
        ~cls:"setting-text"
        [ div
            ~cls:"setting-head"
            [ span ~cls:"setting-name" name
            ; (match scope with
               | Some scope ->
                 Node.span
                   ~attrs:
                     [ Attr.class_ "setting-scope"
                     ; Attr.title (Scope.title scope)
                     ]
                   [ Node.text (Scope.tag scope) ]
               | None -> Node.none)
            ]
        ; span ~cls:"setting-about" about
        ; (match note with
           | Some (level, text) -> span ~cls:("setting-note " ^ level) text
           | None -> Node.none)
        ]
    ; div ~cls:"setting-control" controls
    ]
;;

let group ?title ?note rows =
  Node.section
    ~attrs:[ Attr.class_ "settings-group" ]
    [ (match title, note with
       | None, None -> Node.none
       | _ ->
         div
           ~cls:"settings-group-head"
           [ (match title with
              | Some title -> Node.h4 [ Node.text title ]
              | None -> Node.none)
           ; (match note with
              | Some note -> span ~cls:"settings-group-note" note
              | None -> Node.none)
           ])
    ; div ~cls:"settings-card" rows
    ]
;;

let switch ?(disabled = false) ~label ~title ~on ~on_click () =
  Node.button
    ~attrs:
      [ classes [ "switch" ] [ "on", on ]
      ; Attr.type_ "button"
      ; Attr.role "switch"
      ; Attr.create "aria-checked" (Bool.to_string on)
      ; Attr.create "aria-label" label
      ; Attr.title title
      ; (if disabled then Attr.disabled else Attr.on_click (fun _ -> on_click))
      ]
    [ Node.span ~attrs:[ Attr.class_ "switch-knob" ] [] ]
;;

let select ?(disabled = false) ~cls ~title ~label ~on_click value =
  button
    ~cls:("small setting-select " ^ cls)
    ~title
    ~label
    ~disabled
    ~on_click
    [ span ~cls:"setting-select-value" value; icon ~cls:"chevron" Chevron ]
;;

let segmented ?(disabled = false) ~label choices =
  Node.div
    ~attrs:
      [ Attr.class_ "segmented"
      ; Attr.role "group"
      ; Attr.create "aria-label" label
      ]
    (List.map choices ~f:(fun (text, title, current, on_click) ->
       Node.button
         ~attrs:
           [ classes [ "segment" ] [ "on", current ]
           ; Attr.type_ "button"
           ; Attr.create "aria-pressed" (Bool.to_string current)
           ; Attr.title title
           ; (if disabled
              then Attr.disabled
              else Attr.on_click (fun _ -> on_click))
           ]
         [ Node.text text ]))
;;
