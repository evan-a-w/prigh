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

let row ?(cls = "") ?note ?scope ?(wide = false) ?keys ~name ~about controls =
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
        ; (match keys with
           | None -> span ~cls:"setting-about" about
           | Some keys ->
             let extra cls text =
               Node.span ~attrs:[ Attr.class_ cls ] [ Node.text text ]
             in
             (* The shortcut goes before the sentence's full stop. *)
             let about, stop =
               match String.chop_suffix about ~suffix:"." with
               | Some about -> about, "."
               | None -> about, ""
             in
             Node.span
               ~attrs:[ Attr.class_ "setting-about" ]
               [ Node.text about
               ; extra "keyboard" (sprintf " (%s)" keys)
               ; Node.text stop
               ])
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

(* A segmented control whose choices each carry a line about them: on a
   desktop the line is only the tooltip, on a phone the choices are a list of
   rows, the line under each name (too many, or too long, to sit side by
   side). *)
let choice_list ?(disabled = false) ~label choices =
  Node.div
    ~attrs:
      [ Attr.class_ "segmented choice-list"
      ; Attr.role "group"
      ; Attr.create "aria-label" label
      ]
    (List.map choices ~f:(fun (text, about, title, current, on_click) ->
       Node.button
         ~attrs:
           [ classes [ "segment" ] [ "on", current ]
           ; Attr.type_ "button"
           ; Attr.create "aria-pressed" (Bool.to_string current)
           ; Attr.create "aria-label" text
           ; Attr.title title
           ; (if disabled
              then Attr.disabled
              else Attr.on_click (fun _ -> on_click))
           ]
         [ Node.span ~attrs:[ Attr.class_ "segment-name" ] [ Node.text text ]
         ; Node.span ~attrs:[ Attr.class_ "segment-about" ] [ Node.text about ]
         ]))
;;
