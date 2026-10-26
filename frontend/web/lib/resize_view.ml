open! Core
open! Import
module Handle = Resize.Handle

(* Where the size shows while it is dragged, before the model has it. *)
let target : Handle.t -> string = function
  | Sidebar -> ".sidebar"
  | Files | Files_preview -> "#files-panel"
  | Agents -> "#agents-panel"
  | Terminal -> "#terminal-panel"
  | Review_chat -> "main.main"
  | Review_files -> ".review-body"
  | Review_margin -> "#review-page"
;;

let handle (m : App.Model.t) ~inject h =
  if m.narrow
  then Node.none
  else (
    let b = Resize.bounds m.layout (App.Model.shown m) h in
    let int name v = Attr.create name (Int.to_string v) in
    Node.div
      ~attrs:
        [ Attr.classes [ "resizer"; "resizer-" ^ Handle.name h ]
        ; Attr.role "separator"
        ; Attr.tabindex 0
        ; Attr.create "aria-label" (Handle.label h)
        ; Attr.create
            "aria-orientation"
            (if Handle.vertical h then "vertical" else "horizontal")
        ; int "aria-valuenow" b.value
        ; int "aria-valuemin" b.min
        ; int "aria-valuemax" b.max
        ; Attr.title "Drag to resize, double-click for the default size"
        ; Attr.create "data-resize" (Handle.name h)
        ; Attr.create "data-resize-var" (Handle.var h)
        ; int "data-resize-grows" (Handle.grows h)
        ; Attr.create "data-resize-target" (target h)
        ; Attr.on_double_click (fun _ -> inject (App.Action.Resize (Reset h)))
        ]
      [])
;;

let vars (m : App.Model.t) =
  Attr.many
    (List.map
       (Resize.vars m.layout (App.Model.shown m))
       ~f:(fun (var, value) ->
         Attr.css_var ~name:(String.chop_prefix_exn var ~prefix:"--") value))
;;
