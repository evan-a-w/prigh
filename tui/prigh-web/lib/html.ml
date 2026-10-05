open! Core
open! Import
open Js_of_ocaml

let cls_attrs cls attrs =
  if String.is_empty cls then attrs else Attr.class_ cls :: attrs
;;

let div ?(cls = "") ?(attrs = []) children =
  Node.div ~attrs:(cls_attrs cls attrs) children
;;

let span ?(cls = "") ?(attrs = []) text =
  Node.span ~attrs:(cls_attrs cls attrs) [ Node.text text ]
;;

let classes always optional =
  Attr.classes
    (always @ List.filter_map optional ~f:(fun (c, on) -> Option.some_if on c))
;;

let button
      ?(cls = "")
      ?title
      ?(disabled = false)
      ?(attrs = [])
      ~on_click
      children
  =
  Node.button
    ~attrs:
      ([ Attr.class_ (String.strip ("btn " ^ cls))
       ; Attr.type_ "button"
       ; Attr.on_click (fun _ -> on_click)
       ]
       @ Option.value_map title ~default:[] ~f:(fun t ->
         [ Attr.title t; Attr.create "aria-label" t ])
       @ (if disabled then [ Attr.disabled ] else [])
       @ attrs)
    children
;;

module Icon = struct
  type t =
    | Menu
    | Sidebar
    | Plus
    | Search
    | Trash
    | Pencil
    | Close
    | Send
    | Stop
    | Chevron
    | Arrow_down
    | Brain
    | Folder
    | Branch
    | Cpu
    | Logout
    | Undo
    | External
    | Key
    | Server
    | Help
    | Bot
    | Back
    | Locate
    | User
    | Users
    | User_plus
    | Copy
    | Check
  [@@deriving sexp_of]

  (* Paths in the style of Lucide (24x24, stroked). *)
  let paths = function
    | Menu -> {|<path d="M4 6h16M4 12h16M4 18h16"/>|}
    | Sidebar ->
      {|<rect x="3" y="4" width="18" height="16" rx="2"/><path d="M9 4v16"/>|}
    | Plus -> {|<path d="M12 5v14M5 12h14"/>|}
    | Search -> {|<circle cx="11" cy="11" r="7"/><path d="m20 20-3.5-3.5"/>|}
    | Trash ->
      {|<path d="M4 7h16M10 11v6M14 11v6M6 7l1 13h10l1-13M9 7V4h6v3"/>|}
    | Pencil -> {|<path d="M4 20h4L19 9l-4-4L4 16v4zM13.5 6.5l4 4"/>|}
    | Close -> {|<path d="M6 6l12 12M18 6 6 18"/>|}
    | Send -> {|<path d="M12 19V5M5 12l7-7 7 7"/>|}
    | Stop ->
      {|<rect x="6" y="6" width="12" height="12" rx="2" fill="currentColor"/>|}
    | Chevron -> {|<path d="m6 9 6 6 6-6"/>|}
    | Arrow_down -> {|<path d="M12 5v14"/><path d="m19 12-7 7-7-7"/>|}
    | Brain ->
      {|<path d="M12 5a3 3 0 0 0-5.8-1A3 3 0 0 0 4 8a3 3 0 0 0 0 6 3 3 0 0 0 3 4 3 3 0 0 0 5 1M12 5a3 3 0 0 1 5.8-1A3 3 0 0 1 20 8a3 3 0 0 1 0 6 3 3 0 0 1-3 4 3 3 0 0 1-5 1M12 5v14"/>|}
    | Folder ->
      {|<path d="M3 7a2 2 0 0 1 2-2h4l2 2h8a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"/>|}
    | Branch ->
      {|<circle cx="6" cy="5" r="2"/><circle cx="6" cy="19" r="2"/><circle cx="18" cy="7" r="2"/><path d="M6 7v10M18 9c0 5-6 4-11 8"/>|}
    | Cpu ->
      {|<rect x="6" y="6" width="12" height="12" rx="2"/><path d="M9 2v4M15 2v4M9 18v4M15 18v4M2 9h4M2 15h4M18 9h4M18 15h4"/>|}
    | Logout ->
      {|<path d="M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4M16 17l5-5-5-5M21 12H9"/>|}
    | Undo -> {|<path d="M9 14 4 9l5-5"/><path d="M4 9h11a5 5 0 0 1 0 10h-3"/>|}
    | External ->
      {|<path d="M14 4h6v6M20 4 10 14M18 14v5a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V7a1 1 0 0 1 1-1h5"/>|}
    | Key ->
      {|<circle cx="8" cy="15" r="4"/><path d="m11 12 9-9M17 6l3 3M14 9l2 2"/>|}
    | Server ->
      {|<rect x="3" y="4" width="18" height="7" rx="2"/><rect x="3" y="13" width="18" height="7" rx="2"/><path d="M7 7.5h.01M7 16.5h.01"/>|}
    | Help ->
      {|<circle cx="12" cy="12" r="9"/><path d="M9.5 9a2.5 2.5 0 0 1 4.9.8c0 1.7-2.4 2.2-2.4 3.7M12 17h.01"/>|}
    | Bot ->
      {|<rect x="4" y="8" width="16" height="12" rx="2"/><path d="M12 8V4M9 13v2M15 13v2M2 14h2M20 14h2"/>|}
    | Back -> {|<path d="M19 12H5M12 19l-7-7 7-7"/>|}
    | Locate ->
      {|<circle cx="12" cy="12" r="7"/><circle cx="12" cy="12" r="2"/><path d="M12 2v3M12 19v3M2 12h3M19 12h3"/>|}
    | User -> {|<circle cx="12" cy="8" r="4"/><path d="M4 21a8 8 0 0 1 16 0"/>|}
    | Users ->
      {|<circle cx="9" cy="8" r="4"/><path d="M2 21a7 7 0 0 1 14 0M16 3.5a4 4 0 0 1 0 9M22 21a7 7 0 0 0-4-6.3"/>|}
    | User_plus ->
      {|<circle cx="9" cy="8" r="4"/><path d="M2 21a7 7 0 0 1 14 0M19 8v6M16 11h6"/>|}
    | Copy ->
      {|<rect x="8" y="8" width="13" height="13" rx="2"/><path d="M16 8V5a2 2 0 0 0-2-2H5a2 2 0 0 0-2 2v9a2 2 0 0 0 2 2h3"/>|}
    | Check -> {|<path d="m5 12 5 5L20 7"/>|}
  ;;

  let name t = String.lowercase (Sexp.to_string (sexp_of_t t))

  let view ?(cls = "") t =
    Node.inner_html_svg
      ~override_vdom_for_testing:
        (lazy (Node.create "icon" ~attrs:[ Attr.class_ (name t) ] []))
      ~tag:"svg"
      ~attrs:
        [ Attr.class_ (String.strip ("icon " ^ cls))
        ; Attr.create "viewBox" "0 0 24 24"
        ; Attr.create "fill" "none"
        ; Attr.create "stroke" "currentColor"
        ; Attr.create "stroke-width" "2"
        ; Attr.create "stroke-linecap" "round"
        ; Attr.create "stroke-linejoin" "round"
        ; Attr.create "aria-hidden" "true"
        ]
      ~this_html_is_sanitized_and_is_totally_safe_trust_me:(paths t)
      ()
  ;;
end

let icon = Icon.view

let caret (ev : Dom_html.event Js.t) =
  Js.Opt.case
    ev##.target
    (fun () -> None)
    (fun target ->
       match Dom_html.tagged target with
       | Textarea t ->
         Some
           (Utf16.byte_offset
              (Js.to_string t##.value)
              ~utf16:t##.selectionStart)
       | Input i ->
         Some
           (Utf16.byte_offset
              (Js.to_string i##.value)
              ~utf16:i##.selectionStart)
       | _ -> None)
;;
