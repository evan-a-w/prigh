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
      ?label
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
       @ Option.value_map title ~default:[] ~f:(fun t -> [ Attr.title t ])
       @ Option.value_map label ~default:[] ~f:(fun l ->
         [ Attr.create "aria-label" l ])
       @ (if disabled then [ Attr.disabled ] else [])
       @ attrs)
    children
;;

let lamp ?title () =
  Node.span
    ~attrs:
      (Attr.class_ "lamp"
       ::
       (match title with
        | Some t ->
          [ Attr.title t; Attr.role "img"; Attr.create "aria-label" t ]
        | None -> [ Attr.create "aria-hidden" "true" ]))
    []
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
    | Terminal
    | Restart
    | Target
    | Clock
    | Archive
    | Unarchive
    | Bell
    | Bell_off
    | Shield
    | Gear
    | Cross
    | Dash
    | Square
    | Warning
    | Next
    | Ring
    | Toc
    | Repo
    | Branch_plus
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
    | Terminal -> {|<path d="m4 17 6-6-6-6M12 19h8"/>|}
    | Archive ->
      {|<rect x="2" y="3" width="20" height="5" rx="1"/><path d="M4 8v11a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8M10 12h4"/>|}
    | Unarchive ->
      {|<rect x="2" y="3" width="20" height="5" rx="1"/><path d="M4 8v11a2 2 0 0 0 2 2h2M20 8v11a2 2 0 0 1-2 2h-2M9 15l3-3 3 3M12 12v9"/>|}
    | Restart -> {|<path d="M3 12a9 9 0 1 0 2.6-6.4L3 8"/><path d="M3 3v5h5"/>|}
    | Target ->
      {|<circle cx="12" cy="12" r="9"/><circle cx="12" cy="12" r="5"/><circle cx="12" cy="12" r="1"/>|}
    | Clock -> {|<circle cx="12" cy="12" r="9"/><path d="M12 7v5l3 2"/>|}
    | Bell ->
      {|<path d="M6 8a6 6 0 0 1 12 0c0 7 3 9 3 9H3s3-2 3-9M10.3 21a1.9 1.9 0 0 0 3.4 0"/>|}
    | Bell_off ->
      {|<path d="M8.7 3A6 6 0 0 1 18 8a21 21 0 0 0 .6 5M17 17H3s3-2 3-9a4.7 4.7 0 0 1 .3-2M10.3 21a1.9 1.9 0 0 0 3.4 0M2 2l20 20"/>|}
    | Shield ->
      {|<path d="M12 21s7-3.5 7-9V5.5L12 3 5 5.5V12c0 5.5 7 9 7 9z"/>|}
    | Gear ->
      {|<path d="M12.2 2h-.4a2 2 0 0 0-2 2v.2a2 2 0 0 1-1 1.7l-.4.3a2 2 0 0 1-2 0l-.2-.1a2 2 0 0 0-2.7.7l-.2.4a2 2 0 0 0 .7 2.7l.2.1a2 2 0 0 1 1 1.7v.5a2 2 0 0 1-1 1.8l-.2.1a2 2 0 0 0-.7 2.7l.2.4a2 2 0 0 0 2.7.7l.2-.1a2 2 0 0 1 2 0l.4.3a2 2 0 0 1 1 1.7v.2a2 2 0 0 0 2 2h.4a2 2 0 0 0 2-2v-.2a2 2 0 0 1 1-1.7l.4-.3a2 2 0 0 1 2 0l.2.1a2 2 0 0 0 2.7-.7l.2-.4a2 2 0 0 0-.7-2.7l-.2-.1a2 2 0 0 1-1-1.8v-.5a2 2 0 0 1 1-1.7l.2-.1a2 2 0 0 0 .7-2.7l-.2-.4a2 2 0 0 0-2.7-.7l-.2.1a2 2 0 0 1-2 0l-.4-.3a2 2 0 0 1-1-1.7V4a2 2 0 0 0-2-2z"/><circle cx="12" cy="12" r="3"/>|}
    (* For [mark]: as heavy as [Check] beside them at 13-14px. *)
    | Cross -> {|<path d="M6.5 6.5l11 11M17.5 6.5l-11 11"/>|}
    | Dash -> {|<path d="M6.5 12h11"/>|}
    | Square ->
      {|<rect x="7" y="7" width="10" height="10" rx="1.5" fill="currentColor"/>|}
    | Warning ->
      {|<path d="M10.3 4.2 2.6 17.5A2 2 0 0 0 4.3 20.5h15.4a2 2 0 0 0 1.7-3L13.7 4.2a2 2 0 0 0-3.4 0zM12 10v4M12 17.5h.01"/>|}
    | Next -> {|<path d="m10 7 5 5-5 5"/>|}
    | Ring -> {|<circle cx="12" cy="12" r="4.5"/>|}
    | Toc -> {|<path d="M10 6h10M10 12h10M10 18h10M4 6h2M4 12h2M4 18h2"/>|}
    | Repo ->
      {|<path d="M9 20H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h3.9a2 2 0 0 1 1.69.9l.81 1.2a2 2 0 0 0 1.67.9H20a2 2 0 0 1 2 2v5"/><circle cx="13" cy="12" r="2"/><path d="M18 19c-2.8 0-5-2.2-5-5v8"/><circle cx="20" cy="19" r="2"/>|}
    | Branch_plus ->
      {|<path d="M6 3v12"/><circle cx="18" cy="6" r="3"/><circle cx="6" cy="18" r="3"/><path d="M15 6a9 9 0 0 0-9 9M18 15v6M21 18h-6"/>|}
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

let mark ?(cls = "") ?label t =
  Node.span
    ~attrs:
      (Attr.class_ (String.strip ("mark " ^ cls))
       ::
       (match label with
        | Some l ->
          [ Attr.title l; Attr.role "img"; Attr.create "aria-label" l ]
        | None -> [ Attr.create "aria-hidden" "true" ]))
    [ icon t ]
;;

let check ?(radio = false) on =
  Node.span
    ~attrs:
      [ classes [ "check" ] [ "on", on; "radio", radio ]
      ; Attr.create "aria-hidden" "true"
      ]
    (if on && not radio then [ icon Check ] else [])
;;

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

let rec inert (node : Node.t) : Node.t =
  match node with
  | Element e ->
    Element
      (Node.Element.map_attrs e ~f:(fun a ->
         Attr.many [ a; Attr.create "inert" "" ]))
  | Lazy { key; t } -> Lazy { key; t = lazy (inert (Lazy.force t)) }
  | Fragment nodes -> Fragment (List.map nodes ~f:inert)
  | (None | Text _ | Widget _) as node -> node
;;
