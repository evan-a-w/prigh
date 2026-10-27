open! Core
open! Import
open Html
module Action = App.Action

let hidden_names filter =
  Transcript_filter.hidden filter
  |> List.map ~f:(fun kind ->
    String.lowercase (Transcript_filter.Kind.label kind))
  |> String.concat ~sep:", "
;;

let button (m : App.Model.t) ~inject =
  let hidden = List.length (Transcript_filter.hidden m.transcript_filter) in
  let open_ =
    match m.dialog with
    | Some (Filter _) -> true
    | _ -> false
  in
  Html.button
    ~cls:
      (String.concat
         ~sep:" "
         ([ "icon ghost filter-toggle" ]
          @ (if hidden > 0 then [ "active" ] else [])
          @ if open_ then [ "open" ] else []))
    ~title:
      (if hidden > 0
       then
         sprintf
           "Filter: hiding %s (/filter)"
           (hidden_names m.transcript_filter)
       else "Filter the transcript (/filter)")
    ~label:
      (if hidden > 0
       then
         sprintf
           "Filter the transcript: %d %s hidden"
           hidden
           (if hidden = 1 then "kind" else "kinds")
       else "Filter the transcript")
    ~attrs:
      [ Attr.id Filter_menu.anchor
      ; Attr.create "aria-haspopup" "menu"
      ; Attr.create "aria-expanded" (Bool.to_string open_)
      ]
    ~on_click:(inject Action.Open_filter)
    (icon Filter
     ::
     (if hidden > 0 then [ span ~cls:"badge" (Int.to_string hidden) ] else []))
;;

let menu (m : App.Model.t) (menu : Filter_menu.t) ~inject =
  let filter = m.transcript_filter in
  let selected = Filter_menu.selected menu in
  let row (row : Filter_menu.Row.t) =
    let label, detail, check =
      match row with
      | Kind kind ->
        ( Transcript_filter.Kind.label kind
        , Transcript_filter.Kind.detail kind
        , Some (Transcript_filter.shows filter kind) )
      | Show_all -> "Show all", "everything the transcript has", None
    in
    Node.div
      ~attrs:
        [ classes
            [ "menu-item"; "filter-item" ]
            [ "selected", Filter_menu.Row.equal row selected
            ; "show-all", Option.is_none check
            ]
        ; Attr.role
            (match check with
             | Some _ -> "menuitemcheckbox"
             | None -> "menuitem")
        ; (match check with
           | Some on -> Attr.create "aria-checked" (Bool.to_string on)
           | None -> Attr.empty)
        ; (match row, check with
           | Show_all, None when Transcript_filter.is_all filter ->
             Attr.create "aria-disabled" "true"
           | _ -> Attr.empty)
        ; Attr.on_click (fun _ -> inject (Action.Filter_press (Some row)))
        ]
      [ (match check with
         | Some on -> Html.check on
         | None -> icon Restart)
      ; div
          ~cls:"menu-text"
          [ span ~cls:"menu-label" label; span ~cls:"menu-detail" detail ]
      ]
  in
  let kinds, rest =
    List.partition_tf Filter_menu.Row.all ~f:(function
      | Kind _ -> true
      | Show_all -> false)
  in
  Node.div
    ~attrs:
      [ Attr.class_ "menu-backdrop"
      ; Attr.on_click (fun ev ->
          if Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
          then inject Action.Dialog_accept
          else Effect.Ignore)
      ]
    [ Node.div
        ~attrs:
          [ Attr.class_ "filter-menu menu-surface"
          ; Attr.id "dialog"
          ; Attr.role "menu"
          ; Attr.create "aria-label" "Show in the transcript"
          ; Attr.tabindex (-1)
          ; Attr.create "data-anchor" Filter_menu.anchor
          ]
        [ div ~cls:"menu-heading" [ Node.text "Show in the transcript" ]
        ; div ~cls:"menu-section" (List.map kinds ~f:row)
        ; div ~cls:"menu-section" (List.map rest ~f:row)
        ; div
            ~cls:"menu-keys"
            [ Node.text "Space shows or hides · Enter keeps · Esc undoes" ]
        ]
    ]
;;

let note (m : App.Model.t) ~inject =
  if Transcript_filter.is_all m.transcript_filter
  then []
  else (
    let hidden = Chat_view.hidden ~fold:(App.Model.fold m) m.chat in
    List.return
    @@ Node.div
         ~attrs:[ Attr.class_ "filter-note"; Attr.role "status" ]
         [ icon Filter
         ; span
             ~cls:"filter-note-text"
             (sprintf
                "Hiding %s%s"
                (hidden_names m.transcript_filter)
                (match hidden with
                 | 0 -> ": nothing loaded is hidden"
                 | 1 -> " · 1 item hidden"
                 | n -> sprintf " · %d items hidden" n))
         ; Html.button
             ~cls:"link filter-show-all"
             ~on_click:(inject (Action.Run "/filter all"))
             [ Node.text "Show all" ]
         ])
;;
