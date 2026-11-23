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
    let selected = Filter_menu.Row.equal row selected in
    Node.div
      ~attrs:
        [ classes
            [ "menu-item filter-item"
            ; Menu_view.item_look
            ; "gap-2.5 px-2 py-1.5"
            ]
            [ "selected", selected; "show-all", Option.is_none check ]
        ; Attr.create
            "data-slot"
            (match check with
             | Some _ -> "dropdown-menu-checkbox-item"
             | None -> "dropdown-menu-item")
        ; (if selected then Attr.create "data-highlighted" "" else Attr.empty)
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
          ~cls:"menu-text flex min-w-0 flex-col leading-tight"
          [ span ~cls:Menu_view.label_look label
          ; span
              ~cls:"menu-detail truncate text-xs text-muted-foreground"
              detail
          ]
      ]
  in
  let kinds, rest =
    List.partition_tf Filter_menu.Row.all ~f:(function
      | Kind _ -> true
      | Show_all -> false)
  in
  Menu_view.backdrop
    ~on_close:(inject Action.Dialog_accept)
    [ Menu_view.surface
        ~cls:
          "filter-menu fixed w-[min(340px,calc(100vw-16px))] \
           max-h-[calc(100vh-16px)] [&:not(.placed)]:opacity-0 \
           [&:not(.placed)]:pointer-events-none motion-reduce:animate-none"
        ~attrs:
          [ Attr.id "dialog"; Attr.create "data-anchor" Filter_menu.anchor ]
        ~label:"Show in the transcript"
        [ Menu_view.heading "Show in the transcript"
        ; Menu_view.section (List.map kinds ~f:row)
        ; Menu_view.section (List.map rest ~f:row)
        ; div
            ~cls:
              "menu-keys -mx-1 mt-1 border-t border-border px-2.5 pt-1.5 pb-1 \
               text-xs text-muted-foreground pointer-coarse:hidden"
            [ Node.text "Space shows or hides · Enter keeps · Esc undoes" ]
        ]
    ]
;;

let note (m : App.Model.t) ~inject =
  if Transcript_filter.is_all m.transcript_filter
  then []
  else (
    let hidden =
      Chat_view.hidden
        ~deleting:m.trimmed.deleting
        ~fold:(App.Model.fold m)
        m.chat
    in
    List.return
    @@ Node.div
         ~attrs:
           [ Attr.class_
               "filter-note mb-3.5 flex max-w-(--column) items-center gap-2 \
                text-xs text-muted-foreground narrow:mr-10 [&>.icon]:size-3.5 \
                [&>.icon]:shrink-0 [&>.icon]:text-primary after:order-1 \
                after:min-w-3 after:flex-1 after:border-t after:border-border \
                after:content-['']"
           ; Attr.role "status"
           ]
         [ icon Filter
         ; span
             ~cls:"filter-note-text min-w-0 truncate"
             (sprintf
                "Hiding %s%s"
                (hidden_names m.transcript_filter)
                (match hidden with
                 | 0 -> ": nothing loaded is hidden"
                 | 1 -> " · 1 item hidden"
                 | n -> sprintf " · %d items hidden" n))
         ; Html.button
             ~cls:"filter-show-all order-2 h-auto shrink-0 px-0 text-primary"
             ~variant:Link
             ~size:Xs
             ~on_click:(inject (Action.Run "/filter all"))
             [ Node.text "Show all" ]
         ])
;;
