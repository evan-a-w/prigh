open! Core
open! Import
open Html
module Action = App.Action

(* Glyphs the shared icon set does not have, drawn the same way. *)
module Glyph = struct
  type t =
    | File
    | Folder
    | Link
    | Up
    | Upload
    | Download
    | Attach
    | Folder_plus
    | Eye
    | Refresh
    | At
  [@@deriving sexp_of]

  let paths = function
    | File ->
      {|<path d="M14 3H7a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V8z"/><path d="M14 3v5h5"/>|}
    | Folder ->
      {|<path d="M3 7a2 2 0 0 1 2-2h4l2 2h8a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"/>|}
    | Link ->
      {|<path d="M10 14a4 4 0 0 0 5.7 0l3-3a4 4 0 0 0-5.7-5.7l-1 1M14 10a4 4 0 0 0-5.7 0l-3 3a4 4 0 0 0 5.7 5.7l1-1"/>|}
    | Up -> {|<path d="M12 19V5M5 12l7-7 7 7"/>|}
    | Upload -> {|<path d="M12 15V3M7 8l5-5 5 5M4 15v4a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-4"/>|}
    | Download -> {|<path d="M12 3v12M7 10l5 5 5-5M4 15v4a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-4"/>|}
    | Attach ->
      {|<path d="m21 11-8.6 8.6a5 5 0 0 1-7-7l8.5-8.6a3.3 3.3 0 0 1 4.7 4.7l-8.6 8.6a1.7 1.7 0 0 1-2.4-2.4l8-8"/>|}
    | Folder_plus ->
      {|<path d="M3 7a2 2 0 0 1 2-2h4l2 2h8a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2zM12 10v6M9 13h6"/>|}
    | Eye ->
      {|<path d="M2 12s3.5-7 10-7 10 7 10 7-3.5 7-10 7S2 12 2 12z"/><circle cx="12" cy="12" r="3"/>|}
    | Refresh -> {|<path d="M21 12a9 9 0 1 1-2.6-6.4L21 8M21 3v5h-5"/>|}
    | At -> {|<circle cx="12" cy="12" r="4"/><path d="M16 8v5a3 3 0 0 0 6 0v-1a10 10 0 1 0-4 8"/>|}
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

let glyph = Glyph.view
let files action = Action.Files action
let size = Session_summary.size_string

let tool ~inject ?(cls = "") ~title ?disabled action icon =
  button
    ~cls:("icon ghost files-tool " ^ cls)
    ~title
    ?disabled
    ~on_click:(inject (files action))
    [ icon ]
;;

(* ---- the panel *)

(* [~ / proj / src]: each a link to its directory. *)
let crumbs (t : Files.t) (listing : File_listing.t) ~inject =
  let home = listing.home in
  let under_home =
    String.equal listing.path home
    || ((not (String.equal home "/")) && String.is_prefix listing.path ~prefix:(home ^ "/"))
  in
  let root, rest =
    if under_home
    then ("~", home), String.drop_prefix listing.path (String.length home)
    else ("/", "/"), listing.path
  in
  let parts = String.split rest ~on:'/' |> List.filter ~f:(Fn.non String.is_empty) in
  let _, links =
    List.fold
      parts
      ~init:(snd root, [])
      ~f:(fun (dir, acc) part ->
        let path = if String.equal dir "/" then "/" ^ part else dir ^ "/" ^ part in
        path, acc @ [ part, path ])
  in
  let crumb ~last (label, path) =
    if last
    then span ~cls:"files-crumb current" label
    else
      Node.button
        ~attrs:
          [ Attr.class_ "files-crumb"
          ; Attr.type_ "button"
          ; Attr.title (Files.display_path t path)
          ; Attr.on_click (fun _ -> inject (files (Go path)))
          ]
        [ Node.text label ]
  in
  let all = root :: links in
  let n = List.length all in
  List.concat_mapi all ~f:(fun i c ->
    let node = crumb ~last:(i = n - 1) c in
    if i = 0 || (i = 1 && String.equal (fst root) "/")
    then [ node ]
    else [ span ~cls:"files-sep" "/"; node ])
;;

let path_bar (t : Files.t) ~inject =
  match t.editing_path, t.listing with
  | Some text, _ ->
    Node.input
      ~attrs:
        [ Attr.id "files-path"
        ; Attr.class_ "files-path-input"
        ; Attr.value text
        ; Attr.placeholder "A path: /abs, ~/in/home or relative, Enter to go"
        ; Attr.create "autocomplete" "off"
        ; Attr.create "spellcheck" "false"
        ; Attr.create "aria-label" "Go to path"
        ; Attr.on_input (fun _ text -> inject (files (Edit_path (Some text))))
        ; Attr.on_keydown (fun ev ->
            match Js_of_ocaml.Js.Optdef.to_option ev##.key with
            | Some key when String.equal (Js_of_ocaml.Js.to_string key) "Enter" ->
              inject (files Submit_path)
            | Some key when String.equal (Js_of_ocaml.Js.to_string key) "Escape" ->
              Effect.Many
                [ Effect.Stop_propagation; inject (files (Edit_path None)) ]
            | _ -> Effect.Ignore)
        ]
      ()
  | None, Some listing ->
    div
      ~cls:"files-crumbs"
      ~attrs:[ Attr.create "aria-label" "Path" ]
      (crumbs t listing ~inject
       @ [ button
             ~cls:"icon ghost files-edit-path"
             ~title:"Type a path"
             ~on_click:
               (inject (files (Edit_path (Some (Files.display_path t listing.path)))))
             [ icon Pencil ]
         ])
  | None, None ->
    div
      ~cls:"files-crumbs"
      [ span
          ~cls:"files-crumb current"
          (match t.where with
           | Some w -> Files.display_path t w.cwd
           | None -> "")
      ]
;;

let sort_header (t : Files.t) ~inject =
  div
    ~cls:"files-columns"
    ~attrs:[ Attr.role "row" ]
    (List.map Files.Sort.Key.all ~f:(fun key ->
       let active = Files.Sort.Key.equal key t.sort.key in
       Node.button
         ~attrs:
           [ classes
               [ "files-sort"; "col-" ^ String.lowercase (Files.Sort.Key.label key) ]
               [ "active", active ]
           ; Attr.type_ "button"
           ; Attr.title (sprintf "Sort by %s" (String.lowercase (Files.Sort.Key.label key)))
           ; Attr.create
               "aria-sort"
               (if not active
                then "none"
                else if t.sort.descending
                then "descending"
                else "ascending")
           ; Attr.on_click (fun _ -> inject (files (Sort_by key)))
           ]
         [ Node.text (Files.Sort.Key.label key)
         ; (if active
            then span ~cls:"files-arrow" (if t.sort.descending then "↓" else "↑")
            else Node.none)
         ]))
;;

let kind_glyph (kind : File_listing.Kind.t) =
  match kind with
  | Dir -> glyph ~cls:"files-kind dir" Folder
  | Link `Dir -> glyph ~cls:"files-kind dir" Link
  | Link _ -> glyph ~cls:"files-kind" Link
  | File | Other -> glyph ~cls:"files-kind" File
;;

let mtime (m : App.Model.t) = function
  | None -> ""
  | Some time -> Message_time.short (App.Model.message_time m) time
;;

let row (m : App.Model.t) (t : Files.t) (e : File_listing.Entry.t) ~inject =
  let selected = Option.exists t.selected ~f:(String.equal e.name) in
  let dir = File_listing.Kind.is_dir e.kind in
  let uploading =
    List.find t.uploads ~f:(fun u ->
      Option.exists t.listing ~f:(fun l -> String.equal u.dir l.path)
      && String.equal u.local.name e.name)
  in
  Node.div
    ~attrs:
      [ classes
          [ "files-row" ]
          [ "selected", selected
          ; "dir", dir
          ; "dot", String.is_prefix e.name ~prefix:"."
          ; ( "missing"
            , match e.kind with
              | Link `Missing -> true
              | _ -> false )
          ]
      ; Attr.role "option"
      ; Attr.create "aria-selected" (Bool.to_string selected)
      ; Attr.create "data-name" e.name
      ; Attr.on_click (fun _ -> inject (files (Select e.name)))
      ]
    [ kind_glyph e.kind
    ; span ~cls:"files-name" (if dir then e.name ^ "/" else e.name)
    ; (match uploading with
       | Some _ -> span ~cls:"files-lamp" ""
       | None -> Node.none)
    ; span ~cls:"files-size" (Option.value_map e.size ~default:"" ~f:size)
    ; span
        ~cls:"files-mtime"
        ~attrs:
          (match e.mtime with
           | Some time -> [ Attr.title (Message_time.full (App.Model.message_time m) time) ]
           | None -> [])
        (mtime m e.mtime)
    ]
;;

let entry_path (t : Files.t) name =
  Option.value_map t.listing ~default:name ~f:(fun l ->
    if String.equal l.path "/" then "/" ^ name else l.path ^ "/" ^ name)
;;

(* What can be done with the selected entry. *)
let actions (t : Files.t) ~inject =
  match
    Option.bind t.selected ~f:(fun name ->
      Option.bind t.listing ~f:(fun l ->
        List.find l.entries ~f:(fun (e : File_listing.Entry.t) -> String.equal e.name name)))
  with
  | None -> Node.none
  | Some e ->
    let dir = File_listing.Kind.is_dir e.kind in
    let path = entry_path t e.name in
    let act ?(cls = "") ~title label icon action =
      button
        ~cls:("small ghost files-act " ^ cls)
        ~title
        ~on_click:(inject (files action))
        [ icon; Node.text label ]
    in
    div
      ~cls:"files-actions"
      ~attrs:[ Attr.create "aria-label" (sprintf "Actions for %s" e.name) ]
      ([ (if dir
          then act ~title:"Open (Enter)" "Open" (glyph Folder) (Select e.name)
          else
            act
              ~cls:"attach"
              ~title:"Attach to the next message"
              "Attach"
              (glyph Attach)
              (Attach e.name))
       ; act
           ~title:
             (if dir
              then "Download as a .tar.gz archive"
              else "Download to this computer")
           (if dir then "Download .tar.gz" else "Download")
           (glyph Download)
           (Download e.name)
       ; act
           ~title:"Insert @path into the message"
           "Mention"
           (glyph At)
           (Insert_path path)
       ; act ~title:"Copy the path" "Copy path" (icon Copy) (Copy_path path)
       ; act ~title:"Rename (F2)" "Rename" (icon Pencil) (Rename e.name)
       ; act ~cls:"danger" ~title:"Delete (Delete)" "Delete" (icon Trash) (Delete e.name)
       ])
;;

let text_preview (p : File_preview.t) ~text ~truncated =
  let lines = String.split_lines text in
  let count = Int.max 1 (List.length lines) in
  [ Node.div
      ~attrs:[ Attr.class_ "files-text" ]
      [ Node.pre
          ~attrs:[ Attr.class_ "files-gutter"; Attr.create "aria-hidden" "true" ]
          [ Node.text
              (String.concat ~sep:"\n" (List.init count ~f:(fun i -> Int.to_string (i + 1))))
          ]
      ; Node.pre ~attrs:[ Attr.class_ "files-code" ] [ Node.text text ]
      ]
  ; (if truncated
     then
       div
         ~cls:"files-note"
         [ Node.text
             (sprintf
                "Showing the first %s of %s: download it for the rest."
                (size (String.length text))
                (size p.size))
         ]
     else Node.none)
  ]
;;

let preview (m : App.Model.t) (t : Files.t) ~inject =
  let close =
    button
      ~cls:"icon ghost files-close-preview"
      ~title:"Close the preview (Esc)"
      ~on_click:(inject (files Close_preview))
      [ icon Close ]
  in
  let frame ?(meta = []) ~name body =
    Node.create
      "section"
      ~attrs:[ Attr.class_ "files-preview"; Attr.create "aria-label" ("Preview of " ^ name) ]
      [ div
          ~cls:"files-preview-head"
          ([ span ~cls:"files-preview-name" name ] @ meta @ [ close ])
      ; div ~cls:"files-preview-body" body
      ]
  in
  match t.preview with
  | None -> Node.none
  | Some (Loading path) ->
    frame
      ~name:(Filename.basename path)
      [ div ~cls:"files-wait" [ span ~cls:"files-lamp" ""; Node.text "Reading…" ] ]
  | Some (Failed { path; error }) ->
    frame
      ~name:(Filename.basename path)
      [ div
          ~cls:"files-error"
          ~attrs:[ Attr.role "alert" ]
          [ Node.text (sprintf "Cannot show it: %s" error) ]
      ]
  | Some (Shown p) ->
    let meta =
      [ span ~cls:"files-preview-meta" (size p.size)
      ; span ~cls:"files-preview-meta" (mtime m p.mtime)
      ]
    in
    frame
      ~meta
      ~name:p.name
      (match p.content with
       | Text { text; truncated } -> text_preview p ~text ~truncated
       | Image { url = Some url; mime } ->
         [ div
             ~cls:"files-image"
             [ Node.img
                 ~attrs:
                   [ Attr.src (Files.link t url)
                   ; Attr.alt (sprintf "%s (%s)" p.name mime)
                   ]
                 ()
             ]
         ]
       | Image { url = None; _ } ->
         [ div
             ~cls:"files-note"
             [ Node.text
                 (sprintf
                    "This image is %s, too large to show here: download it."
                    (size p.size))
             ]
         ]
       | Binary ->
         [ div
             ~cls:"files-note"
             [ Node.text
                 "A binary file: nothing to show here. Download it, or attach \
                  it for the agent to inspect with its tools."
             ]
         ])
;;

let uploads (t : Files.t) =
  match t.uploads, t.busy with
  | [], None -> Node.none
  | uploads, busy ->
    div
      ~cls:"files-transfers"
      ~attrs:[ Attr.role "status" ]
      ((match busy with
        | Some text -> [ div ~cls:"files-transfer" [ span ~cls:"files-lamp" ""; Node.text text ] ]
        | None -> [])
       @ List.map uploads ~f:(fun (u : Files.Upload.t) ->
         let percent =
           match u.sent with
           | Some sent when u.local.size > 0 -> sent * 100 / u.local.size
           | Some _ -> 100
           | None -> 0
         in
         div
           ~cls:"files-transfer"
           [ span ~cls:"files-lamp" ""
           ; span ~cls:"files-transfer-name" u.local.name
           ; span
               ~cls:"files-transfer-meta"
               (match u.sent with
                | None -> "waiting…"
                | Some sent -> sprintf "%s of %s" (size sent) (size u.local.size))
           ; Node.div
               ~attrs:
                 [ Attr.class_ "files-bar"
                 ; Attr.css_var ~name:"done" (sprintf "%d%%" percent)
                 ; Attr.role "progressbar"
                 ; Attr.create "aria-valuenow" (Int.to_string percent)
                 ]
               []
           ]))
;;

let body (m : App.Model.t) (t : Files.t) ~inject =
  let listing_body =
    match t.listing, t.error with
    | _, Some error ->
      [ div
          ~cls:"files-error"
          ~attrs:[ Attr.role "alert" ]
          [ Node.text error
          ; div
              ~cls:"files-advice"
              [ Node.text "Check the path (the pencil types one), go up, or try again." ]
          ; button ~cls:"small" ~on_click:(inject (files Refresh)) [ Node.text "Try again" ]
          ]
      ]
    | None, None ->
      [ div ~cls:"files-wait" [ span ~cls:"files-lamp" ""; Node.text "Listing…" ] ]
    | Some l, None ->
      let entries = Files.entries t in
      [ sort_header t ~inject
      ; Node.div
          ~attrs:
            [ Attr.class_ "files-list"
            ; Attr.id "files-list"
            ; Attr.role "listbox"
            ; Attr.tabindex 0
            ; Attr.create "aria-label" "Files"
            ]
          (match entries with
           | [] ->
             [ div
                 ~cls:"files-empty"
                 [ Node.text
                     (if l.hidden > 0
                      then
                        sprintf
                          "Only hidden files here (%d): the eye shows them. Drop \
                           files to upload them."
                          l.hidden
                      else "An empty directory: drop files here or upload them.")
                 ]
             ]
           | entries -> List.map entries ~f:(row m t ~inject))
      ; div
          ~cls:"files-count"
          [ Node.text
              (String.concat
                 ~sep:"  "
                 (List.filter_opt
                    [ Some
                        (sprintf
                           "%d %s"
                           l.total
                           (if l.total = 1 then "item" else "items"))
                    ; Option.some_if
                        (l.total > List.length l.entries)
                        (sprintf "first %d shown" (List.length l.entries))
                    ; Option.some_if (l.hidden > 0) (sprintf "%d hidden" l.hidden)
                    ; Option.some_if (Option.is_some t.loading) "listing…"
                    ]))
          ]
      ]
  in
  let previewing =
    match t.preview with
    | Some _ -> true
    | None -> false
  in
  div
    ~cls:(if previewing then "files-body previewing" else "files-body")
    [ div ~cls:"files-browse" (listing_body @ [ actions t ~inject ])
    ; preview m t ~inject
    ]
;;

let view (m : App.Model.t) ~inject =
  let t = m.files in
  if not t.open_
  then Node.none
  else (
    let host =
      match t.where with
      | Some w -> w.host_name
      | None -> "…"
    in
    Node.create
      "aside"
      ~attrs:
        [ Attr.class_ "files-panel"
        ; Attr.id "files-panel"
        ; Attr.create "aria-label" "Files"
        ; Attr.create
            "data-drop"
            (sprintf
               "Drop to upload to %s"
               (Option.value_map t.listing ~default:"here" ~f:(fun l ->
                  Files.display_path t l.path)))
        ]
      [ div
          ~cls:"files-head"
          [ glyph ~cls:"files-head-icon" Folder
          ; span ~cls:"files-machine" ~attrs:[ Attr.title "The tools' host" ] host
          ; div
              ~cls:"files-tools"
              [ tool
                  ~inject
                  ~title:"Up (Backspace)"
                  ~disabled:
                    (Option.value_map t.listing ~default:true ~f:(fun l ->
                       Option.is_none l.parent))
                  Up
                  (glyph Up)
              ; tool ~inject ~title:"Refresh" Refresh (glyph Refresh)
              ; tool
                  ~inject
                  ~cls:(if t.hidden then "on" else "")
                  ~title:(if t.hidden then "Hide dot files" else "Show dot files")
                  Toggle_hidden
                  (glyph Eye)
              ; tool ~inject ~title:"New folder" New_folder (glyph Folder_plus)
              ; tool ~inject ~title:"Upload files here" (Pick Here) (glyph Upload)
              ; button
                  ~cls:"icon ghost files-close"
                  ~title:"Close (Ctrl+Shift+F)"
                  ~on_click:(inject (files Close))
                  [ icon Close ]
              ]
          ]
      ; path_bar t ~inject
      ; body m t ~inject
      ; uploads t
      ])
;;

let dialog (m : App.Model.t) ~inject =
  let t = m.files in
  let close = inject (files Ask_cancel) in
  let input ~value ~placeholder =
    Node.input
      ~attrs:
        [ Attr.id "files-ask-input"
        ; Attr.class_ "text-input"
        ; Attr.value value
        ; Attr.placeholder placeholder
        ; Attr.create "autocomplete" "off"
        ; Attr.create "spellcheck" "false"
        ; Attr.on_input (fun _ text -> inject (files (Ask_input text)))
        ]
      ()
  in
  let error = function
    | None -> Node.none
    | Some e -> div ~cls:"dialog-error files-ask-error" ~attrs:[ Attr.role "alert" ] [ Node.text e ]
  in
  let buttons ?(danger = false) ~busy label =
    [ button ~on_click:close [ Node.text "Cancel" ]
    ; button
        ~cls:(if danger then "danger" else "primary")
        ~disabled:busy
        ~attrs:(if busy then [ Attr.create "aria-busy" "true" ] else [])
        ~on_click:(inject (files Ask_accept))
        [ Node.text label ]
    ]
  in
  match t.ask with
  | None -> Node.none
  | Some (New_folder { dir; input = value; error = e; busy }) ->
    Modal.view
      ~id:"files-dialog"
      ~cls:"files-ask"
      ~title:"New folder"
      ~on_close:close
      ~footer:(buttons ~busy (if busy then "Creating…" else "Create"))
      [ div ~cls:"files-ask-where" [ Node.text ("In " ^ Files.display_path t dir) ]
      ; input ~value ~placeholder:"Folder name"
      ; error e
      ]
  | Some (Rename { path; input = value; error = e; busy }) ->
    Modal.view
      ~id:"files-dialog"
      ~cls:"files-ask"
      ~title:(sprintf "Rename %s" (Filename.basename path))
      ~on_close:close
      ~footer:(buttons ~busy (if busy then "Renaming…" else "Rename"))
      [ input ~value ~placeholder:"New name"; error e ]
  | Some (Delete { path; dir; error = e; busy }) ->
    Modal.view
      ~id:"files-dialog"
      ~cls:"files-ask"
      ~title:(sprintf "Delete %s?" (Filename.basename path))
      ~on_close:close
      ~footer:(buttons ~danger:true ~busy (if busy then "Deleting…" else "Delete"))
      [ div
          ~cls:"files-ask-text"
          [ Node.text
              (if dir
               then
                 sprintf
                   "%s and everything in it will be deleted from %s. This cannot \
                    be undone."
                   (Files.display_path t path)
                   (Option.value_map t.where ~default:"the host" ~f:(fun w -> w.host_name))
               else
                 sprintf
                   "%s will be deleted from %s. This cannot be undone."
                   (Files.display_path t path)
                   (Option.value_map t.where ~default:"the host" ~f:(fun w -> w.host_name)))
          ]
      ; error e
      ]
  | Some (Overwrite u) ->
    Modal.view
      ~id:"files-dialog"
      ~cls:"files-ask"
      ~title:(sprintf "Replace %s?" u.local.name)
      ~on_close:close
      ~footer:
        [ button ~on_click:close [ Node.text "Skip" ]
        ; button ~cls:"danger" ~on_click:(inject (files Ask_accept)) [ Node.text "Replace" ]
        ]
      [ div
          ~cls:"files-ask-text"
          [ Node.text
              (sprintf
                 "%s already has a %s. Replace it with the one you are uploading (%s)?"
                 (Files.display_path t u.dir)
                 u.local.name
                 (size u.local.size))
          ]
      ]
;;

let toggle (m : App.Model.t) ~inject =
  button
    ~cls:(if m.files.open_ then "icon ghost files-toggle open" else "icon ghost files-toggle")
    ~title:"Files (Ctrl+Shift+F)"
    ~on_click:(inject (files Toggle))
    [ glyph Folder ]
;;

(* ---- the composer *)

let chips (m : App.Model.t) ~inject =
  match m.files.attachments with
  | [] -> Node.none
  | attachments ->
    div
      ~cls:"attach-chips"
      ~attrs:[ Attr.create "aria-label" "Attached files" ]
      (List.mapi attachments ~f:(fun i (a : Files.Attachment.t) ->
         let state, detail, percent =
           match a.status with
           | Uploading sent ->
             let percent =
               match sent, a.size with
               | Some sent, Some total when total > 0 -> sent * 100 / total
               | _ -> 0
             in
             "uploading", sprintf "%d%%" percent, Some percent
           | Ready _ -> "ready", Option.value_map a.size ~default:"" ~f:size, None
           | Failed e -> "failed", e, None
         in
         Node.div
           ~attrs:
             ([ Attr.class_ ("attach-chip " ^ state)
              ; Attr.title
                  (match a.status with
                   | Ready path -> path
                   | Uploading _ -> sprintf "Uploading %s…" a.name
                   | Failed e -> sprintf "%s: %s" a.name e)
              ]
              @
              match percent with
              | Some p -> [ Attr.css_var ~name:"done" (sprintf "%d%%" p) ]
              | None -> [])
           [ (match a.status with
              | Uploading _ -> span ~cls:"files-lamp" ""
              | Ready _ | Failed _ -> glyph ~cls:"attach-chip-icon" File)
           ; span ~cls:"attach-chip-name" a.name
           ; span ~cls:"attach-chip-detail" detail
           ; button
               ~cls:"icon ghost attach-chip-remove"
               ~title:(sprintf "Remove %s" a.name)
               ~on_click:(inject (files (Remove_attachment i)))
               [ icon Close ]
           ]))
;;

let attach_button (_ : App.Model.t) ~inject =
  button
    ~cls:"icon ghost attach"
    ~title:"Attach files (or drop them here)"
    ~on_click:(inject (files (Pick Chat)))
    [ glyph Attach ]
;;

let pickers =
  let picker id =
    Node.input
      ~attrs:
        [ Attr.id id
        ; Attr.class_ "hidden-file-input"
        ; Attr.type_ "file"
        ; Attr.create "multiple" ""
        ; Attr.tabindex (-1)
        ; Attr.create "aria-hidden" "true"
        ]
      ()
  in
  Node.div
    ~attrs:[ Attr.class_ "file-pickers" ]
    [ picker "files-upload-input"; picker "chat-attach-input" ]
;;
