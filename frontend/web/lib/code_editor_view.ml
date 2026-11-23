open! Core
open! Import
open Html
module Action = App.Action
module E = Code_editor

module Input = struct
  type t =
    { path : string
    ; lang : E.Lang.t
    ; text : string
    ; revision : int
    ; hunks : E.Hunk.t list
    ; kept : E.Hunk.t list
    ; ask : E.Chip.t option
    ; on_change : path:string -> string -> unit Effect.t
    ; on_select : E.Selection.t -> unit Effect.t
    ; on_scroll : [ `Down | `Up ] -> unit Effect.t
    ; on_hunk : int -> [ `Keep | `Undo | `Reopen ] -> unit Effect.t
    ; on_ask : [ `Send of string | `Cancel ] -> unit Effect.t
    ; on_swipe : int -> unit Effect.t
    }

  let sexp_of_t t =
    [%sexp
      { path : string = t.path
      ; lang : E.Lang.t = t.lang
      ; revision : int = t.revision
      ; lines : int = List.length (String.split_lines t.text)
      ; hunks : E.Hunk.t list = t.hunks
      ; kept : E.Hunk.t list = t.kept
      ; ask : E.Chip.t option = t.ask
      }]
  ;;
end

let code action = Action.Code action

let key (ev : Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t) =
  Js_of_ocaml.Js.Optdef.case ev##.key (fun () -> "") Js_of_ocaml.Js.to_string
;;

let dir_and_name path =
  match String.rsplit2 path ~on:'/' with
  | Some (dir, name) -> dir ^ "/", name
  | None -> "", path
;;

let path_view ?(cls = "") path =
  let dir, name = dir_and_name path in
  Node.span
    ~attrs:[ Attr.class_ (String.strip ("code-path " ^ cls)); Attr.title path ]
    [ Node.span
        ~attrs:[ Attr.class_ "code-path-dir" ]
        [ Node.create "bdi" [ Node.text dir ] ]
    ; span ~cls:"code-path-name" name
    ]
;;

let dirty_dot (b : E.Buffer.t) =
  if E.Buffer.dirty b
  then
    Node.span
      ~attrs:
        [ Attr.class_ "code-dirty"
        ; Attr.title "Unsaved changes (Ctrl+S saves)"
        ; Attr.role "img"
        ; Attr.create "aria-label" "Unsaved changes"
        ]
      []
  else Node.none
;;

(* loon's DropdownMenu (Menu_view's surface), its items buttons. *)
let menu (_ : E.t) (b : E.Buffer.t option) ~inject =
  let item ?(cls = "") ?(disabled = false) label hint action =
    Node.button
      ~attrs:
        ([ Attr.class_
             (String.strip
                ("code-menu-item menu-item w-full border-0 bg-transparent \
                  text-left narrow:min-h-11 "
                 ^ cls))
         ; Attr.create "data-slot" "dropdown-menu-item"
         ; Attr.type_ "button"
         ; Attr.role "menuitem"
         ; Attr.on_click (fun _ -> inject (code action))
         ]
         @ (if String.equal cls "danger"
            then [ Attr.create "data-variant" "destructive" ]
            else [])
         @ if disabled then [ Attr.disabled ] else [])
      [ span ~cls:"code-menu-label menu-label" label
      ; (if String.is_empty hint
         then Node.none
         else span ~cls:"code-menu-hint menu-command" hint)
      ]
  in
  let no_file = Option.is_none b in
  Menu_view.surface
    ~cls:"code-menu"
    ~label:"Editor"
    ~attrs:[ Attr.id "code-menu" ]
    [ item ~disabled:no_file "Save" "Ctrl+S" Save
    ; item ~disabled:no_file "Reload from disk" "" Reload
    ; item ~disabled:no_file "Go to line" "Ctrl+G" Goto_open
    ; item ~disabled:no_file "Find" "Ctrl+F" (Command Find)
    ; item ~disabled:no_file "Ask here, under the lines" "Ctrl+I" Ask_inline
    ; item ~disabled:no_file "Ask in the chat" "Ctrl+L" Ask
    ; item
        ~disabled:no_file
        ~cls:"danger"
        "Close file"
        "Alt+W"
        (Close (Option.value_map b ~default:"" ~f:(fun b -> b.path)))
    ; item "Leave the editor" "Esc" Leave
    ]
;;

let strip (m : App.Model.t) ~inject =
  let t = m.code in
  let b = E.current t in
  let dots =
    match t.buffers with
    | [] | [ _ ] -> Node.none
    | buffers ->
      div
        ~cls:"code-dots"
        ~attrs:[ Attr.create "aria-hidden" "true" ]
        (List.map buffers ~f:(fun (b' : E.Buffer.t) ->
           Node.span
             ~attrs:
               [ classes
                   [ "code-dot" ]
                   [ ( "on"
                     , Option.exists b ~f:(fun b -> String.equal b.path b'.path)
                     )
                   ]
               ]
             []))
  in
  Node.header
    ~attrs:[ classes [ "code-strip"; "loon" ] [ "hidden", t.strip_hidden && m.narrow ] ]
    [ button
        ~cls:"code-tree-toggle shrink-0 narrow:size-11"
        ~variant:Ghost
        ~size:Icon_sm
        ~title:(if m.narrow then "Files" else "Show or hide the files (Ctrl+B)")
        ~label:"Files"
        ~attrs:
          [ Attr.create
              "aria-expanded"
              (Bool.to_string (if m.narrow then t.drawer else t.tree))
          ]
        ~on_click:
          (inject (code (if m.narrow then Toggle_drawer else Toggle_tree)))
        [ icon Menu ]
    ; (match b with
       | None -> span ~cls:"code-path code-path-none" "No file open"
       | Some b -> path_view (E.relative t b.path))
    ; Option.value_map b ~default:Node.none ~f:dirty_dot
    ; (if m.narrow then dots else Node.none)
    ; div
        ~cls:"code-strip-end"
        [ (match b with
           | Some b when E.Buffer.dirty b && not m.narrow ->
             button
               ~cls:"code-save mr-1.5"
               ~variant:Default
               ~size:Sm
               ~title:"Save (Ctrl+S)"
               ~disabled:b.saving
               ~on_click:(inject (code Save))
               [ Node.text (if b.saving then "Saving…" else "Save") ]
           | Some _ | None -> Node.none)
        ; (if m.narrow
           then Node.none
           else
             button
               ~cls:"code-chat-toggle shrink-0 narrow:size-11"
        ~variant:Ghost
        ~size:Icon_sm
               ~title:"Show or hide the chat (Alt+C)"
               ~label:"Chat"
               ~attrs:[ Attr.create "aria-pressed" (Bool.to_string t.chat) ]
               ~on_click:(inject (code Toggle_chat))
               [ icon Message ])
        ; Node.div
            ~attrs:[ Attr.class_ "code-menu-anchor" ]
            [ button
                ~cls:"code-more shrink-0 narrow:size-11"
        ~variant:Ghost
        ~size:Icon_sm
                ~title:"More"
                ~label:"More"
                ~attrs:
                  [ Attr.create "aria-haspopup" "menu"
                  ; Attr.create "aria-expanded" (Bool.to_string t.menu)
                  ]
                ~on_click:(inject (code Toggle_menu))
                [ icon More ]
            ; (if t.menu
               then
                 Node.div
                   ~attrs:
                     [ Attr.class_ "code-menu-backdrop"
                     ; Attr.on_click (fun _ -> inject (code Toggle_menu))
                     ]
                   []
               else Node.none)
            ; (if t.menu then menu t b ~inject else Node.none)
            ]
        ; (if m.narrow
           then Node.none
           else
             button
               ~cls:"code-leave shrink-0 narrow:size-11"
        ~variant:Ghost
        ~size:Icon_sm
               ~title:"Back to the chat (Esc)"
               ~label:"Leave the editor"
               ~on_click:(inject (code Leave))
               [ icon Close ])
        ]
    ]
;;

let tabs (m : App.Model.t) ~inject =
  let t = m.code in
  match t.buffers with
  | [] -> Node.none
  | buffers ->
    div
      ~cls:"code-tabs loon"
      ~attrs:[ Attr.role "tablist"; Attr.create "aria-label" "Open files" ]
      (List.map buffers ~f:(fun (b : E.Buffer.t) ->
         let on = Option.equal String.equal t.current (Some b.path) in
         let _, name = dir_and_name b.path in
         Node.div
           ~attrs:[ classes [ "code-tab" ] [ "on", on ] ]
           [ Node.button
               ~attrs:
                 [ Attr.class_ "code-tab-open"
                 ; Attr.type_ "button"
                 ; Attr.role "tab"
                 ; Attr.create "aria-selected" (Bool.to_string on)
                 ; Attr.title (E.relative t b.path)
                 ; Attr.on_click (fun _ -> inject (code (Switch b.path)))
                 ]
               [ Node.text name; dirty_dot b ]
           ; button
               ~cls:"code-tab-close mr-1"
               ~variant:Ghost
               ~size:Icon_xs
               ~title:(sprintf "Close %s (Alt+W)" name)
               ~label:(sprintf "Close %s" name)
               ~on_click:(inject (code (Close b.path)))
               [ icon Close ]
           ]))
;;

let mark (m : App.Model.t) abs =
  match Code_review.mark m.review ~abs with
  | `File _ ->
    Node.span
      ~attrs:
        [ Attr.class_ "code-git"
        ; Attr.title "Changed"
        ; Attr.create "aria-label" "changed"
        ]
      [ Node.text "M" ]
  | `Dir n ->
    Node.span
      ~attrs:
        [ Attr.class_ "code-git dir"
        ; Attr.title (sprintf "%d changed inside" n)
        ]
      [ Node.text (Int.to_string n) ]
  | `None -> Node.none
;;

let row ?(cls = "") ~title ~on_click children =
  Node.button
    ~attrs:
      [ Attr.class_ (String.strip ("code-row " ^ cls))
      ; Attr.type_ "button"
      ; Attr.title title
      ; Attr.on_click (fun _ -> on_click)
      ]
    children
;;

(* [path] with the letters the query matched marked. *)
let match_view path (positions : int array) =
  let marked = Int.Hash_set.of_list (Array.to_list positions) in
  let pieces ~pos ~len =
    let out = ref [] in
    let run = Buffer.create 16 in
    let in_mark = ref false in
    let flush () =
      if Buffer.length run > 0
      then (
        let text = Buffer.contents run in
        out
        := (if !in_mark
            then Node.create "mark" [ Node.text text ]
            else Node.text text)
           :: !out;
        Buffer.clear run)
    in
    for i = pos to pos + len - 1 do
      let m = Hash_set.mem marked i in
      if Bool.( <> ) m !in_mark
      then (
        flush ();
        in_mark := m);
      Buffer.add_char run path.[i]
    done;
    flush ();
    List.rev !out
  in
  let base =
    match String.rindex path '/' with
    | Some i -> i + 1
    | None -> 0
  in
  Node.span
    ~attrs:[ Attr.class_ "code-path"; Attr.title path ]
    [ Node.span
        ~attrs:[ Attr.class_ "code-path-dir" ]
        [ Node.create "bdi" (pieces ~pos:0 ~len:base) ]
    ; Node.span
        ~attrs:[ Attr.class_ "code-path-name" ]
        (pieces ~pos:base ~len:(String.length path - base))
    ]
;;

(* The matches are a list as tall as all of them, with only the rows near
   the scroll position in it. *)
let row_height (m : App.Model.t) = if m.narrow then 44 else 30
let rows_around = 40

let matches_view (m : App.Model.t) ~inject =
  let t = m.code in
  let h = row_height m in
  let n = Array.length t.matches in
  let first = Int.max 0 (t.list_top - (rows_around / 2)) in
  let last = Int.min n (first + rows_around + (rows_around / 2)) in
  let rows =
    List.range first last
    |> List.map ~f:(fun i ->
      let (mt : Prigh_ui.Fuzzy_path.Match.t) = t.matches.(i) in
      Node.button
        ~key:mt.path
        ~attrs:
          [ Attr.class_ "code-row file"
          ; Attr.type_ "button"
          ; Attr.title mt.path
          ; Attr.style
              (Css_gen.create ~field:"top" ~value:(sprintf "%dpx" (i * h)))
          ; Attr.on_click (fun _ -> inject (code (Open (E.absolute t mt.path))))
          ]
        [ match_view mt.path mt.positions ])
  in
  Node.div
    ~attrs:
      [ Attr.class_ "code-matches"
      ; Attr.style
          (Css_gen.create ~field:"height" ~value:(sprintf "%dpx" (n * h)))
      ]
    rows
;;

let found_label (t : E.t) =
  let n = Array.length t.matches in
  let of_ = Array.length t.index.files in
  sprintf
    "%s of %s%s"
    (if n = 1 then "1 file" else Int.to_string_hum ~delimiter:',' n ^ " files")
    (Int.to_string_hum ~delimiter:',' of_)
    (if t.index.truncated then "+ (the host stopped listing)" else "")
;;

let tree (m : App.Model.t) ~inject =
  let t = m.code in
  let filtering = not (String.is_empty (String.strip t.filter)) in
  let filter =
    div
      ~cls:"code-goto-file"
      [ icon Search
      ; Node.input
          ~attrs:
            [ Attr.id "code-filter"
            ; Attr.type_ "search"
            ; Attr.placeholder "Go to file"
            ; Attr.create "aria-label" "Go to file"
            ; Attr.create "autocomplete" "off"
            ; Attr.create "autocapitalize" "off"
            ; Attr.create "spellcheck" "false"
            ; Attr.create "enterkeyhint" "go"
            ; Attr.value t.filter
            ; Attr.on_input (fun _ s -> inject (code (Filter s)))
            ; Attr.on_keydown (fun ev ->
                match key ev with
                | "Enter" ->
                  if Array.is_empty t.matches
                  then Effect.Ignore
                  else inject (code (Open (E.absolute t t.matches.(0).path)))
                | _ -> Effect.Ignore)
            ]
          ()
      ]
  in
  let section title children =
    if List.is_empty children
    then Node.none
    else
      Node.div
        ~attrs:[ Attr.class_ "code-section" ]
        (Node.h3 ~attrs:[ Attr.class_ "code-section-title" ] [ Node.text title ]
         :: children)
  in
  let open_files =
    section
      "Open"
      (List.map t.buffers ~f:(fun (b : E.Buffer.t) ->
         let on = Option.equal String.equal t.current (Some b.path) in
         row
           ~cls:(if on then "on" else "")
           ~title:(E.relative t b.path)
           ~on_click:(inject (code (Switch b.path)))
           [ path_view (E.relative t b.path); dirty_dot b ]))
  in
  let listing =
    match t.dir with
    | None -> [ span ~cls:"code-empty" "Listing…" ]
    | Some l ->
      let up =
        match l.parent with
        | None -> Node.none
        | Some parent ->
          row
            ~cls:"dir up"
            ~title:parent
            ~on_click:(inject (code (List (Some parent))))
            [ icon Back; span ~cls:"code-row-name" ".." ]
      in
      up
      :: List.map l.entries ~f:(fun e ->
        let abs = Filename.concat l.path e.name in
        if File_listing.Kind.is_dir e.kind
        then
          row
            ~cls:"dir"
            ~title:abs
            ~on_click:(inject (code (List (Some abs))))
            [ icon Folder
            ; span ~cls:"code-row-name" (e.name ^ "/")
            ; mark m abs
            ]
        else
          row
            ~cls:
              (if Option.equal String.equal t.current (Some abs)
               then "file on"
               else "file")
            ~title:abs
            ~on_click:(inject (code (Open abs)))
            [ span ~cls:"code-row-name" e.name; mark m abs ])
  in
  let body =
    if filtering
    then
      [ Node.div
          ~attrs:[ Attr.class_ "code-section" ]
          [ Node.h3
              ~attrs:[ Attr.class_ "code-section-title" ]
              [ Node.text
                  (if not t.index.loaded
                   then "Listing the files…"
                   else found_label t)
              ]
          ; (if t.index.loaded && Array.is_empty t.matches
             then span ~cls:"code-empty" "No file matches: try fewer letters"
             else matches_view m ~inject)
          ]
      ]
    else
      [ open_files
      ; section
          (match t.dir with
           | Some l when String.equal l.path t.cwd ->
             Filename.basename l.path ^ "/"
           | Some l -> E.relative t l.path ^ "/"
           | None -> "Files")
          listing
      ]
  in
  Node.nav
    ~attrs:
      [ classes [ "code-tree" ] [ "drawer", m.narrow ]
      ; Attr.create "aria-label" "Files"
      ]
    (let scroll =
       Node.div
         ~attrs:
           [ Attr.class_ "code-tree-scroll"
           ; Attr.on_scroll (fun ev ->
               if not filtering
               then Effect.Ignore
               else
                 Js_of_ocaml.Js.Opt.case
                   ev##.target
                   (fun () -> Effect.Ignore)
                   (fun el ->
                      (* The rows' top: the section title above them. *)
                      let top = Js_of_ocaml.Js.to_float el##.scrollTop in
                      let row =
                        Float.iround_down_exn
                          (top /. Float.of_int (row_height m))
                      in
                      if abs (row - t.list_top) < rows_around / 4
                      then Effect.Ignore
                      else inject (code (List_scrolled row))))
           ]
         body
     in
     if m.narrow then [ scroll; filter ] else [ filter; scroll ])
;;

let notices (t : E.t) (b : E.Buffer.t) ~inject =
  let bar ~cls text buttons =
    div
      ~cls:("code-notice " ^ cls)
      ~attrs:[ Attr.role "status" ]
      (span ~cls:"code-notice-text" text :: buttons)
  in
  let act ?(cls = "small") label action =
    button ~cls ~on_click:(inject (code action)) [ Node.text label ]
  in
  [ (match b.conflict with
     | Some Changed_on_disk ->
       bar
         ~cls:"conflict"
         (sprintf
            "%s changed on the host since you opened it: reload it (losing \
             your edits) or keep yours and overwrite it."
            (E.relative t b.path))
         [ act "Reload" Reload; act ~cls:"small danger" "Overwrite" Overwrite ]
     | Some (Save_refused e) ->
       bar ~cls:"conflict" (sprintf "Not saved: %s" e) [ act "Try again" Save ]
     | None -> Node.none)
  ; (match b.hunks with
     | [] -> Node.none
     | hunks ->
       let added =
         List.sum (module Int) hunks ~f:(fun h -> List.length h.added)
       in
       let removed =
         List.sum (module Int) hunks ~f:(fun h -> List.length h.removed)
       in
       let n = List.length hunks in
       bar
         ~cls:"agent"
         (sprintf
            "The agent changed this file: %s, +%d −%d"
            (if n = 1 then "1 change" else sprintf "%d changes" n)
            added
            removed)
         (if n = 1
          then [ act ~cls:"small primary" "Keep" (Keep 0); act "Undo" (Undo 0) ]
          else
            [ act ~cls:"small primary" "Keep all" Keep_all
            ; act "Undo all" Undo_all
            ]))
  ]
;;

let goto (t : E.t) ~inject =
  match t.goto with
  | None -> Node.none
  | Some s ->
    Node.form
      ~attrs:
        [ Attr.class_ "code-goto"
        ; Attr.on_submit (fun ev ->
            Js_of_ocaml.Dom.preventDefault ev;
            inject (code Goto_line))
        ]
      [ Node.label ~attrs:[ Attr.for_ "code-goto" ] [ Node.text "Go to line" ]
      ; Node.input
          ~attrs:
            [ Attr.id "code-goto"
            ; Attr.type_ "text"
            ; Attr.create "inputmode" "numeric"
            ; Attr.create "enterkeyhint" "go"
            ; Attr.value s
            ; Attr.on_input (fun _ s -> inject (code (Goto_input s)))
            ; Attr.on_keydown (fun ev ->
                match key ev with
                | "Escape" -> inject (code Goto_cancel)
                | _ -> Effect.Ignore)
            ]
          ()
      ; button
          ~cls:"small"
          ~on_click:(inject (code Goto_line))
          [ Node.text "Go" ]
      ]
;;

(* What the agent is doing, or said last: a phone's way up to the chat. *)
let peek (m : App.Model.t) ~inject =
  let latest =
    List.find_map (Chat.rev_entries m.chat) ~f:(function
      | Chat.Entry.Assistant { message; _ } ->
        List.find_map (List.rev message.content) ~f:(function
          | Content.Tool_call call ->
            Some (Prigh_ui.Tool_phrase.doing (Call call))
          | Text s when not (String.is_empty (String.strip s)) ->
            Some (String.strip s)
          | Text _ | Thinking _ | Server_tool _ -> None)
      | User _ | Shell _ | Compaction _ -> None)
  in
  let running = App.Model.running m in
  match latest, running with
  | None, false -> Node.none
  | _ ->
    Node.button
      ~attrs:
        [ classes [ "code-peek" ] [ "running", running ]
        ; Attr.type_ "button"
        ; Attr.title "Show the chat"
        ; Attr.create "aria-expanded" (Bool.to_string m.code.sheet)
        ; Attr.on_click (fun _ -> inject (code Toggle_sheet))
        ]
      [ (if running then lamp () else icon Message)
      ; span
          ~cls:"code-peek-text"
          (match latest with
           | Some s -> List.hd (String.split_lines s) |> Option.value ~default:s
           | None -> "Working…")
      ; icon Chevron
      ]
;;

let keys (m : App.Model.t) ~inject =
  let key ?(cls = "") ?label text command =
    Node.button
      ~attrs:
        [ Attr.class_ (String.strip ("code-key " ^ cls))
        ; Attr.type_ "button"
        ; Attr.create "aria-label" (Option.value label ~default:text)
        ; (* Keep the editor's focus, and the keyboard up. *)
          Attr.on_mousedown (fun ev ->
            Js_of_ocaml.Dom.preventDefault ev;
            Effect.Ignore)
        ; Attr.on_click (fun _ -> inject (code (Command command)))
        ]
      [ Node.text text ]
  in
  if not m.narrow
  then Node.none
  else
    div
      ~cls:"code-keys"
      ~attrs:[ Attr.create "aria-label" "Keys" ]
      [ Node.button
          ~attrs:
            [ Attr.class_ "code-key code-ask"
            ; Attr.type_ "button"
            ; Attr.title "Ask about the selection"
            ; (* The box opens under the lines, the keyboard stays up. *)
              Attr.on_mousedown (fun ev ->
                Js_of_ocaml.Dom.preventDefault ev;
                Effect.Ignore)
            ; Attr.on_click (fun _ -> inject (code Ask_inline))
            ]
          [ icon Message; Node.text "Ask" ]
      ; div
          ~cls:"code-keys-scroll"
          [ key ~label:"Indent" "⇥" Indent
          ; key "{" (Insert "{")
          ; key "}" (Insert "}")
          ; key "(" (Insert "(")
          ; key ")" (Insert ")")
          ; key "[" (Insert "[")
          ; key "]" (Insert "]")
          ; key ";" (Insert ";")
          ; key "=" (Insert "=")
          ; key "|" (Insert "|")
          ; key "->" (Insert "->")
          ; key ~label:"Left" "←" Left
          ; key ~label:"Right" "→" Right
          ; key ~cls:"undo" ~label:"Undo" "↶" Undo
          ]
      ]
;;

let editor (m : App.Model.t) ~inject ~widget =
  let t = m.code in
  match E.current t with
  | None ->
    div
      ~cls:"code-none"
      [ Node.p
          [ Node.text
              (match t.loading with
               | Some p -> sprintf "Opening %s…" (E.relative t p)
               | None ->
                 if m.narrow
                 then "Open a file: ☰ lists them, or Go to file."
                 else "Open a file from the list, or Ctrl+P to go to one.")
          ]
      ; (if m.narrow
         then
           button
             ~cls:"primary"
             ~on_click:(inject (code Toggle_drawer))
             [ Node.text "Files" ]
         else Node.none)
      ]
  | Some b ->
    div
      ~cls:"code-file"
      ~attrs:
        (* The drawer is over it: out of reach, as it is out of sight. *)
        (if m.narrow && t.drawer then [ Attr.create "inert" "" ] else [])
      (notices t b ~inject
       @ [ goto t ~inject
         ; widget
             { Input.path = b.path
             ; lang = E.Lang.of_path b.path
             ; text = b.text
             ; revision = b.revision
             ; hunks = b.hunks
             ; kept = b.kept
             ; ask = t.asking
             ; on_change =
                 (fun ~path text -> inject (code (Edited { path; text })))
             ; on_select = (fun s -> inject (code (Selected (Some s))))
             ; on_scroll = (fun d -> inject (code (Scrolled d)))
             ; on_hunk =
                 (fun i -> function
                    | `Keep -> inject (code (Keep i))
                    | `Undo -> inject (code (Undo i))
                    | `Reopen -> inject (code (Reopen i)))
             ; on_ask =
                 (function
                   | `Send text -> inject (code (Ask_send text))
                   | `Cancel -> inject (code Ask_cancel))
             ; on_swipe = (fun n -> inject (code (Swiped n)))
             }
         ])
;;

let view (m : App.Model.t) ~inject ~widget =
  let t = m.code in
  Node.section
    ~attrs:
      [ classes
          [ "code-page" ]
          [ ("tree-shown", if m.narrow then t.drawer else t.tree)
          ; "menu-open", t.menu
          ]
      ; Attr.id "code-page"
      ; Attr.create "aria-label" "Editor"
      ]
    [ strip m ~inject
    ; (if m.narrow then Node.none else tabs m ~inject)
    ; div
        ~cls:"code-body"
        [ (if m.narrow && not t.drawer
           then Node.none
           else if (not m.narrow) && not t.tree
           then Node.none
           else tree m ~inject)
        ; (if m.narrow && t.drawer
           then
             Node.div
               ~attrs:
                 [ Attr.class_ "code-scrim"
                 ; Attr.on_click (fun _ -> inject (code Toggle_drawer))
                 ]
               []
           else Node.none)
        ; (if (not m.narrow) && t.tree
           then Resize_view.handle m ~inject Editor_tree
           else Node.none)
        ; editor m ~inject ~widget
        ]
    ; (if m.narrow then peek m ~inject else Node.none)
    ; keys m ~inject
    ]
;;

let chip (m : App.Model.t) ~inject =
  match m.code.chip with
  | None -> Node.none
  | Some c ->
    div
      ~cls:"attach-chips code-chips"
      ~attrs:[ Attr.create "aria-label" "Code to ask about" ]
      [ Node.div
          ~attrs:
            [ Attr.class_ "attach-chip ready code-chip"
            ; Attr.title (E.Chip.label c)
            ]
          [ span ~cls:"attach-chip-name" (E.Chip.label c)
          ; span
              ~cls:"attach-chip-detail"
              (if String.is_empty c.text
               then "line"
               else sprintf "%d lines" (List.length (String.split_lines c.text)))
          ; button
              ~cls:"icon ghost attach-chip-remove"
              ~title:"Remove"
              ~label:(sprintf "Remove %s" (E.Chip.label c))
              ~on_click:(inject (code Drop_chip))
              [ icon Close ]
          ]
      ]
;;
