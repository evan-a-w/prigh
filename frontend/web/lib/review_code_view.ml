open! Core
open! Import
open Html
module Diff_files = Prigh_ui.Diff_files
module Comment_anchor = Prigh_ui.Comment_anchor
module Row = Diff_view.Row

let review action = App.Action.Review action

(* ---- rows *)

(* The whole file with the hunks' changes inline: the lines between hunks
   are the file's own. *)
let full_rows (hunk_rows : Row.t list) (lines : string array) =
  let out = ref [] in
  let old_n = ref 1 in
  let new_n = ref 1 in
  let fill_to last =
    while !new_n <= last && !new_n <= Array.length lines do
      out
      := Row.Line
           { kind = `Same
           ; old_line = Some !old_n
           ; new_line = Some !new_n
           ; text = lines.(!new_n - 1)
           }
         :: !out;
      incr old_n;
      incr new_n
    done
  in
  List.iter hunk_rows ~f:(fun (row : Row.t) ->
    match row with
    | Hunk header ->
      (match Diff_view.hunk_starts header with
       | Some (o, n) ->
         fill_to (n - 1);
         old_n := Int.max o 1;
         new_n := Int.max n 1
       | None -> ())
    | Line { kind; _ } ->
      out := row :: !out;
      (match kind with
       | `Same ->
         incr old_n;
         incr new_n
       | `Added -> incr new_n
       | `Removed -> incr old_n));
  fill_to (Array.length lines);
  List.rev !out
;;

let plain_rows lines =
  Array.to_list
    (Array.mapi lines ~f:(fun i text ->
       Row.Line
         { kind = `Same
         ; old_line = Some (i + 1)
         ; new_line = Some (i + 1)
         ; text
         }))
;;

module Rows = struct
  (* Rows and their tokens are made once per file version and layout, so
     that a key press or a streamed delta does not highlight a file again,
     and so that the rows (the cache's keys) stay the same values. *)
  type key =
    { path : string
    ; hash : string option
    ; text : string option
    ; full : bool
    }

  type t =
    { rows : Row.t list
    ; tokens : Highlight.Token.t list option list
    }

  let memo : (string, key * t) Hashtbl.t = Hashtbl.create (module String)

  let same a b =
    String.equal a.path b.path
    && Option.equal String.equal a.hash b.hash
    && Option.equal phys_equal a.text b.text
    && Bool.equal a.full b.full
  ;;

  let find key ~make =
    match Hashtbl.find memo key.path with
    | Some (k, t) when same k key -> t
    | Some _ | None ->
      let t = make () in
      Hashtbl.set memo ~key:key.path ~data:(key, t);
      t
  ;;
end

let text_of (t : Code_review.t) path =
  match Map.find t.contents path with
  | Some (Loaded { text; _ }) -> Some text
  | Some (Loading | Failed _) | None -> None
;;

let rows (t : Code_review.t) ~path ~(file : Diff_files.File.t option) =
  let text = text_of t path in
  let full = Code_review.Layout.equal t.layout Full in
  Rows.find
    { path
    ; hash = Option.map file ~f:(fun f -> f.hash)
    ; text = (if full || Option.is_none file then text else None)
    ; full
    }
    ~make:(fun () ->
      let lines () =
        Array.of_list (String.split_lines (Option.value text ~default:""))
      in
      let rows =
        match file, text with
        | None, _ -> plain_rows (lines ())
        | Some f, _ ->
          let hunk_rows =
            Diff_view.unified_rows (String.concat ~sep:"\n" f.hunks)
          in
          if full && Option.is_some text
          then full_rows hunk_rows (lines ())
          else hunk_rows
      in
      let tokens =
        match Highlight.Lang.of_path path with
        | Some lang when List.length rows <= 20_000 ->
          Diff_view.highlighted lang rows
        | Some _ | None -> List.map rows ~f:(fun _ -> None)
      in
      { Rows.rows; tokens })
;;

(* ---- a row *)

module Deps = struct
  type t =
    { selected : bool
    ; first : bool (** of the selection *)
    ; commented : bool
    }
  [@@deriving equal]
end

let row_cache : (Row.t, Deps.t) View_cache.t = View_cache.create ()

let row_view ~path (row : Row.t) tokens (deps : Deps.t) ~inject =
  let num = Option.value_map ~default:"" ~f:Int.to_string in
  match row with
  | Hunk text ->
    Node.tr
      ~attrs:[ Attr.class_ "hunk" ]
      [ Node.td ~attrs:[ Attr.create "colspan" "4" ] [ Node.text text ] ]
  | Line { kind; old_line; new_line; text } ->
    let cls, sign =
      match kind with
      | `Same -> "same", " "
      | `Added -> "add", "+"
      | `Removed -> "del", "-"
    in
    let line_cell =
      match new_line with
      | None -> Node.td ~attrs:[ Attr.class_ "ln" ] []
      | Some line ->
        Node.td
          ~attrs:
            [ Attr.class_ "ln pick"
            ; Attr.title
                (sprintf
                   "Select line %d to comment on (Shift+click: a range)"
                   line)
            ; Attr.on_click (fun ev ->
                inject
                  (review
                     (Select_line
                        { path
                        ; line
                        ; extend = Js_of_ocaml.Js.to_bool ev##.shiftKey
                        })))
            ]
          [ Node.text (Int.to_string line) ]
    in
    Node.tr
      ~attrs:
        [ classes
            [ cls ]
            [ "selected", deps.selected
            ; "sel-first", deps.first
            ; "commented", deps.commented
            ]
        ; (match new_line with
           | Some n -> Attr.create "data-line" (Int.to_string n)
           | None -> Attr.empty)
        ]
      [ Node.td ~attrs:[ Attr.class_ "ln" ] [ Node.text (num old_line) ]
      ; line_cell
      ; Node.td ~attrs:[ Attr.class_ "sign" ] [ Node.text sign ]
      ; (match tokens with
         | Some tokens ->
           Node.td ~attrs:[ Attr.class_ "text" ] (Highlight_view.spans tokens)
         | None -> Node.td ~attrs:[ Attr.class_ "text" ] [ Node.text text ])
      ]
;;

(* ---- threads *)

let ctrl_enter ~inject action =
  Attr.on_keydown (fun ev ->
    let key =
      Js_of_ocaml.Js.Optdef.case
        ev##.key
        (fun () -> "")
        Js_of_ocaml.Js.to_string
    in
    let command =
      Js_of_ocaml.Js.to_bool ev##.ctrlKey || Js_of_ocaml.Js.to_bool ev##.metaKey
    in
    match key with
    | "Enter" when command ->
      ev##preventDefault;
      inject action
    | _ -> Effect.Ignore)
;;

let lines_label first last =
  if first = last
  then sprintf "line %d" first
  else sprintf "lines %d–%d" first last
;;

let post_view
      (m : App.Model.t)
      (c : Review_comment.t)
      i
      (p : Review_comment.Post.t)
      ~inject
  =
  let busy = Set.mem m.review.busy c.id in
  let who, cls =
    match p.author with
    | User -> "You", "user"
    | Agent -> "prigh", "agent"
  in
  let proposal =
    match p.proposal with
    | None -> Node.none
    | Some { edits; state } ->
      let edits = List.map edits ~f:(fun e -> e.old_text, e.new_text) in
      let fold = Fold.scoped (App.Model.fold m) "review" in
      div
        ~cls:"review-proposal"
        [ Diff_view.edits
            ?lang:(Highlight.Lang.of_path c.path)
            ~fold:(fold, sprintf "%s-%d" c.id i)
            edits
        ; (match state with
           | Pending ->
             div
               ~cls:"review-proposal-actions"
               [ button
                   ~cls:"small primary"
                   ~disabled:busy
                   ~title:"Apply this change to the file"
                   ~on_click:
                     (inject
                        (review (Decide { id = c.id; post = i; accept = true })))
                   [ Node.text "Accept" ]
               ; button
                   ~cls:"small ghost"
                   ~disabled:busy
                   ~title:"Leave the file as it is; prigh is told"
                   ~on_click:
                     (inject
                        (review
                           (Decide { id = c.id; post = i; accept = false })))
                   [ Node.text "Reject" ]
               ]
           | Accepted ->
             div
               ~cls:"review-proposal-state done"
               [ Node.text "Accepted: applied to the file" ]
           | Rejected ->
             div ~cls:"review-proposal-state" [ Node.text "Rejected" ]
           | Failed e ->
             div
               ~cls:"review-proposal-state failed"
               [ Node.text ("Could not apply it: " ^ e) ])
        ]
  in
  Node.create
    "li"
    ~attrs:[ Attr.class_ ("review-post " ^ cls) ]
    [ div
        ~cls:"review-post-head"
        [ span ~cls:"review-who" who
        ; (match p.at with
           | Some at ->
             span
               ~cls:"review-time"
               (Message_time.short (App.Model.message_time m) at)
           | None -> Node.none)
        ; (if Review_comment.Author.equal p.author User && not p.delivered
           then span ~cls:"review-unsent" "not sent yet"
           else Node.none)
        ]
    ; div ~cls:"review-post-text" [ Markdown_view.render p.text ]
    ; proposal
    ]
;;

let thread ?at (m : App.Model.t) (c : Review_comment.t) ~outdated ~inject =
  let first, last = Option.value at ~default:(c.first, c.last) in
  let busy = Set.mem m.review.busy c.id in
  let reply = Option.value (Map.find m.review.replies c.id) ~default:"" in
  let resolved = Review_comment.Status.equal c.status Resolved in
  let field_id = "review-reply-" ^ c.id in
  Node.create
    "article"
    ~attrs:
      [ classes
          [ "review-thread" ]
          [ "resolved", resolved; "outdated", outdated ]
      ; Attr.id ("review-thread-" ^ c.id)
      ; Attr.create
          "aria-label"
          (sprintf "Comment %s on %s" c.id (lines_label first last))
      ]
    [ div
        ~cls:"review-thread-head"
        [ span ~cls:"review-id" c.id
        ; span ~cls:"review-where" (lines_label first last)
        ; (if outdated
           then span ~cls:"review-flag" "its lines changed"
           else Node.none)
        ; (if resolved
           then span ~cls:"review-flag done" "resolved"
           else Node.none)
        ; (if busy then lamp ~title:"Saving" () else Node.none)
        ; div
            ~cls:"review-thread-tools"
            [ button
                ~cls:"small ghost"
                ~disabled:busy
                ~title:
                  (if resolved then "Open the thread again" else "Mark it done")
                ~on_click:
                  (inject
                     (review
                        (Set_status
                           { id = c.id
                           ; status = (if resolved then Open else Resolved)
                           })))
                [ Node.text (if resolved then "Reopen" else "Resolve") ]
            ; button
                ~cls:"icon ghost"
                ~disabled:busy
                ~title:"Delete the comment and its thread"
                ~label:"Delete"
                ~on_click:(inject (review (Delete c.id)))
                [ icon Trash ]
            ]
        ]
    ; (if resolved && String.is_empty reply
       then Node.none
       else
         Node.ul
           ~attrs:[ Attr.class_ "review-posts" ]
           (List.mapi c.thread ~f:(fun i p -> post_view m c i p ~inject)))
    ; (if resolved
       then Node.none
       else
         div
           ~cls:"review-reply"
           [ Node.textarea
               ~attrs:
                 [ Attr.id field_id
                 ; Attr.class_ "review-field"
                 ; Attr.placeholder "Reply…"
                 ; Attr.create "rows" "1"
                 ; Attr.create "aria-label" ("Reply to " ^ c.id)
                 ; Attr.string_property "value" reply
                 ; Attr.on_input (fun _ text ->
                     inject (review (Reply_input { id = c.id; text })))
                 ; ctrl_enter ~inject (review (Reply_send c.id))
                 ]
               []
           ; (if String.is_empty (String.strip reply)
              then Node.none
              else
                button
                  ~cls:"small"
                  ~disabled:busy
                  ~title:"Ctrl+Enter"
                  ~on_click:(inject (review (Reply_send c.id)))
                  [ Node.text "Reply" ])
           ])
    ]
;;

let draft_view (d : Code_review.Draft.t) ~inject =
  div
    ~cls:"review-thread draft"
    [ div
        ~cls:"review-thread-head"
        [ span ~cls:"review-where" ("Comment on " ^ lines_label d.first d.last)
        ]
    ; Node.textarea
        ~attrs:
          [ Attr.id "review-draft"
          ; Attr.class_ "review-field"
          ; Attr.placeholder
              "What should change here? prigh reads it with the code."
          ; Attr.create "rows" "3"
          ; Attr.create "aria-label" "Comment"
          ; Attr.string_property "value" d.text
          ; Attr.on_input (fun _ text -> inject (review (Draft_input text)))
          ; ctrl_enter ~inject (review Draft_send)
          ; Attr.on_keydown (fun ev ->
              match Js_of_ocaml.Js.Optdef.to_option ev##.key with
              | Some k when String.equal (Js_of_ocaml.Js.to_string k) "Escape"
                ->
                Effect.Many
                  [ Effect.Stop_propagation; inject (review Draft_cancel) ]
              | _ -> Effect.Ignore)
          ]
        []
    ; div
        ~cls:"review-draft-actions"
        [ span
            ~cls:"keys-hint"
            ~attrs:
              [ Attr.title "Send then gives prigh every new comment at once" ]
            "Ctrl+Enter adds it"
        ; button
            ~cls:"small ghost"
            ~on_click:(inject (review Draft_cancel))
            [ Node.text "Cancel" ]
        ; button
            ~cls:"small primary"
            ~disabled:d.busy
            ~on_click:(inject (review Draft_send))
            [ Node.text "Add comment" ]
        ]
    ]
;;

(* A row spanning the table, holding a thread or the draft. *)
let insert node =
  Node.tr
    ~attrs:[ Attr.class_ "review-insert" ]
    [ Node.td ~attrs:[ Attr.create "colspan" "4" ] [ node ] ]
;;

let comment_button (s : Code_review.Selection.t) ~inject =
  insert
    (div
       ~cls:"review-select-bar"
       [ button
           ~cls:"small primary"
           ~title:"c"
           ~on_click:(inject (review Start_comment))
           [ icon Pencil
           ; Node.text ("Comment on " ^ lines_label s.first s.last)
           ]
       ; button
           ~cls:"small ghost"
           ~title:"Esc"
           ~on_click:(inject (review Clear_selection))
           [ Node.text "Clear" ]
       ])
;;

let view (m : App.Model.t) ~path ~inject =
  let t = m.review in
  let file = Code_review.file t path in
  let { Rows.rows; tokens } = rows t ~path ~file in
  let comments = Review.on t.review path in
  let lines =
    Option.map (text_of t path) ~f:(fun text ->
      Array.of_list (String.split_lines text))
  in
  let located =
    List.map comments ~f:(fun c ->
      match lines with
      | Some lines -> c, Comment_anchor.locate c ~lines
      | None -> c, Comment_anchor.Located.At { first = c.first; last = c.last })
  in
  let shown_lines =
    List.filter_map rows ~f:(fun (r : Row.t) ->
      match r with
      | Line { new_line = Some n; _ } -> Some n
      | Line _ | Hunk _ -> None)
    |> Int.Set.of_list
  in
  let at_line =
    List.filter_map located ~f:(fun (c, l) ->
      match l with
      | At { first; last } when Set.mem shown_lines last ->
        Some (last, (c, first))
      | At _ | Outdated -> None)
  in
  let elsewhere =
    List.filter_map located ~f:(fun (c, l) ->
      match l with
      | At { last; _ } when Set.mem shown_lines last -> None
      | At _ -> Some (c, false)
      | Outdated -> Some (c, true))
  in
  let commented =
    List.concat_map located ~f:(fun (_, l) ->
      match l with
      | At { first; last } -> List.range first (last + 1)
      | Outdated -> [])
    |> Int.Set.of_list
  in
  let selection =
    Option.filter t.selection ~f:(fun s -> String.equal s.path path)
  in
  let draft = Option.filter t.draft ~f:(fun d -> String.equal d.path path) in
  let body =
    List.concat
      (List.map2_exn rows tokens ~f:(fun (row : Row.t) tok ->
         let line =
           match row with
           | Line { new_line; _ } -> new_line
           | Hunk _ -> None
         in
         let deps =
           { Deps.selected =
               Option.exists line ~f:(fun n ->
                 Option.exists selection ~f:(fun s ->
                   n >= s.first && n <= s.last))
           ; first =
               Option.exists line ~f:(fun n ->
                 Option.exists selection ~f:(fun s -> n = s.first))
           ; commented = Option.exists line ~f:(Set.mem commented)
           }
         in
         let node =
           View_cache.find
             ~inject
             row_cache
             row
             ~deps
             ~equal:Deps.equal
             ~f:(fun () -> row_view ~path row tok deps ~inject)
         in
         match line, row with
         | Some n, Line { kind = `Added | `Same; _ } ->
           let threads =
             List.filter_map at_line ~f:(fun (l, (c, first)) ->
               Option.some_if
                 (l = n)
                 (insert (thread m c ~at:(first, l) ~outdated:false ~inject)))
           in
           let after_selection =
             match selection, draft with
             | Some s, None when s.last = n -> [ comment_button s ~inject ]
             | _ -> []
           in
           let after_draft =
             match draft with
             | Some d when d.last = n -> [ insert (draft_view d ~inject) ]
             | Some _ | None -> []
           in
           (node :: threads) @ after_selection @ after_draft
         | _ -> [ node ]))
  in
  let empty =
    match rows, file, Map.find t.contents path with
    | [], Some f, _ ->
      [ div
          ~cls:"review-empty"
          [ Node.text
              (match f.status with
               | Binary -> "A binary file: no lines to show."
               | Added | Deleted | Modified -> "No text changes to show.")
          ]
      ]
    | [], None, Some Loading ->
      [ div ~cls:"review-empty" [ lamp (); Node.text "Reading…" ] ]
    | [], None, Some (Failed e) ->
      [ div ~cls:"review-empty" [ Node.text ("Cannot show it: " ^ e) ] ]
    | _ -> []
  in
  let full_hint =
    match file, t.layout, Map.find t.contents path with
    | Some _, Full, Some Loading ->
      [ div ~cls:"review-note" [ lamp (); Node.text "Reading the whole file…" ]
      ]
    | Some _, Full, Some (Failed e) ->
      [ div
          ~cls:"review-note"
          [ Node.text
              (sprintf "Only the hunks: the file cannot be read (%s)." e)
          ]
      ]
    | _ -> []
  in
  div
    ~cls:"review-code"
    ([ (match elsewhere with
        | [] -> Node.none
        | elsewhere ->
          div
            ~cls:"review-elsewhere"
            (List.map elsewhere ~f:(fun (c, outdated) ->
               thread m c ~outdated ~inject)))
     ]
     @ full_hint
     @ empty
     @ [ (if List.is_empty rows
          then Node.none
          else
            Node.table
              ~attrs:[ Attr.class_ "diff review-diff" ]
              [ Node.tbody body ])
       ])
;;
