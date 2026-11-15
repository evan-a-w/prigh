open! Core
open! Import
open Html
module Comment_anchor = Prigh_ui.Comment_anchor
module Row = Diff_view.Row

let review action = App.Action.Review action

(* ---- rows *)

module Tokens = struct
  (* Highlighted (and the changed words found) once per rows value (see
     [Code_review.rows]). *)
  type t =
    { tokens : Highlight.Token.t list option list
    ; changes : Word_diff.Range.t list list
    }

  let memo : (string, Row.t list * t) Hashtbl.t = Hashtbl.create (module String)

  let find ~path rows =
    match Hashtbl.find memo path with
    | Some (r, t) when phys_equal r rows -> t
    | Some _ | None ->
      let tokens =
        match Highlight.Lang.of_path path with
        | Some lang when List.length rows <= 20_000 ->
          Diff_view.highlighted lang rows
        | Some _ | None -> List.map rows ~f:(fun _ -> None)
      in
      let t = { tokens; changes = Diff_view.word_changes rows } in
      Hashtbl.set memo ~key:path ~data:(rows, t);
      t
  ;;
end

(* ---- a row *)

module Mark = struct
  (* Text of a row a comment (or the draft) is on: bytes [start, end_). *)
  type t =
    { start : int
    ; end_ : int
    ; id : string option (** the comment's; [None]: the draft's *)
    ; hot : bool (** its thread is pointed at *)
    ; quiet : bool (** resolved *)
    }
  [@@deriving equal]
end

module Deps = struct
  type t =
    { selected : bool (** by its line number *)
    ; first : bool (** of the selection *)
    ; commented : bool
    ; marks : Mark.t list
    ; changed : Word_diff.Range.t list
    ; start : bool (** of a change: what Shift+J goes to *)
    ; tap : bool (** on a phone: a tap anywhere on it selects it *)
    }
  [@@deriving equal]
end

let row_cache : (Row.t, Deps.t) View_cache.t = View_cache.create ()

(* The tokens' text, split where marks and changed words start and end: the
   changed words in [span.wd]s, the marked parts in [<mark>]s. *)
let marked tokens (marks : Mark.t list) ~changed ~inject =
  match marks with
  | [] -> Diff_view.changed_spans tokens ~changed
  | marks ->
    let cuts =
      List.concat_map marks ~f:(fun m -> [ m.start; m.end_ ])
      @ List.concat_map changed ~f:(fun (r : Word_diff.Range.t) ->
        [ r.start; r.end_ ])
      |> List.dedup_and_sort ~compare:Int.compare
    in
    let pieces = Diff_view.pieces tokens ~cuts in
    let covering at =
      List.filter marks ~f:(fun m -> m.start <= at && at < m.end_)
    in
    (* A mark is one element, the changed words in it spans inside it. *)
    List.group pieces ~break:(fun (a, _) (b, _) ->
      not (List.equal Mark.equal (covering a) (covering b)))
    |> List.map ~f:(fun group ->
      let at = fst (List.hd_exn group) in
      let spans =
        List.group group ~break:(fun (a, _) (b, _) ->
          not
            (Bool.equal
               (Diff_view.in_change changed a)
               (Diff_view.in_change changed b)))
        |> List.concat_map ~f:(fun run ->
          let spans = Highlight_view.spans (List.map run ~f:snd) in
          if Diff_view.in_change changed (fst (List.hd_exn run))
          then [ Node.span ~attrs:[ Attr.class_ "wd" ] spans ]
          else spans)
      in
      match covering at with
      | [] -> spans
      | m :: _ as ms ->
        [ Node.create
            "mark"
            ~attrs:
              ([ classes
                   [ "review-mark" ]
                   [ "hot", List.exists ms ~f:(fun m -> m.hot)
                   ; "draft", Option.is_none m.id
                   ; "many", List.length ms > 1
                   ; "quiet", List.for_all ms ~f:(fun m -> m.quiet)
                   ]
               ]
               @
               match m.id with
               | None -> []
               | Some id ->
                 [ Attr.create "data-comment" id
                 ; Attr.on_mouseenter (fun _ ->
                     inject (review (Hover (Some id))))
                 ; Attr.on_mouseleave (fun _ -> inject (review (Hover None)))
                 ])
            spans
        ])
    |> List.concat
;;

(* Whether the page has text selected (a long press on a phone): a tap that
   ends one is not a tap on a line. *)
let text_selected () =
  let open Js_of_ocaml in
  let global = Js.Unsafe.global in
  Js.Optdef.test (Js.Unsafe.get global "getSelection")
  &&
  let sel = Js.Unsafe.meth_call global "getSelection" [||] in
  Js.Opt.test sel && not (Js.to_bool (Js.Unsafe.get sel "isCollapsed"))
;;

let gap_view ~path (g : Diff_view.Gap.t) ~inject =
  let show how label title =
    Node.button
      ~attrs:
        [ Attr.class_ "btn small ghost review-gap-show"
        ; Attr.type_ "button"
        ; Attr.title title
        ; Attr.on_click (fun _ ->
            inject (review (Reveal { path; gap = g; how })))
        ]
      [ Node.text label ]
  in
  let step = Int.min Context_fold.step g.hidden in
  Node.tr
    ~attrs:[ Attr.class_ "gap" ]
    [ Node.td
        ~attrs:[ Attr.create "colspan" "3" ]
        [ div
            ~cls:"review-gap"
            ([ span ~cls:"review-gap-text" (Diff_view.gap_text g) ]
             @ (if g.before && step < g.hidden
                then
                  [ show
                      Down
                      (sprintf "↓ %d" step)
                      (sprintf "Show %d more lines under the ones above" step)
                  ]
                else [])
             @ [ show All "Show all" "Show every hidden line here" ]
             @
             if g.after && step < g.hidden
             then
               [ show
                   Up
                   (sprintf "↑ %d" step)
                   (sprintf "Show %d more lines over the ones below" step)
               ]
             else [])
        ]
    ]
;;

let row_view ~path (row : Row.t) tokens (deps : Deps.t) ~inject =
  match row with
  | Gap g -> gap_view ~path g ~inject
  | Hunk text ->
    Node.tr
      ~attrs:[ Attr.class_ "hunk" ]
      [ Node.td
          ~attrs:[ Attr.create "colspan" "3" ]
          (Diff_view.hunk_header text)
      ]
  | Line { kind; text; _ } ->
    let cls, sign =
      match kind with
      | `Same -> "same", " "
      | `Added -> "add", "+"
      | `Removed -> "del", "-"
    in
    let key = Code_selection.key row in
    let line_cell =
      match key with
      | None -> Node.td ~attrs:[ Attr.class_ "ln" ] []
      | Some (side, line) ->
        Node.td
          ~attrs:
            ([ classes
                 [ "ln"; "pick" ]
                 [ "old", Review_comment.Side.equal side Old ]
             ]
             @
             if deps.tap
             then []
             else
               [ Attr.on_click (fun ev ->
                   inject
                     (review
                        (Select_line
                           { path
                           ; side
                           ; line
                           ; extend = Js_of_ocaml.Js.to_bool ev##.shiftKey
                           })))
               ])
          [ Node.text (Int.to_string line) ]
    in
    let tokens =
      Option.value tokens ~default:[ { Highlight.Token.kind = Plain; text } ]
    in
    Node.tr
      ~attrs:
        ([ classes
             [ cls ]
             [ "selected", deps.selected
             ; "sel-first", deps.first
             ; "commented", deps.commented
             ; "change-start", deps.start
             ]
         ]
         @ (match key with
            | Some (side, n) ->
              [ Attr.create "data-side" (Review_comment.Side.to_string side)
              ; Attr.create "data-line" (Int.to_string n)
              ]
            | None -> [])
         @
         match key with
         | Some (side, line) when deps.tap ->
           [ Attr.on_click (fun _ ->
               if text_selected ()
               then Effect.Ignore
               else inject (review (Tap_line { path; side; line })))
           ]
         | Some _ | None -> [])
      [ line_cell
      ; Node.td
          ~attrs:[ Attr.class_ "sign"; Attr.create "aria-hidden" "true" ]
          [ Node.text sign ]
      ; Node.td
          ~attrs:(Diff_view.text_attrs text)
          (marked tokens deps.marks ~changed:deps.changed ~inject)
      ]
;;

(* ---- threads *)

(* Enter sends, Shift+Enter is a new line, Esc cancels (and goes no
   further: it would close the review). *)
let field_keys (m : App.Model.t) ~field ~inject =
  Attr.on_keydown (fun ev ->
    let key =
      Js_of_ocaml.Js.Optdef.case
        ev##.key
        (fun () -> "")
        Js_of_ocaml.Js.to_string
    in
    let composing =
      Js_of_ocaml.Js.to_bool (Js_of_ocaml.Js.Unsafe.coerce ev)##.isComposing
    in
    match
      if composing
      then None
      else
        Code_review.field_key
          m.review
          ~field
          ~key
          ~shift:(Js_of_ocaml.Js.to_bool ev##.shiftKey)
          ~ctrl:(Js_of_ocaml.Js.to_bool ev##.ctrlKey)
          ~meta:(Js_of_ocaml.Js.to_bool ev##.metaKey)
    with
    | Some action ->
      ev##preventDefault;
      Effect.Many [ Effect.Stop_propagation; inject (review action) ]
    | None -> Effect.Ignore)
;;

(* The selected text, as the comment box and the thread quote it: a few
   lines at most. *)
let quote ~(range : Review_comment.Range.t) ~anchor =
  match range.columns with
  | None -> Node.none
  | Some _ ->
    let lines =
      String.split_lines (Review_comment.Range.selected range ~anchor)
    in
    let shown = List.take lines 4 in
    Node.create
      "blockquote"
      ~attrs:
        [ classes
            [ "review-quote" ]
            [ "old", Review_comment.Side.equal range.side Old ]
        ]
      [ Node.pre
          [ Node.text
              (String.concat ~sep:"\n" shown
               ^ if List.length lines > List.length shown then "\n…" else "")
          ]
      ]
;;

let where = Review_comment.Range.label

let old_flag (range : Review_comment.Range.t) =
  match range.side with
  | Old ->
    span
      ~cls:"review-flag old"
      ~attrs:[ Attr.title "On lines the change removed" ]
      "old text"
  | New -> Node.none
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
                   ~disabled:
                     (busy || Option.is_some (Code_review.read_only m.review))
                   ~title:
                     (match Code_review.read_only m.review with
                      | Some rev ->
                        sprintf
                          "%s is not checked out: Check out into a worktree to \
                           apply it"
                          rev
                      | None -> "Apply this change to the file")
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
  let range = Option.value at ~default:c.range in
  let busy = Set.mem m.review.busy c.id in
  let reply = Option.value (Map.find m.review.replies c.id) ~default:"" in
  let resolved = Review_comment.Status.equal c.status Resolved in
  let field_id = "review-reply-" ^ c.id in
  Node.create
    "article"
    ~attrs:
      [ classes
          [ "review-thread" ]
          [ "resolved", resolved
          ; "outdated", outdated
          ; "hot", Option.exists m.review.hover ~f:(String.equal c.id)
          ]
      ; Attr.id ("review-thread-" ^ c.id)
      ; Attr.create "data-comment" c.id
      ; Attr.create "aria-label" (sprintf "Comment %s on %s" c.id (where range))
      ; Attr.on_mouseenter (fun _ -> inject (review (Hover (Some c.id))))
      ; Attr.on_mouseleave (fun _ -> inject (review (Hover None)))
      ]
    [ div
        ~cls:"review-thread-head"
        [ span ~cls:"review-id" c.id
        ; span ~cls:"review-where" (where range)
        ; old_flag range
        ; (if outdated
           then span ~cls:"review-flag" "its code changed"
           else Node.none)
        ; (if resolved
           then span ~cls:"review-flag done" "resolved"
           else Node.none)
        ; (if busy then lamp ~title:"Saving" () else Node.none)
        ; div
            ~cls:"review-thread-tools"
            [ (if resolved && not outdated
               then
                 button
                   ~cls:"icon ghost review-fold"
                   ~title:"Fold it to its mark on the line"
                   ~label:"Fold"
                   ~on_click:(inject (review (Unfold c.id)))
                   [ icon ~cls:"chevron up" Chevron ]
               else Node.none)
            ; button
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
    ; quote ~range:c.range ~anchor:c.anchor
    ; Node.ul
        ~attrs:[ Attr.class_ "review-posts" ]
        (List.mapi c.thread ~f:(fun i p -> post_view m c i p ~inject))
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
                 ; field_keys m ~field:(`Reply c.id) ~inject
                 ]
               []
           ; (if String.is_empty (String.strip reply)
              then Node.none
              else
                button
                  ~cls:"small"
                  ~disabled:busy
                  ~title:"Enter (Shift+Enter: a new line)"
                  ~on_click:(inject (review (Reply_send c.id)))
                  [ Node.text "Reply" ])
           ])
    ]
;;

let draft_view (m : App.Model.t) (d : Code_review.Draft.t) ~inject =
  div
    ~cls:"review-thread draft"
    [ div
        ~cls:"review-thread-head"
        [ span ~cls:"review-where" ("Comment on " ^ where d.range)
        ; old_flag d.range
        ]
    ; quote ~range:d.range ~anchor:d.anchor
    ; Node.textarea
        ~attrs:
          [ Attr.id "review-draft"
          ; Attr.class_ "review-field"
          ; Attr.placeholder
              (match d.range.columns with
               | Some _ ->
                 "What should change here? prigh reads it with the code."
               | None -> "What should change in these lines?")
          ; Attr.create "rows" "3"
          ; Attr.create "aria-label" "Comment"
          ; Attr.string_property "value" d.text
          ; Attr.on_input (fun _ text -> inject (review (Draft_input text)))
          ; field_keys m ~field:`Draft ~inject
          ]
        []
    ; div
        ~cls:"review-draft-actions"
        [ span
            ~cls:"keys-hint keyboard"
            ~attrs:
              [ Attr.title "Send then gives prigh every new comment at once" ]
            "Enter adds it · Shift+Enter a new line · Esc cancels"
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
    [ Node.td ~attrs:[ Attr.create "colspan" "3" ] [ node ] ]
;;

(* Lines picked by their numbers: the bar under them. *)
let comment_bar (s : Code_review.Selection.t) ~inject =
  insert
    (div
       ~cls:"review-select-bar"
       [ button
           ~cls:"small primary"
           ~title:"c"
           ~on_click:(inject (review Start_comment))
           [ icon Pencil; Node.text ("Comment on " ^ where s.range) ]
       ; button
           ~cls:"small ghost"
           ~title:"Esc"
           ~on_click:(inject (review Clear_selection))
           [ Node.text "Clear" ]
       ])
;;

(* Text selected: the button floating by its end. Pressing it keeps the
   page's selection. *)
let pop (p : Code_review.Pop.t) ~inject =
  Node.div
    ~attrs:
      [ Attr.class_ "review-pop"
      ; Attr.style Css_gen.(left (`Px p.left) @> top (`Px p.top))
      ]
    [ Node.button
        ~attrs:
          [ Attr.class_ "btn small primary"
          ; Attr.type_ "button"
          ; Attr.title "Comment on the selected code (c)"
          ; Attr.on_mousedown (fun ev ->
              Js_of_ocaml.Dom.preventDefault ev;
              Effect.Ignore)
          ; Attr.on_click (fun _ -> inject (review Start_comment))
          ]
        [ icon Pencil
        ; Node.text "Comment"
        ; Node.kbd ~attrs:[ Attr.class_ "review-pop-key" ] [ Node.text "c" ]
        ]
    ]
;;

(* The lines of a side as they are now, to find comments again in: the
   file's text (or the rows shown, while it is read), or the removed rows. *)
let side_lines (t : Code_review.t) ~path rows (side : Review_comment.Side.t) =
  let of_rows () =
    let map =
      List.filter_map rows ~f:(fun row ->
        match Code_selection.key row, row with
        | Some (s, n), Row.Line { text; _ }
          when Review_comment.Side.equal s side -> Some (n, text)
        | _ -> None)
      |> Int.Map.of_alist_reduce ~f:(fun a _ -> a)
    in
    Map.find map, Option.value_map (Map.max_elt map) ~default:0 ~f:fst
  in
  match side, Code_review.lines t path with
  | New, Some lines ->
    ( (fun n ->
        if n >= 1 && n <= Array.length lines then Some lines.(n - 1) else None)
    , Array.length lines )
  | New, None | Old, _ -> of_rows ()
;;

(* A resolved thread folded to its line: a mark that unfolds it. *)
let folded_mark (m : App.Model.t) (c : Review_comment.t) ~inject =
  let first =
    List.find_map c.thread ~f:(fun (p : Review_comment.Post.t) ->
      match p.author with
      | User -> Some p.text
      | Agent -> None)
    |> Option.value ~default:""
    |> String.split_lines
    |> List.hd
    |> Option.value ~default:""
  in
  Node.button
    ~attrs:
      [ classes
          [ "review-folded-thread" ]
          [ "hot", Option.exists m.review.hover ~f:(String.equal c.id) ]
      ; Attr.type_ "button"
      ; Attr.id ("review-thread-" ^ c.id)
      ; Attr.create "data-comment" c.id
      ; Attr.create "aria-expanded" "false"
      ; Attr.title
          (sprintf
             "Resolved: %s on %s. Click to show its thread."
             c.id
             (where c.range))
      ; Attr.on_click (fun _ -> inject (review (Unfold c.id)))
      ; Attr.on_mouseenter (fun _ -> inject (review (Hover (Some c.id))))
      ; Attr.on_mouseleave (fun _ -> inject (review (Hover None)))
      ]
    [ icon Check
    ; span ~cls:"review-id" c.id
    ; span ~cls:"review-folded-text" first
    ; span ~cls:"review-folded-count" (Int.to_string (List.length c.thread))
    ]
;;

(* Threads not on a line shown, folded to one line each until opened. *)
let folded_list (m : App.Model.t) ~key ~label ~title threads ~inject =
  match threads with
  | [] -> Node.none
  | threads ->
    let open_ = Set.mem m.review.unfolded key in
    div
      ~cls:"review-aside-list"
      ([ Node.button
           ~attrs:
             [ classes [ "review-aside-toggle" ] [ "open", open_ ]
             ; Attr.type_ "button"
             ; Attr.create "aria-expanded" (Bool.to_string open_)
             ; Attr.title title
             ; Attr.on_click (fun _ -> inject (review (Unfold key)))
             ]
           [ icon ~cls:"chevron" Chevron
           ; Node.text (label (List.length threads))
           ]
       ]
       @
       if open_
       then [ div ~cls:"review-aside-threads" (List.map threads ~f:Fn.id) ]
       else [])
;;

(* Where the review's comments show beside the code rather than between its
   lines: wide enough for both, not on a phone. *)
let beside (m : App.Model.t) ~place =
  let shown = App.Model.shown m in
  let margin = (Resize.bounds m.layout shown Review_margin).value in
  let room =
    match place with
    | `Page -> Resize.review_room m.layout shown
    | `Files -> Resize.preview_width m.layout shown
  in
  (not m.narrow) && room - 40 - margin >= 440
;;

let plural n one many = if n = 1 then one else sprintf many n

(* ---- files folded on a phone *)

let folded_key path = "file:" ^ path

(* The folded files not approved yet, as they are now. *)
let folded_unapproved (t : Code_review.t) =
  let folded = Folded_files.classify t.files in
  List.filter_map t.files ~f:(fun f ->
    Option.some_if
      (Map.mem folded f.path
       && not
            (Prigh_ui.File_approval.Status.equal
               (Code_review.approval t f)
               Approved))
      f.path)
;;

let folded_card (m : App.Model.t) ~path (why : Folded_files.Why.t) ~inject =
  let t = m.review in
  let text =
    match why with
    | Generated ->
      "A generated file (a lock file, build output or minified code): its \
       changes are folded."
    | Renamed_from from -> sprintf "Renamed from %s, its text unchanged." from
    | Renamed_to to_ -> sprintf "Renamed to %s, its text unchanged." to_
  in
  let unapproved = folded_unapproved t in
  div
    ~cls:"review-folded-file"
    [ Node.p [ span ~cls:"chip" (Folded_files.Why.label why); Node.text text ]
    ; div
        ~cls:"review-folded-actions"
        ([ button
             ~cls:"small"
             ~on_click:(inject (review (Unfold (folded_key path))))
             [ Node.text "Show the changes" ]
         ]
         @
         match unapproved with
         | [] | [ _ ] -> []
         | paths ->
           [ button
               ~cls:"small primary"
               ~title:(String.concat ~sep:"\n" paths)
               ~on_click:
                 (inject (review (Approve_files { paths; approve = true })))
               [ icon Check
               ; Node.textf
                   "Approve the %d generated and renamed"
                   (List.length paths)
               ]
           ])
    ]
;;

let view ?(place = `Page) (m : App.Model.t) ~path ~inject =
  let t = m.review in
  let file = Code_review.file t path in
  let rows = Code_review.rows t ~path in
  let { Tokens.tokens; changes } = Tokens.find ~path rows in
  (* On a phone the review page's sheet has the selection and the comment
     being written, and a tap on a line selects it. *)
  let sheet =
    m.narrow
    &&
    match place with
    | `Page -> true
    | `Files -> false
  in
  let starts =
    let changed (row : Row.t) =
      match row with
      | Line { kind = `Added | `Removed; _ } -> true
      | Line { kind = `Same; _ } | Hunk _ | Gap _ -> false
    in
    List.folding_map rows ~init:false ~f:(fun before row ->
      let now = changed row in
      now, now && not before)
  in
  let comments = Review.on t.review path in
  let lines_new = lazy (side_lines t ~path rows New) in
  let lines_old = lazy (side_lines t ~path rows Old) in
  let since_view =
    Code_review.Layout.equal (Code_review.layout_of t path) Since
  in
  let since = Option.is_some (Code_review.since_diff t path) in
  let beside = beside m ~place in
  let located =
    List.map comments ~f:(fun (c : Review_comment.t) ->
      let line, count =
        force
          (match c.range.side with
           | New -> lines_new
           | Old -> lines_old)
      in
      c, Comment_anchor.locate c ~line ~count)
  in
  let shown = List.filter_map rows ~f:Code_selection.key |> Set.Poly.of_list in
  (* The since-view's removed lines are the file at the action, not what
     the change removed: comments on those are listed apart. *)
  let shown_at (r : Review_comment.Range.t) =
    (not (since && Review_comment.Side.equal r.side Old))
    && Set.mem shown (r.side, r.last)
  in
  let at_row =
    List.filter_map located ~f:(fun (c, l) ->
      match l with
      | At r when shown_at r -> Some (c, r)
      | At _ | Outdated -> None)
  in
  let unfolded (c : Review_comment.t) =
    (not (Review_comment.Status.equal c.status Resolved))
    || Set.mem t.unfolded c.id
  in
  let outdated =
    List.filter_map located ~f:(fun (c, l) ->
      match l with
      | Outdated -> Some (thread m c ~outdated:true ~inject)
      | At _ -> None)
  in
  let hidden =
    List.filter_map located ~f:(fun (c, l) ->
      match l with
      | At r when not (shown_at r) ->
        Some (thread m c ~at:r ~outdated:false ~inject)
      | At _ | Outdated -> None)
  in
  let selection =
    Option.filter t.selection ~f:(fun s -> String.equal s.path path)
  in
  let draft = Option.filter t.draft ~f:(fun d -> String.equal d.path path) in
  let line_selection =
    Option.bind selection ~f:(fun s -> Option.map s.from ~f:(fun _ -> s.range))
  in
  let ends_at row (r : Review_comment.Range.t) =
    Option.equal
      [%equal: Review_comment.Side.t * int]
      (Code_selection.key row)
      (Some (r.side, r.last))
  in
  let shown_thread (c, r) =
    if unfolded c
    then thread m c ~at:r ~outdated:false ~inject
    else folded_mark m c ~inject
  in
  let body =
    List.concat
      (List.map2_exn
         (List.zip_exn rows tokens)
         (List.zip_exn changes starts)
         ~f:(fun ((row : Row.t), tok) (changed, start) ->
           let marks =
             List.filter_map at_row ~f:(fun (c, r) ->
               Option.map
                 (Code_selection.covered r row)
                 ~f:(fun (start, end_) ->
                   { Mark.start
                   ; end_
                   ; id = Some c.id
                   ; hot = Option.exists t.hover ~f:(String.equal c.id)
                   ; quiet = Review_comment.Status.equal c.status Resolved
                   }))
             @ Option.to_list
                 (Option.bind draft ~f:(fun d ->
                    Option.map
                      (Code_selection.covered d.range row)
                      ~f:(fun (start, end_) ->
                        { Mark.start
                        ; end_
                        ; id = None
                        ; hot = false
                        ; quiet = false
                        })))
           in
           let selected =
             Option.bind line_selection ~f:(fun r ->
               Code_selection.covered r row)
           in
           let deps =
             { Deps.selected = Option.is_some selected
             ; first =
                 Option.is_some selected
                 && Option.exists line_selection ~f:(fun r ->
                   Option.exists (Code_selection.key row) ~f:(fun (_, n) ->
                     n = r.first))
             ; commented = not (List.is_empty marks)
             ; marks = List.filter marks ~f:(fun m -> m.end_ > m.start)
             ; changed
             ; start
             ; tap = sheet
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
           let threads =
             if beside
             then []
             else
               List.filter_map at_row ~f:(fun (c, r) ->
                 Option.some_if (ends_at row r) (insert (shown_thread (c, r))))
           in
           let bar =
             match selection, draft with
             | Some ({ from = Some _; _ } as s), None
               when (not sheet) && ends_at row s.range ->
               [ comment_bar s ~inject ]
             | _ -> []
           in
           let draft_row =
             match draft with
             | Some d when (not (beside || sheet)) && ends_at row d.range ->
               [ insert (draft_view m d ~inject) ]
             | Some _ | None -> []
           in
           (node :: threads) @ bar @ draft_row))
  in
  (* Beside the code: each thread (and the draft) by the first of its lines
     shown; the page's glue (app/review_margin.ml) moves them down to their
     line, and further when the one above is in the way. *)
  let margin () =
    let index =
      List.filter_map rows ~f:Code_selection.key
      |> List.mapi ~f:(fun i key -> key, i)
      |> Map.Poly.of_alist_reduce ~f:(fun a _ -> a)
    in
    let item ~key ~cls (r : Review_comment.Range.t) node =
      let line =
        List.range r.first (r.last + 1)
        |> List.find ~f:(fun n -> Set.mem shown (r.side, n))
        |> Option.value ~default:r.last
      in
      ( Option.value (Map.find index (r.side, line)) ~default:Int.max_value
      , Node.div
          ~key
          ~attrs:
            [ Attr.class_ ("review-margin-item " ^ cls)
            ; Attr.create "data-side" (Review_comment.Side.to_string r.side)
            ; Attr.create "data-line" (Int.to_string line)
            ]
          [ node ] )
    in
    let items =
      List.map at_row ~f:(fun ((c : Review_comment.t), r) ->
        item
          ~key:c.id
          ~cls:(if unfolded c then "thread" else "folded")
          r
          (shown_thread (c, r)))
      @ Option.to_list
          (Option.map draft ~f:(fun d ->
             item ~key:"draft" ~cls:"draft" d.range (draft_view m d ~inject)))
    in
    Node.create
      "aside"
      ~attrs:
        [ Attr.class_ "review-margin"; Attr.create "aria-label" "Comments" ]
      ((match place with
        | `Page -> Resize_view.handle m ~inject Review_margin
        | `Files -> Node.none)
       :: List.map
            (List.stable_sort items ~compare:(fun (a, _) (b, _) ->
               Int.compare a b))
            ~f:snd)
  in
  let empty =
    match rows, file, Map.find t.contents path with
    | _ when since -> []
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
    match file, Code_review.layout_of t path, Map.find t.contents path with
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
  let aside =
    match outdated, hidden with
    | [], [] -> Node.none
    | _ ->
      div
        ~cls:"review-asides"
        [ folded_list
            m
            ~key:("outdated:" ^ path)
            ~label:(fun n ->
              plural n "1 outdated comment" "%d outdated comments")
            ~title:"Comments whose code changed so much it is not found again"
            outdated
            ~inject
        ; folded_list
            m
            ~key:("hidden:" ^ path)
            ~label:(fun n ->
              plural
                n
                "1 comment on lines not shown"
                "%d comments on lines not shown")
            ~title:
              (if since
               then
                 "On lines this since-view does not show: Changes or Whole \
                  file shows them by their lines"
               else "On lines outside these hunks: Whole file shows them")
            hidden
            ~inject
        ]
  in
  let wrap = not (Set.mem t.scrolled path) in
  let table () =
    Node.table
      ~attrs:[ classes [ "diff"; "review-diff" ] [ "nowrap", not wrap ] ]
      [ Node.tbody body ]
  in
  let folded =
    if sheet && not (Set.mem t.unfolded (folded_key path))
    then Map.find (Folded_files.classify t.files) path
    else None
  in
  div
    ~cls:"review-code"
    ~attrs:
      [ Attr.create "data-path" path
      ; (if beside then Attr.class_ "beside" else Attr.empty)
      ; (if wrap then Attr.empty else Attr.class_ "scrolls")
      ]
    ([ (if since_view then Review_since_view.note m path else Node.none)
     ; aside
     ]
     @ full_hint
     @ empty
     @ [ (match rows, folded with
          | _, Some why -> folded_card m ~path why ~inject
          | [], None -> Node.none
          | _, None when beside ->
            div
              ~cls:"review-code-grid"
              [ div ~cls:"review-code-main" [ table () ]; margin () ]
          | _, None -> table ())
       ; (match selection, draft with
          | Some { pop = Some p; _ }, None when not sheet -> pop p ~inject
          | _ -> Node.none)
       ])
;;

(* ---- a phone's sheet *)

let segmented ~label items =
  Node.div
    ~attrs:
      [ Attr.class_ "segmented"
      ; Attr.role "group"
      ; Attr.create "aria-label" label
      ]
    (List.map items ~f:(fun (text, title, on, action) ->
       Node.button
         ~attrs:
           [ classes [ "segment" ] [ "on", on ]
           ; Attr.type_ "button"
           ; Attr.create "aria-pressed" (Bool.to_string on)
           ; Attr.title title
           ; Attr.on_click (fun _ -> action)
           ]
         [ Node.text text ]))
;;

let quick_chips ~inject =
  div
    ~cls:"review-chips"
    ~attrs:[ Attr.role "group"; Attr.create "aria-label" "Quick asks" ]
    (List.map Quick_ask.all ~f:(fun q ->
       button
         ~cls:"small review-chip"
         ~title:(Quick_ask.text q ^ " (sent to prigh at once)")
         ~on_click:(inject (review (Quick q)))
         [ Node.text (Quick_ask.label q) ]))
;;

let display_sheet (m : App.Model.t) ~path ~inject =
  let t = m.review in
  let wrap = not (Set.mem t.scrolled path) in
  div
    ~cls:"review-sheet display"
    ~attrs:[ Attr.role "group"; Attr.create "aria-label" "Display" ]
    [ div
        ~cls:"review-sheet-head"
        [ span ~cls:"review-sheet-title" "Display"
        ; button
            ~cls:"small"
            ~title:"Esc"
            ~on_click:(inject (review Toggle_display))
            [ Node.text "Done" ]
        ]
    ; div
        ~cls:"review-sheet-row"
        [ span ~cls:"review-sheet-label" "Long lines"
        ; segmented
            ~label:"Long lines"
            [ ( "Wrap"
              , "Wrap them under their indent (w)"
              , wrap
              , if wrap
                then Effect.Ignore
                else inject (review (Toggle_wrap path)) )
            ; ( "Scroll"
              , "Keep them whole: the code scrolls sideways (w)"
              , not wrap
              , if wrap
                then inject (review (Toggle_wrap path))
                else Effect.Ignore )
            ]
        ]
    ; div
        ~cls:"review-sheet-row"
        [ span ~cls:"review-sheet-label" "Code size"
        ; div
            ~cls:"review-size"
            [ button
                ~cls:"icon review-size-step"
                ~title:"Smaller (-)"
                ~label:"Smaller code"
                ~disabled:(Code_size.equal m.code_size Small)
                ~on_click:(inject (App.Action.Code_size (-1)))
                [ Node.text "A−" ]
            ; span ~cls:"review-size-name" (Code_size.name m.code_size)
            ; button
                ~cls:"icon review-size-step"
                ~title:"Bigger (+)"
                ~label:"Bigger code"
                ~disabled:(Code_size.equal m.code_size Huge)
                ~on_click:(inject (App.Action.Code_size 1))
                [ Node.text "A+" ]
            ]
        ]
    ]
;;

let sheet (m : App.Model.t) ~path ~inject =
  let t = m.review in
  if not m.narrow
  then None
  else if t.display_open
  then Some (display_sheet m ~path ~inject)
  else (
    match
      ( Option.filter t.draft ~f:(fun d -> String.equal d.path path)
      , Option.filter t.selection ~f:(fun s -> String.equal s.path path) )
    with
    | Some d, _ ->
      Some
        (div
           ~cls:"review-sheet comment"
           ~attrs:[ Attr.role "group"; Attr.create "aria-label" "Comment" ]
           [ draft_view m d ~inject
           ; (if String.is_empty (String.strip d.text) && not d.busy
              then quick_chips ~inject
              else Node.none)
           ])
    | None, Some s ->
      Some
        (div
           ~cls:"review-sheet selected"
           ~attrs:
             [ Attr.role "group"; Attr.create "aria-label" "Selected code" ]
           [ div
               ~cls:"review-sheet-head"
               [ span ~cls:"review-sheet-title" (where s.range)
               ; old_flag s.range
               ; span
                   ~cls:"review-sheet-hint"
                   (match s.from with
                    | Some _ -> "Tap another line to select to it"
                    | None -> "")
               ; button
                   ~cls:"icon ghost review-sheet-close"
                   ~title:"Clear the selection (Esc)"
                   ~label:"Clear the selection"
                   ~on_click:(inject (review Clear_selection))
                   [ icon Close ]
               ]
           ; quote ~range:s.range ~anchor:s.anchor
           ; quick_chips ~inject
           ; button
               ~cls:"primary review-sheet-comment"
               ~title:"c"
               ~on_click:(inject (review Start_comment))
               [ icon Pencil; Node.text "Write a comment" ]
           ])
    | None, None -> None)
;;
