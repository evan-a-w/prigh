open! Core
open! Import
open Html
module Comment_anchor = Prigh_ui.Comment_anchor
module Row = Diff_view.Row

let review action = App.Action.Review action

(* ---- rows *)

module Tokens = struct
  (* Highlighted once per rows value (see [Code_review.rows]). *)
  let memo : (string, Row.t list * Highlight.Token.t list option list) Hashtbl.t
    =
    Hashtbl.create (module String)
  ;;

  let find ~path rows =
    match Hashtbl.find memo path with
    | Some (r, tokens) when phys_equal r rows -> tokens
    | Some _ | None ->
      let tokens =
        match Highlight.Lang.of_path path with
        | Some lang when List.length rows <= 20_000 ->
          Diff_view.highlighted lang rows
        | Some _ | None -> List.map rows ~f:(fun _ -> None)
      in
      Hashtbl.set memo ~key:path ~data:(rows, tokens);
      tokens
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
    }
  [@@deriving equal]
end

module Deps = struct
  type t =
    { selected : bool (** by its line number *)
    ; first : bool (** of the selection *)
    ; commented : bool
    ; marks : Mark.t list
    }
  [@@deriving equal]
end

let row_cache : (Row.t, Deps.t) View_cache.t = View_cache.create ()

(* The tokens' text, split where marks start and end, the marked parts in
   [<mark>]s. *)
let marked tokens (marks : Mark.t list) ~inject =
  match marks with
  | [] -> Highlight_view.spans tokens
  | marks ->
    let cuts =
      List.concat_map marks ~f:(fun m -> [ m.start; m.end_ ])
      |> List.dedup_and_sort ~compare:Int.compare
    in
    let pieces =
      List.fold
        tokens
        ~init:(0, [])
        ~f:(fun (pos, acc) ({ kind; text } : Highlight.Token.t) ->
          let len = String.length text in
          let inside =
            List.filter cuts ~f:(fun c -> c > pos && c < pos + len)
          in
          let bounds = (pos :: inside) @ [ pos + len ] in
          let rec split acc = function
            | a :: (b :: _ as rest) ->
              split
                (( a
                 , { Highlight.Token.kind
                   ; text = String.sub text ~pos:(a - pos) ~len:(b - a)
                   } )
                 :: acc)
                rest
            | [ _ ] | [] -> acc
          in
          pos + len, split acc bounds)
      |> snd
      |> List.rev
    in
    let covering at =
      List.filter marks ~f:(fun m -> m.start <= at && at < m.end_)
    in
    List.group pieces ~break:(fun (a, _) (b, _) ->
      not (List.equal Mark.equal (covering a) (covering b)))
    |> List.map ~f:(fun group ->
      let at = fst (List.hd_exn group) in
      let spans = Highlight_view.spans (List.map group ~f:snd) in
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

let row_view ~path (row : Row.t) tokens (deps : Deps.t) ~inject =
  match row with
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
            [ classes
                [ "ln"; "pick" ]
                [ "old", Review_comment.Side.equal side Old ]
            ; Attr.on_click (fun ev ->
                inject
                  (review
                     (Select_line
                        { path
                        ; side
                        ; line
                        ; extend = Js_of_ocaml.Js.to_bool ev##.shiftKey
                        })))
            ]
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
             ]
         ]
         @
         match key with
         | Some (side, n) ->
           [ Attr.create "data-side" (Review_comment.Side.to_string side)
           ; Attr.create "data-line" (Int.to_string n)
           ]
         | None -> [])
      [ line_cell
      ; Node.td
          ~attrs:[ Attr.class_ "sign"; Attr.create "aria-hidden" "true" ]
          [ Node.text sign ]
      ; Node.td ~attrs:[ Attr.class_ "text" ] (marked tokens deps.marks ~inject)
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
    ; quote ~range:c.range ~anchor:c.anchor
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
            ~cls:"keys-hint"
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

let view (m : App.Model.t) ~path ~inject =
  let t = m.review in
  let file = Code_review.file t path in
  let rows = Code_review.rows t ~path in
  let tokens = Tokens.find ~path rows in
  let comments = Review.on t.review path in
  let lines_new = lazy (side_lines t ~path rows New) in
  let lines_old = lazy (side_lines t ~path rows Old) in
  let since = Code_review.Layout.equal (Code_review.layout_of t path) Since in
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
     the change removed: comments on those show above the file. *)
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
  let elsewhere =
    List.filter_map located ~f:(fun (c, l) ->
      match l with
      | At r when shown_at r -> None
      | At _ -> Some (c, false)
      | Outdated -> Some (c, true))
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
  let body =
    List.concat
      (List.map2_exn rows tokens ~f:(fun (row : Row.t) tok ->
         let marks =
           List.filter_map at_row ~f:(fun (c, r) ->
             Option.map (Code_selection.covered r row) ~f:(fun (start, end_) ->
               { Mark.start
               ; end_
               ; id = Some c.id
               ; hot = Option.exists t.hover ~f:(String.equal c.id)
               }))
           @ Option.to_list
               (Option.bind draft ~f:(fun d ->
                  Option.map
                    (Code_selection.covered d.range row)
                    ~f:(fun (start, end_) ->
                      { Mark.start; end_; id = None; hot = false })))
         in
         let selected =
           Option.bind line_selection ~f:(fun r -> Code_selection.covered r row)
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
           List.filter_map at_row ~f:(fun (c, r) ->
             Option.some_if
               (ends_at row r)
               (insert (thread m c ~at:r ~outdated:false ~inject)))
         in
         let bar =
           match selection, draft with
           | Some ({ from = Some _; _ } as s), None when ends_at row s.range ->
             [ comment_bar s ~inject ]
           | _ -> []
         in
         let draft_row =
           match draft with
           | Some d when ends_at row d.range ->
             [ insert (draft_view m d ~inject) ]
           | Some _ | None -> []
         in
         (node :: threads) @ bar @ draft_row))
  in
  let empty =
    match rows, file, Map.find t.contents path with
    | _ when since -> [ Review_since_view.note m path ]
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
  div
    ~cls:"review-code"
    ~attrs:[ Attr.create "data-path" path ]
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
       ; (match selection, draft with
          | Some { pop = Some p; _ }, None -> pop p ~inject
          | _ -> Node.none)
       ])
;;
