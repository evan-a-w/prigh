open! Core
open! Import
open Html
module Diff_files = Prigh_ui.Diff_files
module File_approval = Prigh_ui.File_approval

let review action = App.Action.Review action

let approval_mark (status : File_approval.Status.t) =
  match status with
  | Approved -> mark ~cls:"done" ~label:"Approved" Check
  | Changed -> mark ~cls:"now" ~label:"Changed since you approved it" Warning
  | Unapproved -> mark ~cls:"todo" ~label:"Not approved yet" Ring
;;

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

let open_button ?path (m : App.Model.t) ~inject =
  let unsent = Code_review.unsent m.review in
  button
    ~cls:"small review-open"
    ~title:"Review the changes file by file (Alt+R, /review)"
    ~on_click:(inject (review (Open path)))
    ([ icon Check; Node.text "Review" ]
     @
     if unsent > 0
     then [ span ~cls:"review-count" (Int.to_string unsent) ]
     else [])
;;

let split_path path =
  match String.rsplit2 path ~on:'/' with
  | Some (dir, name) -> dir ^ "/", name
  | None -> "", path
;;

let comment_count (t : Code_review.t) path =
  List.count (Review.on t.review path) ~f:(fun c ->
    Review_comment.Status.equal c.status Open)
;;

let file_row (m : App.Model.t) (f : Diff_files.File.t) ~inject =
  let t = m.review in
  let current = Option.exists t.current ~f:(String.equal f.path) in
  let dir, name = split_path f.path in
  let status = Code_review.approval t f in
  let comments = comment_count t f.path in
  Node.li
    [ Git_bar_view.tick m f.path ~inject
    ; Node.button
        ~attrs:
          [ classes
              [ "review-file" ]
              [ "current", current
              ; "approved", File_approval.Status.equal status Approved
              ; "changed", File_approval.Status.equal status Changed
              ]
          ; Attr.type_ "button"
          ; Attr.create "data-path" f.path
          ; (if current then Attr.create "aria-current" "true" else Attr.empty)
          ; Attr.title
              (sprintf "%s: %s" f.path (File_approval.Status.to_string status))
          ; Attr.on_click (fun _ -> inject (review (Go f.path)))
          ]
        [ approval_mark status
        ; Node.span
            ~attrs:[ Attr.class_ "review-file-path" ]
            [ span ~cls:"dir" dir; span ~cls:"name" name ]
        ; (if comments > 0
           then span ~cls:"review-file-comments" (Int.to_string comments)
           else Node.none)
        ; (match
             Option.bind (Map.find t.since f.path) ~f:File_approval.since_label
           with
           | Some label ->
             span
               ~cls:"review-file-since"
               ~attrs:
                 [ Attr.title ("Changed after your last action: " ^ label) ]
               label
           | None -> Node.none)
        ; span
            ~cls:"review-file-counts"
            (match f.status with
             | Binary -> "bin"
             | Added | Deleted | Modified -> sprintf "+%d −%d" f.added f.removed)
        ]
    ]
;;

let editor (m : App.Model.t) (e : Code_review.Editor.t) ~inject =
  let lines = String.split_lines e.text in
  let rows = Int.max 1 (List.length lines + 1) in
  let cols =
    List.fold lines ~init:40 ~f:(fun acc l -> Int.max acc (String.length l + 2))
  in
  let lang = Highlight.Lang.of_path e.path in
  ignore (m : App.Model.t);
  div
    ~cls:"review-editor"
    [ div
        ~cls:"review-editor-bar"
        [ (if e.saving
           then span ~cls:"review-editor-state" "Saving…"
           else if e.dirty
           then span ~cls:"review-editor-state dirty" "Unsaved changes"
           else span ~cls:"review-editor-state" "Saved on the tool host")
        ; (match e.error with
           | Some error ->
             span ~cls:"review-editor-error" ~attrs:[ Attr.role "alert" ] error
           | None -> Node.none)
        ; div
            ~cls:"review-editor-actions"
            [ button
                ~cls:"small ghost"
                ~title:"Read the file again, dropping your changes"
                ~on_click:(inject (review Reload))
                [ icon Restart; Node.text "Reload" ]
            ; button
                ~cls:"small primary"
                ~title:"Ctrl+S"
                ~disabled:(e.saving || (not e.dirty) || Option.is_none e.md5)
                ~on_click:(inject (review Save))
                [ Node.text "Save" ]
            ]
        ]
    ; div
        ~cls:"review-editor-scroll"
        [ div
            ~cls:"review-editor-stack"
            [ Node.pre
                ~attrs:
                  [ Attr.class_ "review-editor-code"
                  ; Attr.create "aria-hidden" "true"
                  ]
                (Highlight_view.code lang (e.text ^ "\n"))
            ; Node.textarea
                ~attrs:
                  [ Attr.id "review-editor"
                  ; Attr.class_ "review-editor-text"
                  ; Attr.create "spellcheck" "false"
                  ; Attr.create "wrap" "off"
                  ; Attr.create "autocapitalize" "off"
                  ; Attr.create "rows" (Int.to_string rows)
                  ; Attr.create "cols" (Int.to_string cols)
                  ; Attr.create "aria-label" ("Text of " ^ e.path)
                  ; (if Option.is_none e.md5 then Attr.disabled else Attr.empty)
                  ; Attr.string_property "value" e.text
                  ; Attr.on_input (fun _ text ->
                      inject (review (Edit_input text)))
                  ; Attr.on_keydown (fun ev ->
                      let key =
                        Js_of_ocaml.Js.Optdef.case
                          ev##.key
                          (fun () -> "")
                          Js_of_ocaml.Js.to_string
                      in
                      match
                        Code_review.field_key
                          m.review
                          ~field:`Editor
                          ~key
                          ~shift:(Js_of_ocaml.Js.to_bool ev##.shiftKey)
                          ~ctrl:(Js_of_ocaml.Js.to_bool ev##.ctrlKey)
                          ~meta:(Js_of_ocaml.Js.to_bool ev##.metaKey)
                      with
                      | Some action ->
                        ev##preventDefault;
                        inject (review action)
                      | None -> Effect.Ignore)
                  ]
                []
            ]
        ]
    ]
;;

let current_file (m : App.Model.t) path ~inject =
  let t = m.review in
  let file = Code_review.file t path in
  let status = Option.map file ~f:(Code_review.approval t) in
  let head =
    div
      ~cls:"review-file-head"
      [ button
          ~cls:"icon ghost"
          ~title:"Previous file ([, p, k)"
          ~label:"Previous file"
          ~on_click:(inject (review (Step (-1))))
          [ icon ~cls:"flip" Next ]
      ; button
          ~cls:"icon ghost"
          ~title:"Next file (], n, j)"
          ~label:"Next file"
          ~on_click:(inject (review (Step 1)))
          [ icon Next ]
      ; Node.h2
          ~attrs:[ Attr.class_ "review-file-title" ]
          [ Node.text path
          ; (match file with
             | Some { status = Added; _ } -> span ~cls:"chip" "new"
             | Some { status = Deleted; _ } -> span ~cls:"chip" "deleted"
             | Some { status = Binary; _ } -> span ~cls:"chip" "binary"
             | Some { status = Modified; _ } | None -> Node.none)
          ]
      ; (match status with
         | Some Changed ->
           span ~cls:"review-stale" "Changed since you approved it"
         | Some (Approved | Unapproved) | None -> Node.none)
      ; (match status with
         | None -> Node.none
         | Some status ->
           let approved = File_approval.Status.equal status Approved in
           Node.button
             ~attrs:
               [ classes [ "btn"; "small"; "review-approve" ] [ "on", approved ]
               ; Attr.type_ "button"
               ; Attr.create "aria-pressed" (Bool.to_string approved)
               ; Attr.title
                   (if approved
                    then "Take the approval back (a)"
                    else "Approve this file as it is now (a)")
               ; Attr.on_click (fun _ -> inject (review (Toggle_approval path)))
               ]
             [ icon Check
             ; Node.text
                 (match status with
                  | Approved -> "Approved"
                  | Changed -> "Approve again"
                  | Unapproved -> "Approve")
             ])
      ]
  in
  let body =
    match t.mode, t.editor with
    | Edit, Some e when String.equal e.path path -> editor m e ~inject
    | Edit, _ ->
      (match Map.find t.contents path with
       | Some (Failed e) ->
         div ~cls:"review-empty" [ Node.text ("Cannot edit it: " ^ e) ]
       | Some (Loaded { md5 = None; _ }) ->
         div
           ~cls:"review-empty"
           [ Node.text "Too large to edit here: open it in an editor." ]
       | Some (Loading | Loaded _) | None ->
         div ~cls:"review-empty" [ lamp (); Node.text "Reading the file…" ])
    | Comment, _ -> Review_code_view.view m ~path ~inject
  in
  div
    ~cls:"review-current"
    [ head
    ; div ~cls:"review-scroll" ~attrs:[ Attr.id "review-scroll" ] [ body ]
    ]
;;

let empty_state (t : Code_review.t) =
  match t.diff_error, t.diff with
  | Some e, _ ->
    div
      ~cls:"review-empty big"
      ~attrs:[ Attr.role "alert" ]
      [ Node.p [ Node.text e ]
      ; Node.p
          ~attrs:[ Attr.class_ "review-advice" ]
          [ Node.text
              "The review needs a git repository where the tools run: /cd into \
               one."
          ]
      ]
  | None, None ->
    div ~cls:"review-empty big" [ lamp (); Node.text "Reading the changes…" ]
  | None, Some d ->
    div
      ~cls:"review-empty big"
      [ Node.p
          [ Node.text
              (sprintf
                 "No changes in %s against %s."
                 (Diff_files.where d)
                 (Diff_files.against d))
          ]
      ; Node.p
          ~attrs:[ Attr.class_ "review-advice" ]
          [ Node.text "What the agent changes shows here as it works." ]
      ]
;;

let picking (m : App.Model.t) (which : Review_picker.Which.t) =
  match m.dialog with
  | Some (Review_pick p)
    when Review_picker.Which.equal (Review_picker.which p) which -> Some p
  | _ -> None
;;

(* A picker's button, with its menu under it while open. *)
let pick (m : App.Model.t) ~which ~id ~label ~key ~read_only ~inject children =
  let open_ = picking m which in
  div
    ~cls:"review-pick"
    [ Node.button
        ~attrs:
          [ Attr.id id
          ; classes
              [ "review-pick-button" ]
              [ "open", Option.is_some open_; "read-only", read_only ]
          ; Attr.type_ "button"
          ; Attr.create "aria-haspopup" "dialog"
          ; Attr.create "aria-expanded" (Bool.to_string (Option.is_some open_))
          ; (if Option.is_some open_
             then Attr.create "aria-controls" "review-popover"
             else Attr.empty)
          ; Attr.title (sprintf "%s (%s)" label key)
          ; Attr.on_click (fun _ -> inject (App.Action.Review_pick which))
          ]
        (children @ [ icon ~cls:"chevron" Chevron ])
    ; (match open_ with
       | Some p -> Review_picker_view.view p ~inject
       | None -> Node.none)
    ]
;;

let short rev =
  if String.length rev = 40 && String.for_all rev ~f:Char.is_alphanum
  then String.prefix rev 8
  else rev
;;

(* What the page reviews and what it is compared with, each a picker:
   "Reviewing feat/x (/path) against main (merge base 1234abcd)", since the
   merge base or directly; a branch not checked out is read-only, with a
   way to check it out. *)
let target_bar (m : App.Model.t) ~inject =
  let t = m.review in
  match t.diff with
  | None ->
    (match t.target.reviewed with
     | Dir (Some dir) ->
       div
         ~cls:"review-target"
         [ span ~cls:"review-target-text" ("Reviewing " ^ dir) ]
     | Rev rev ->
       div
         ~cls:"review-target"
         [ span ~cls:"review-target-text" ("Reviewing " ^ rev) ]
     | Dir None -> Node.none)
  | Some d ->
    let read_only = Option.is_some d.rev in
    let reviewed =
      match d.rev with
      | Some rev ->
        [ icon ~cls:"review-pick-icon" Branch
        ; span ~cls:"review-pick-name" rev
        ; span ~cls:"review-pick-where" "not checked out"
        ]
      | None ->
        [ span
            ~cls:"review-pick-name"
            (Option.value d.branch ~default:(Filename.basename d.root))
        ; span ~cls:"review-pick-where" d.root
        ]
    in
    let base =
      if String.equal d.base "HEAD"
      then
        [ span ~cls:"review-pick-name" "HEAD"
        ; span ~cls:"review-pick-where" "the uncommitted changes"
        ]
      else [ span ~cls:"review-pick-name" (short d.base) ]
    in
    let how =
      match Diff_files.against d with
      | "HEAD" -> Node.none
      | _ when d.direct -> span ~cls:"review-merge-base" "directly"
      | against when String.equal against d.base -> Node.none
      | against ->
        (match String.chop_prefix against ~prefix:(d.base ^ " ") with
         | Some rest -> span ~cls:"review-merge-base" rest
         | None -> span ~cls:"review-merge-base" against)
    in
    let compare =
      if String.equal d.base "HEAD"
      then Node.none
      else
        segmented
          ~label:"Compare"
          [ ( "Since merge base"
            , "What the branch changed since it left the base, as a pull \
               request shows it (d)"
            , not d.direct
            , inject (App.Action.Review_direct false) )
          ; ( "Directly"
            , "Every difference from the base as it is now, its own changes \
               too (d)"
            , d.direct
            , inject (App.Action.Review_direct true) )
          ]
    in
    let checkout =
      match d.rev with
      | None -> Node.none
      | Some rev ->
        div
          ~cls:"review-readonly"
          [ span
              ~cls:"review-readonly-note"
              ~attrs:
                [ Attr.title
                    (sprintf
                       "%s is not checked out: you can comment, not edit or \
                        accept proposed changes"
                       rev)
                ]
              "Read-only"
          ; button
              ~cls:"small review-checkout"
              ~title:
                "Check the branch out into a new worktree of this session's, \
                 and review it there"
              ~on_click:(inject App.Action.Review_checkout)
              [ icon Branch_plus; Node.text "Check out into a worktree" ]
          ]
    in
    div
      ~cls:"review-target"
      ~attrs:
        [ Attr.title
            (sprintf
               "Reviewing %s against %s"
               (Diff_files.where d)
               (Diff_files.against d))
        ]
      [ span ~cls:"review-target-text" "Reviewing"
      ; pick
          m
          ~which:Target
          ~id:"review-target-button"
          ~label:"What to review: a worktree, or a branch read-only"
          ~key:"r"
          ~read_only
          ~inject
          reviewed
      ; span ~cls:"review-target-text" "against"
      ; pick
          m
          ~which:Base
          ~id:"review-base-button"
          ~label:"What to compare it with: a branch, tag or commit"
          ~key:"b"
          ~read_only:false
          ~inject
          base
      ; how
      ; compare
      ; checkout
      ]
;;

let view (m : App.Model.t) ~inject =
  let t = m.review in
  if not t.open_
  then Node.none
  else (
    let total = List.length t.files in
    let approved = Code_review.approved t in
    let unsent = Code_review.unsent t in
    let head =
      Node.header
        ~attrs:[ Attr.class_ "review-head" ]
        [ Node.h1 ~attrs:[ Attr.class_ "review-title" ] [ Node.text "Review" ]
        ; div
            ~cls:"review-progress"
            ~attrs:
              [ Attr.role "progressbar"
              ; Attr.create "aria-label" "Files approved"
              ; Attr.create "aria-valuemin" "0"
              ; Attr.create "aria-valuemax" (Int.to_string total)
              ; Attr.create "aria-valuenow" (Int.to_string approved)
              ]
            [ Node.div
                ~attrs:
                  [ Attr.class_ "review-strip"
                  ; Attr.create "aria-hidden" "true"
                  ]
                (List.map t.files ~f:(fun f ->
                   Node.span
                     ~attrs:
                       [ classes
                           []
                           [ ( "approved"
                             , File_approval.Status.equal
                                 (Code_review.approval t f)
                                 Approved )
                           ; ( "changed"
                             , File_approval.Status.equal
                                 (Code_review.approval t f)
                                 Changed )
                           ; ( "current"
                             , Option.exists t.current ~f:(String.equal f.path)
                             )
                           ]
                       ; Attr.title f.path
                       ]
                     []))
            ; span
                ~cls:"review-progress-text"
                (sprintf "%d/%d approved" approved total)
            ]
        ; (if t.loading
           then lamp ~title:"Reading the changes" ()
           else
             button
               ~cls:"icon ghost review-refresh"
               ~title:"Read the changes again (they also update as prigh edits)"
               ~label:"Refresh"
               ~on_click:(inject (review Refresh))
               [ icon Restart ])
        ; div
            ~cls:"review-controls"
            [ segmented
                ~label:"Mode"
                [ ( "Comment"
                  , "Select code and comment on it (e)"
                  , Code_review.Mode.equal t.mode Comment
                  , inject (review (Set_mode Comment)) )
                ; ( "Edit"
                  , (match Code_review.read_only t with
                     | Some rev ->
                       sprintf
                         "%s is not checked out: Check out into a worktree to \
                          edit it"
                         rev
                     | None ->
                       "Edit the file and save it where the tools run (e)")
                  , Code_review.Mode.equal t.mode Edit
                  , inject (review (Set_mode Edit)) )
                ]
            ; (let layout =
                 Option.value_map
                   t.current
                   ~default:t.layout
                   ~f:(Code_review.layout_of t)
               in
               segmented
                 ~label:"Show"
                 ([ ( "Changes"
                    , "Only the changed hunks (v)"
                    , Code_review.Layout.equal layout Unified
                    , inject (review (Set_layout Unified)) )
                  ; ( "Whole file"
                    , "The whole file with its changes inline (v)"
                    , Code_review.Layout.equal layout Full
                    , inject (review (Set_layout Full)) )
                  ]
                  @ Option.to_list
                      (Option.bind t.current ~f:(fun path ->
                         Review_since_view.segment m path ~inject))))
            ; Option.value_map t.current ~default:Node.none ~f:(fun path ->
                Review_since_view.menu m path ~inject)
            ; button
                ~cls:(if unsent > 0 then "small primary" else "small")
                ~disabled:(unsent = 0 || t.submitting)
                ~title:"Send the new comments and replies to prigh (s)"
                ~on_click:(inject (review Submit))
                [ icon Send
                ; Node.text
                    (match unsent with
                     | 0 -> "Send comments"
                     | 1 -> "Send 1 comment"
                     | n -> sprintf "Send %d comments" n)
                ]
            ; button
                ~cls:"icon ghost"
                ~title:"Close the review (Esc, Alt+R)"
                ~label:"Close the review"
                ~on_click:(inject (review Close))
                [ icon Close ]
            ]
        ]
    in
    let body =
      match t.files, t.current with
      | [], _ | _, None -> empty_state t
      | files, Some path ->
        div
          ~cls:"review-body"
          [ Node.nav
              ~attrs:
                [ Attr.class_ "review-files"
                ; Attr.create "aria-label" "Changed files"
                ]
              [ Node.ul (List.map files ~f:(file_row m ~inject))
              ; div
                  ~cls:"review-files-foot"
                  [ Node.text (Diff_files.summary files) ]
              ; Git_bar_view.commit_box m ~inject
              ]
          ; Resize_view.handle m ~inject Review_files
          ; current_file m path ~inject
          ]
    in
    Node.create
      "section"
      ~attrs:
        [ Attr.class_ "review-page"
        ; Attr.id "review-page"
        ; Attr.tabindex (-1)
        ; Attr.create "aria-label" "Review of the changes"
        ]
      (* While a picker is open the rest of the page is out of reach. *)
      (match m.dialog with
       | Some (Review_pick _) ->
         [ inert head
         ; target_bar m ~inject
         ; inert (Git_bar_view.strip m ~inject)
         ; inert body
         ]
       | _ -> [ head; target_bar m ~inject; Git_bar_view.strip m ~inject; body ]))
;;
