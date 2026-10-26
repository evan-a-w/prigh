open! Core
open! Import
open Html

let git action = App.Action.Git action

let files (m : App.Model.t) =
  List.map m.review.files ~f:(fun f ->
    ( f.path
    , Prigh_ui.File_approval.Status.equal
        (Code_review.approval m.review f)
        Approved ))
;;

let plural n word = sprintf "%d %s%s" n word (if n = 1 then "" else "s")

(* A branch reviewed without a checkout has nothing to stage or commit. *)
let read_only (m : App.Model.t) = Option.is_some (Code_review.read_only m.review)

let strip (m : App.Model.t) ~inject =
  let t = m.git_bar in
  match t.status, t.error with
  | _ when read_only m -> Node.none
  | None, None -> Node.none
  | None, Some e ->
    div
      ~cls:"git-bar failed"
      ~attrs:[ Attr.role "status" ]
      [ span ~cls:"git-error" ("No git actions here: " ^ e) ]
  | Some s, _ ->
    let staged, unstaged, untracked = Git_status.counts s in
    let busy = Option.is_some t.busy in
    let branch =
      match s.branch with
      | Some b -> span ~cls:"git-branch" b
      | None -> span ~cls:"git-branch detached" "detached HEAD"
    in
    let upstream =
      match s.upstream with
      | None -> span ~cls:"git-upstream none" "not pushed yet"
      | Some u ->
        Node.span
          ~attrs:
            [ Attr.class_ "git-upstream"
            ; Attr.title
                (sprintf
                   "%s ahead of %s, %s behind"
                   (plural s.ahead "commit")
                   u
                   (plural s.behind "commit"))
            ]
          [ Node.textf "↑%d ↓%d %s" s.ahead s.behind u ]
    in
    let count n what =
      Node.span
        ~attrs:[ classes [ "git-count" ] [ "none", n = 0 ] ]
        [ Node.span ~attrs:[ Attr.class_ "n" ] [ Node.text (Int.to_string n) ]
        ; Node.text (" " ^ what)
        ]
    in
    let counts =
      div
        ~cls:"git-counts"
        [ count staged "staged"
        ; count unstaged "unstaged"
        ; count untracked "untracked"
        ]
    in
    let approved_unstaged =
      List.count (files m) ~f:(fun (path, approved) ->
        approved
        && Option.exists (Git_status.find s path) ~f:Git_status.File.unstaged)
    in
    let push_label =
      match s.upstream with
      | None -> "Push"
      | Some _ when s.ahead > 0 -> sprintf "Push ↑%d" s.ahead
      | Some _ -> "Push"
    in
    div
      ~cls:"git-bar"
      ~attrs:[ Attr.role "group"; Attr.create "aria-label" "Git" ]
      [ icon Branch
      ; branch
      ; upstream
      ; counts
      ; (match t.busy with
         | Some Staging -> lamp ~title:"Staging" ()
         | Some Committing -> lamp ~title:"Committing" ()
         | Some Pushing -> lamp ~title:"Pushing" ()
         | None -> Node.none)
      ; div
          ~cls:"git-actions"
          [ button
              ~cls:"small"
              ~disabled:(busy || approved_unstaged = 0)
              ~title:"Stage the approved files' changes"
              ~on_click:(inject (git Stage_approved))
              [ icon Check; Node.text "Stage approved" ]
          ; button
              ~cls:
                (if s.ahead > 0 || Option.is_none s.upstream
                 then "small primary"
                 else "small")
              ~disabled:
                (busy || Option.is_none s.branch || Option.is_none s.head)
              ~title:
                "Push the branch with the tool host's git credentials (its \
                 upstream is set when it has none)"
              ~on_click:(inject (git Push))
              [ icon ~cls:"git-up" Arrow_down; Node.text push_label ]
          ; button
              ~cls:"small"
              ~disabled:(Option.is_none s.branch)
              ~title:"Open a pull request for this branch (/pr new)"
              ~on_click:(inject (git Create_pr))
              [ icon Pull_request; Node.text "Create PR" ]
          ]
      ; (* What the last action did, or why it was refused: here, where it
           shows whatever the page lists. *)
        (match t.failure, t.notice with
         | Some e, _ ->
           div ~cls:"git-failure" ~attrs:[ Attr.role "alert" ] [ Node.text e ]
         | None, Some n ->
           div ~cls:"git-notice" ~attrs:[ Attr.role "status" ] [ Node.text n ]
         | None, None -> Node.none)
      ]
;;

let tick (m : App.Model.t) path ~inject =
  let t = m.git_bar in
  match Git_bar.tick t ~files:(files m) path with
  | _ when read_only m -> Node.none
  | Clean -> span ~cls:"git-tick clean" ""
  | (Checked | Partly | Unchecked) as tick ->
    let checked =
      match tick with
      | Checked -> "true"
      | Partly -> "mixed"
      | Unchecked | Clean -> "false"
    in
    let staged =
      Option.exists
        (Option.bind t.status ~f:(fun s -> Git_status.find s path))
        ~f:Git_status.File.staged
    in
    Node.button
      ~attrs:
        [ classes
            [ "git-tick" ]
            [ "on", Git_bar.Tick.equal tick Checked
            ; "partly", Git_bar.Tick.equal tick Partly
            ; "pending", Git_bar.Tick.equal tick Checked && not staged
            ]
        ; Attr.type_ "button"
        ; Attr.role "checkbox"
        ; Attr.create "aria-checked" checked
        ; Attr.create "aria-label" ("Commit " ^ path)
        ; Attr.title
            (match tick with
             | Checked when staged -> "Staged: untick to unstage it"
             | Checked ->
               "Approved: staged when you commit (untick to leave it out)"
             | Partly -> "Partly staged: tick to stage all of it"
             | Unchecked | Clean -> "Not staged: tick to stage it")
        ; (if Option.is_some t.busy then Attr.disabled else Attr.empty)
        ; Attr.on_click (fun _ -> inject (git (Toggle path)))
        ]
      [ check (not (Git_bar.Tick.equal tick Unchecked)) ]
;;

let commit_key
      (m : App.Model.t)
      ~inject
      (ev : Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t)
  =
  let key =
    Js_of_ocaml.Js.Optdef.case ev##.key (fun () -> "") Js_of_ocaml.Js.to_string
  in
  let code =
    Js_of_ocaml.Js.Optdef.case ev##.code (fun () -> "") Js_of_ocaml.Js.to_string
  in
  let ctrl =
    Js_of_ocaml.Js.to_bool ev##.ctrlKey || Js_of_ocaml.Js.to_bool ev##.metaKey
  in
  let alt = Js_of_ocaml.Js.to_bool ev##.altKey in
  let action : App.Action.t option =
    match key with
    | "Enter" when ctrl -> Some (git Commit)
    | "Escape" when Generate.running_for m.generate Commit_message ->
      Some Generate_stop
    | _ when alt && (not ctrl) && String.equal code "KeyG" ->
      Some (Generate Commit_message)
    | _ -> None
  in
  match action with
  | Some action ->
    ev##preventDefault;
    Js_of_ocaml.Dom_html.stopPropagation ev;
    inject action
  | None -> Effect.Ignore
;;

let commit_box (m : App.Model.t) ~inject =
  let t = m.git_bar in
  match t.status with
  | _ when read_only m -> Node.none
  | None -> Node.none
  | Some _ ->
    let count = Git_bar.to_commit t ~files:(files m) in
    let committing = Option.equal Git_bar.Busy.equal t.busy (Some Committing) in
    div
      ~cls:"git-commit"
      [ div
          ~cls:"field-head"
          [ Node.label
              ~attrs:[ Attr.for_ "git-message"; Attr.class_ "field-label" ]
              [ Node.text "Commit message" ]
          ; Generate_view.button m.generate Commit_message ~inject
          ]
      ; Node.textarea
          ~attrs:
            [ Attr.id "git-message"
            ; Attr.class_ "text-input git-message"
            ; Attr.create "aria-label" "Commit message"
            ; Attr.placeholder
                (if t.amend
                 then "Empty: keep the commit's message"
                 else "What the change does, and why")
            ; Attr.rows 4
            ; Attr.string_property "value" t.message
            ; Attr.on_input (fun _ text -> inject (git (Message text)))
            ; Attr.on_keydown (commit_key m ~inject)
            ]
          []
      ; div
          ~cls:"git-commit-row"
          [ Node.button
              ~attrs:
                [ Attr.class_ "pr-toggle git-amend"
                ; Attr.type_ "button"
                ; Attr.role "checkbox"
                ; Attr.create "aria-checked" (Bool.to_string t.amend)
                ; Attr.title "Change the last commit instead of making one"
                ; Attr.on_click (fun _ -> inject (git Toggle_amend))
                ]
              [ check t.amend; Node.text "Amend" ]
          ; button
              ~cls:"small primary git-commit-button"
              ~disabled:(Option.is_some t.busy)
              ~title:"Commit the ticked files (Ctrl+Enter)"
              ~attrs:
                (if committing then [ Attr.create "aria-busy" "true" ] else [])
              ~on_click:(inject (git Commit))
              [ Node.text
                  (if committing
                   then "Committing…"
                   else if t.amend
                   then "Amend commit"
                   else (
                     match count with
                     | 0 -> "Commit"
                     | n -> sprintf "Commit %s" (plural n "file")))
              ]
          ]
      ]
;;
