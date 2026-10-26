open! Core
open! Import
open Html
module Action = App.Action
module Filter = Pull_requests.Filter

let keys_hint text = span ~cls:"keys-hint" text

let link ?(cls = "") ~href children =
  Node.a
    ~attrs:
      [ Attr.href href
      ; Attr.target "_blank"
      ; Attr.create "rel" "noopener noreferrer"
      ; Attr.class_ cls
      ]
    children
;;

let state_badge (pr : Pull_request.t) =
  let label = Pull_requests.state_label pr in
  span ~cls:("pr-state " ^ String.lowercase label) label
;;

let failure e =
  div ~cls:"dialog-error" ~attrs:[ Attr.role "alert" ] [ Node.text e ]
;;

let waiting text =
  div
    ~cls:"loading-row"
    ~attrs:[ Attr.role "status" ]
    [ Html.lamp (); Node.text text ]
;;

let which (filter : Filter.t) =
  match filter with
  | Open -> "open "
  | Closed -> "closed "
  | All -> ""
;;

let filters (l : Pull_requests.t) ~inject =
  Node.div
    ~attrs:
      [ Attr.class_ "segmented"
      ; Attr.role "group"
      ; Attr.create "aria-label" "Show"
      ]
    (List.map Filter.all ~f:(fun f ->
       let on = Filter.equal f l.filter in
       Node.button
         ~attrs:
           [ classes [ "segment" ] [ "on", on ]
           ; Attr.type_ "button"
           ; Attr.create "aria-pressed" (Bool.to_string on)
           ; Attr.title ("/pr list " ^ Filter.to_string f)
           ; Attr.on_click (fun _ -> inject (Action.Pr_filter f))
           ]
         [ Node.text (String.capitalize (Filter.to_string f)) ]))
;;

let rows (l : Pull_requests.t) ~inject =
  let p = l.picker in
  match Picker.visible p, l.status with
  | _, Loading ->
    waiting (sprintf "Loading the %spull requests…" (which l.filter))
  | _, Failed e -> failure e
  | [], Loaded repo when List.is_empty l.prs ->
    div
      ~cls:"picker-empty"
      [ Node.textf
          "No %spull requests in %s. New pull request opens one from this \
           branch."
          (which l.filter)
          (Forge_repo.slug repo)
      ]
  | [], Loaded _ ->
    div
      ~cls:"picker-empty"
      [ Node.textf
          "Nothing matches “%s”: Backspace widens the search."
          (Picker.query p)
      ]
  | visible, Loaded _ ->
    div
      ~cls:"picker-items pr-items"
      ~attrs:
        [ Attr.role "listbox"
        ; Attr.id "picker-list"
        ; Attr.create "aria-label" "Pull requests"
        ]
      (List.mapi visible ~f:(fun i (item : Picker.Item.t) ->
         let selected = i = Picker.selected p in
         Node.div
           ~attrs:
             [ classes [ "picker-item"; "pr-item" ] [ "selected", selected ]
             ; Attr.role "option"
             ; Attr.id (sprintf "picker-option-%d" i)
             ; Attr.create "aria-selected" (Bool.to_string selected)
             ; Attr.on_click (fun _ -> inject (Action.Picker_choose item.id))
             ]
           [ (match
                Option.bind
                  (Int.of_string_opt item.id)
                  ~f:(Pull_requests.find l)
              with
              | Some pr -> state_badge pr
              | None -> Node.none)
           ; span ~cls:"picker-label" item.label
           ; span ~cls:"picker-detail" item.detail
           ]))
;;

let list (l : Pull_requests.t) ~inject =
  let p = l.picker in
  let loaded =
    match l.status with
    | Loaded _ -> true
    | Loading | Failed _ -> false
  in
  Modal.view
    ~cls:"picker-dialog prs-dialog"
    ~title:"Pull requests"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_hint "↑↓ to move, Enter to open, Esc to close"
      ; button
          ~title:"/pr new"
          ~on_click:(inject Action.New_pr)
          [ icon Plus; Node.text "New pull request" ]
      ]
    [ div
        ~cls:"prs-head"
        [ (match l.status with
           | Loaded repo ->
             link
               ~cls:"prs-repo"
               ~href:repo.url
               [ Node.text (Forge_repo.slug repo) ]
           | Loading | Failed _ -> Node.none)
        ; filters l ~inject
        ]
    ; div
        ~cls:"search"
        [ icon Search
        ; Node.input
            ~attrs:
              [ Attr.id "picker-input"
              ; Attr.type_ "text"
              ; Attr.create "aria-label" "Filter pull requests"
              ; Attr.role "combobox"
              ; Attr.create "aria-controls" "picker-list"
              ; Attr.create "aria-expanded" "true"
              ; Attr.create "aria-autocomplete" "list"
              ; (if loaded && not (List.is_empty (Picker.visible p))
                 then
                   Attr.create
                     "aria-activedescendant"
                     (sprintf "picker-option-%d" (Picker.selected p))
                 else Attr.empty)
              ; Attr.placeholder "Filter by title, number, author or branch…"
              ; Attr.value (Picker.query p)
              ; Attr.create "autocomplete" "off"
              ; Attr.create "spellcheck" "false"
              ; Attr.on_input (fun _ text -> inject (Action.Picker_query text))
              ]
            ()
        ]
    ; rows l ~inject
    ]
;;

(* ---- one pull request ---------------------------------------------------- *)

let check_mark state =
  match state with
  | "success" -> Html.mark ~cls:"done" ~label:"passed" Check
  | "failure" | "error" -> Html.mark ~cls:"failed" ~label:"failed" Cross
  | "pending" -> Html.lamp ~title:"running" ()
  | _ -> Html.mark ~cls:"stopped" ~label:state Dash
;;

let checks (c : Pr_checks.t option) =
  let summary (c : Pr_checks.t) =
    let count state =
      List.count c.items ~f:(fun (i : Pr_checks.Item.t) ->
        String.equal i.state state)
    in
    List.filter_map
      [ "success", "passed"; "failure", "failed"; "pending", "running" ]
      ~f:(fun (state, word) ->
        match count state with
        | 0 -> None
        | n -> Some (sprintf "%d %s" n word))
    |> String.concat ~sep:", "
  in
  Node.section
    ~attrs:[ Attr.class_ "pr-section pr-checks" ]
    [ Node.h3
        [ Node.text "Checks"
        ; (match c with
           | Some ({ items = _ :: _; _ } as c) ->
             span ~cls:"pr-count" (summary c)
           | _ -> Node.none)
        ]
    ; (match c with
       | None -> Node.p [ Node.text "The checks could not be read." ]
       | Some { items = []; _ } ->
         Node.p [ Node.text "No checks ran on this commit." ]
       | Some { items; _ } ->
         Node.ul
           (List.map items ~f:(fun (i : Pr_checks.Item.t) ->
              Node.li
                ~attrs:[ Attr.class_ ("pr-check " ^ i.state) ]
                [ check_mark i.state
                ; (match i.url with
                   | Some href -> link ~href [ Node.text i.name ]
                   | None -> span ~cls:"pr-check-name" i.name)
                ; span ~cls:"pr-check-state" i.state
                ])))
    ]
;;

let checkout_note (p : Pr_page.t) ~inject =
  match p.checkout with
  | Idle -> Node.none
  | Busy -> waiting (sprintf "Checking out #%d…" p.number)
  | Failed e -> failure e
  | Done c ->
    div
      ~cls:"pr-checkout"
      ~attrs:[ Attr.role "status" ]
      [ Node.p
          [ Node.text
              (if c.created
               then "Checked out in a new worktree: "
               else "Brought up to date in its worktree: ")
          ; span ~cls:"path" c.path
          ; Node.textf " (branch %s)." c.branch
          ]
      ; button
          ~cls:"small primary"
          ~title:("/cd " ^ c.path)
          ~on_click:(inject (Action.Run ("/cd " ^ c.path)))
          [ Node.text "Work there" ]
      ]
;;

let detail (m : App.Model.t) (p : Pr_page.t) (d : Pr_detail.t) ~inject =
  let pr = d.pr in
  let diff = Pr_detail.git_diff d in
  let files =
    Git_diff_view.files
      ~fold:(App.Model.fold m)
      ~scope:(sprintf "pr-%d" pr.number)
      diff
  in
  [ div
      ~cls:"pr-meta"
      [ state_badge pr
      ; span ~cls:"pr-author" pr.author
      ; Node.span
          ~attrs:[ Attr.class_ "pr-branches" ]
          [ span ~cls:"pr-branch" pr.head
          ; Node.text " → "
          ; span ~cls:"pr-branch" pr.base
          ]
      ; span ~cls:"pr-repo" (Forge_repo.slug d.repo)
      ]
  ; checkout_note p ~inject
  ; checks d.checks
  ; Node.section
      ~attrs:[ Attr.class_ "pr-section pr-body" ]
      [ Node.h3 [ Node.text "Description" ]
      ; (if String.is_empty (String.strip pr.body)
         then
           Node.p
             ~attrs:[ Attr.class_ "pr-none" ]
             [ Node.text "No description." ]
         else Markdown_view.render pr.body)
      ]
  ; Node.section
      ~attrs:[ Attr.class_ "pr-section pr-diff" ]
      ([ Node.h3
           [ Node.text "Changes"
           ; span
               ~cls:"pr-count"
               (Prigh_ui.Diff_files.summary (Prigh_ui.Diff_files.parse d.diff))
           ]
       ]
       @ (match files with
          | [] ->
            [ Node.p
                ~attrs:[ Attr.class_ "pr-none" ]
                [ Node.text "No changes." ]
            ]
          | files -> files)
       @
       if d.truncated
       then
         [ Node.p
             ~attrs:[ Attr.class_ "dialog-note warn" ]
             [ Node.textf
                 "The diff is cut short: Open on %s shows all of it."
                 (Pr_page.forge_label p)
             ]
         ]
       else [])
  ]
;;

let page (m : App.Model.t) (p : Pr_page.t) ~inject =
  let pr = Pr_page.pull_request p in
  let title =
    match pr with
    | Some pr -> sprintf "#%d %s" pr.number pr.title
    | None -> sprintf "Pull request #%d" p.number
  in
  let busy =
    match p.checkout with
    | Busy -> true
    | Idle | Done _ | Failed _ -> false
  in
  Modal.view
    ~cls:"pr-dialog"
    ~title
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      ((match p.back with
        | Some _ ->
          [ button
              ~cls:"ghost pr-back"
              ~title:"Esc"
              ~on_click:(inject Action.Close_dialog)
              [ icon Back; Node.text "All pull requests" ]
          ]
        | None -> [ keys_hint "Esc closes" ])
       @ (match pr with
          | Some pr ->
            [ link
                ~cls:"btn link-button"
                ~href:pr.url
                [ Node.textf "Open on %s" (Pr_page.forge_label p)
                ; icon External
                ]
            ]
          | None -> [])
       @ [ button
             ~title:(sprintf "/pr checkout %d" p.number)
             ~disabled:busy
             ~on_click:(inject (Action.Pr_checkout p.number))
             [ icon Branch; Node.text "Check out" ]
         ; button
             ~cls:"primary"
             ~on_click:(inject Action.Dialog_accept)
             [ Node.text "Done" ]
         ])
    (match p.status with
     | Loading -> [ waiting (sprintf "Loading pull request #%d…" p.number) ]
     | Failed e -> [ failure e ]
     | Loaded d -> detail m p d ~inject)
;;

(* ---- creating one -------------------------------------------------------- *)

let field ~id ~label control =
  div
    ~cls:"pr-field"
    [ Node.label ~attrs:[ Attr.for_ id ] [ Node.text label ]; control ]
;;

let toggle ~on ~label ~action ~inject =
  Node.button
    ~attrs:
      [ Attr.class_ "pr-toggle"
      ; Attr.type_ "button"
      ; Attr.role "checkbox"
      ; Attr.create "aria-checked" (Bool.to_string on)
      ; Attr.on_click (fun _ -> inject (Action.Pr_form action))
      ]
    [ Html.check on; Node.text label ]
;;

let create (generate : Generate.t) (f : Pr_create.t) ~inject =
  let input ~id ~label ~value ~placeholder action =
    Node.input
      ~attrs:
        [ Attr.id id
        ; Attr.class_ "text-input"
        ; Attr.type_ "text"
        ; Attr.create "aria-label" label
        ; Attr.placeholder placeholder
        ; Attr.value value
        ; Attr.create "autocomplete" "off"
        ; Attr.create "spellcheck" "false"
        ; (if f.busy then Attr.disabled else Attr.empty)
        ; Attr.on_input (fun _ text -> inject (Action.Pr_form (action text)))
        ]
      ()
  in
  let form (d : Pr_defaults.t) =
    [ div
        ~cls:"pr-into"
        [ span ~cls:"pr-branch" d.head
        ; Node.text " into "
        ; input
            ~id:"pr-base"
            ~label:"Base branch"
            ~value:f.base
            ~placeholder:d.base
            (fun s -> Pr_create.Action.Base s)
        ; span ~cls:"pr-repo" (Forge_repo.slug d.repo)
        ]
    ; div
        ~cls:"pr-generate"
        [ Generate_view.button
            ~label:"Generate title and description"
            generate
            Pr
            ~inject
        ]
    ; field
        ~id:"pr-title"
        ~label:"Title"
        (input
           ~id:"pr-title"
           ~label:"Title"
           ~value:f.title
           ~placeholder:"What it does"
           (fun s -> Pr_create.Action.Title s))
    ; field
        ~id:"pr-body"
        ~label:"Description (Markdown)"
        (Node.textarea
           ~attrs:
             [ Attr.id "pr-body"
             ; Attr.class_ "text-input pr-body-input"
             ; Attr.create "aria-label" "Description"
             ; Attr.value f.body
             ; Attr.rows 8
             ; (if f.busy then Attr.disabled else Attr.empty)
             ; Attr.on_input (fun _ text -> inject (Action.Pr_form (Body text)))
             ]
           [])
    ; (match d.commits with
       | [] -> Node.none
       | commits ->
         Node.details
           ~attrs:[ Attr.class_ "pr-commits" ]
           [ Node.summary
               [ Node.textf
                   "%d commit%s"
                   (List.length commits)
                   (if List.length commits = 1 then "" else "s")
               ]
           ; Node.ul (List.map commits ~f:(fun c -> Node.li [ Node.text c ]))
           ])
    ; div
        ~cls:"pr-toggles"
        [ toggle
            ~on:f.draft
            ~label:"Open it as a draft"
            ~action:Toggle_draft
            ~inject
        ; toggle
            ~on:f.push
            ~label:(sprintf "Push %s first" d.head)
            ~action:Toggle_push
            ~inject
        ]
    ; (if d.pushed
       then Node.none
       else
         div
           ~cls:"dialog-note warn"
           [ Node.textf
               "%s has commits the remote doesn't: push it first."
               d.head
           ])
    ]
  in
  let footer =
    match f.failed with
    | Some _ ->
      [ button
          ~cls:"primary"
          ~on_click:(inject Action.Close_dialog)
          [ Node.text "Close" ]
      ]
    | None ->
      [ keys_hint "Ctrl+Enter creates it"
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"primary"
          ~disabled:(f.busy || Option.is_none f.defaults)
          ~attrs:(if f.busy then [ Attr.create "aria-busy" "true" ] else [])
          ~on_click:(inject Action.Dialog_accept)
          (if f.busy
           then [ Html.lamp (); Node.text "Creating…" ]
           else [ Node.text "Create pull request" ])
      ]
  in
  Modal.view
    ~cls:"pr-create-dialog"
    ~title:"New pull request"
    ~on_close:(inject Action.Close_dialog)
    ~footer
    (match f.failed, f.defaults with
     | Some e, _ -> [ failure e ]
     | None, None -> [ waiting "Reading the branch…" ]
     | None, Some d ->
       form d
       @ [ (match f.error with
            | Some e -> failure e
            | None -> Node.none)
         ])
;;

let checked_out ~number (c : Pr_checkout.t) ~inject =
  Modal.view
    ~title:(sprintf "Checked out #%d" number)
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ button ~on_click:(inject Action.Close_dialog) [ Node.text "Stay here" ]
      ; button
          ~cls:"primary"
          ~title:("/cd " ^ c.path)
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text "Work there" ]
      ]
    [ Node.p
        [ Node.textf
            "Pull request #%d is %s "
            number
            (if c.created
             then "in a new worktree at"
             else "up to date in its worktree at")
        ; span ~cls:"path" c.path
        ; Node.textf ", on branch %s." c.branch
        ]
    ; Node.p [ Node.text "Work there? The session's tools then run in it." ]
    ]
;;
