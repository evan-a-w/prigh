open! Core
open! Import
open App.Model

let gray = Style.fg Gray
let dim = Style.dim Style.plain
let span ?(style = Style.plain) text = { Content.Span.text; style }

let spinner_frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let format_tokens = App.format_tokens

let agent_symbol (m : App.Model.t) (a : Agent_view.t) =
  match a.status with
  | Agent_view.Status.Running ->
    spinner_frames.(m.spinner % Array.length spinner_frames)
  | Agent_view.Status.Done _ -> "✓"
  | Agent_view.Status.Failed _ -> "✗"
;;

(* The status line is quiet gray; yellow marks what wants attention (the mode
   you are in, running work, warm context) and red what is wrong. *)
let attention = Style.fg Yellow

let format_cwd (m : App.Model.t) (s : P.State.t) =
  let cwd =
    match s.git_branch with
    | Some branch -> sprintf "%s (%s)" (App.tilde m s.cwd) branch
    | None -> App.tilde m s.cwd
  in
  match s.session_name with
  | Some name -> sprintf "%s %S" cwd name
  | None -> cwd
;;

let level_style : Loop_status.Level.t -> Style.t = function
  | Normal -> gray
  | Warn -> attention
  | Over -> Style.fg Red
;;

let context_style percent =
  if percent >= 80
  then Style.fg Red
  else if percent >= 50
  then attention
  else gray
;;

(* [ctx:0.2%/1.0M] like pi: a tenth of a percent shows early growth in a
   large window. *)
let context_text (s : P.State.t) =
  let window = s.model.context_window in
  if window > 0
  then (
    let percent =
      100. *. Float.of_int s.context_tokens /. Float.of_int window
    in
    sprintf
      "ctx:%s%%/%s"
      (if Float.( < ) percent 10.
       then sprintf "%.1f" percent
       else sprintf "%.0f" (Float.round_down percent))
      (format_tokens window))
  else "ctx:" ^ format_tokens s.context_tokens
;;

let agent_marker text ~active =
  let text = if active then "[" ^ text ^ "]" else text in
  if active
  then [ span ~style:(Style.invert (Style.bold Style.plain)) text ]
  else [ span ~style:gray text ]
;;

(** The M3 agent strip, folded into the status line as [agents:[main] 1⠋ 2✓]. *)
let agents_part (m : App.Model.t) : Content.Line.t option =
  let background = (not (App.Model.running m)) && App.Model.agents_running m in
  if List.is_empty m.agents
  then (
    (* Running agents whose events this client missed (e.g. it reconnected). *)
    let running =
      Option.value_map m.state ~default:0 ~f:(fun s ->
        List.count s.subagents ~f:(fun a -> a.running))
    in
    if running = 0
    then None
    else
      Some
        [ span ~style:(Style.bold Style.plain) "agents:"
        ; span (sprintf "%d running" running)
        ])
  else (
    let main =
      agent_marker
        "main"
        ~active:
          (match m.focus with
           | `Main -> true
           | `Agent _ -> false)
    in
    let is_focused id =
      match m.focus with
      | `Agent fid -> String.equal id fid
      | `Main -> false
    in
    let agents =
      List.mapi m.agents ~f:(fun i (a : Agent_view.t) ->
        agent_marker
          (sprintf "%d%s" (i + 1) (agent_symbol m a))
          ~active:(is_focused a.id))
    in
    Some
      ([ span ~style:(Style.bold Style.plain) "agents:" ]
       @ main
       @ List.concat_map agents ~f:(fun a -> span " " :: a)
       @ if background then [ span ~style:(Style.fg Yellow) " bg" ] else []))
;;

let jobs_part (m : App.Model.t) : Content.Line.t option =
  let running =
    Option.value_map m.state ~default:0 ~f:(fun s ->
      List.count s.jobs ~f:(fun (j : P.State.Job.t) -> j.running))
  in
  if running = 0
  then None
  else (
    let idle = not (App.Model.running m) in
    Some
      ([ span ~style:(Style.bold Style.plain) "jobs:"
       ; span (Int.to_string running)
       ]
       @
       if idle
       then
         [ span
             ~style:(Style.fg Yellow)
             (" " ^ spinner_frames.(m.spinner % Array.length spinner_frames))
         ]
       else []))
;;

(* A hint of keys ("A · B · C") cut to [width] by dropping whole keys from
   the end, the least important; cut short only when the first does not
   fit. *)
let fit_keys text ~width =
  let keys =
    String.Search_pattern.split_on (String.Search_pattern.create " · ") text
  in
  let rec take acc = function
    | [] -> acc
    | key :: rest ->
      let next = if String.is_empty acc then key else acc ^ " · " ^ key in
      if Text_width.string next <= width then take next rest else acc
  in
  match take "" keys with
  | "" -> Text_width.truncate text ~width
  | fitted -> fitted
;;

(* Picker keys beyond the status line's, on the title row. *)
let picker_keys (kind : Mode.Picker_kind.t) =
  match kind with
  | Sessions _ ->
    Some "Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named"
  | _ -> None
;;

let mode_hint (m : App.Model.t) : Content.Line.t option =
  match m.connection with
  | Reconnecting { attempt; delay_ms; _ } ->
    Some
      [ span
          ~style:(Style.fg Red)
          (sprintf
             "backend gone · retry %d in %gs"
             attempt
             (Float.of_int delay_ms /. 1000.))
      ]
  | Connected ->
    (match m.state with
     | None -> None
     | Some s ->
       (match m.mode with
        | Picker { kind = Scoped_models; _ } ->
          Some
            [ span
                ~style:attention
                "Space toggle · Ctrl+A all · Ctrl+X none · Enter save"
            ]
        | Picker { kind = Models _; _ } ->
          Some
            [ span
                ~style:attention
                "Enter selects · Esc closes · Ctrl+N logged-in only"
            ]
        | Picker { kind = Sessions selection; picker } ->
          let marked =
            match Session_selection.marked_summary selection picker with
            | Some marked -> marked ^ " · "
            | None -> ""
          in
          Some
            [ span
                ~style:attention
                (marked
                 ^ "Enter switches · Tab marks · Ctrl+D deletes · Esc closes")
            ]
        | Picker { kind = Agents; _ } ->
          Some
            [ span
                ~style:attention
                "Enter focuses · Esc closes · Ctrl+D cancels"
            ]
        | Picker { kind = Jobs; _ } ->
          Some
            [ span
                ~style:attention
                "Enter shows output · Esc closes · Ctrl+D kills"
            ]
        | Picker { kind = Queue; _ } ->
          Some
            [ span ~style:attention "Enter edits · Esc closes · Ctrl+D removes"
            ]
        | Picker { kind = Skills; _ } ->
          Some
            [ span
                ~style:attention
                "Enter puts /skill:NAME in the editor · Esc closes"
            ]
        | Picker { kind = Mcp _; _ } ->
          Some
            [ span
                ~style:attention
                "Enter approves a server or lists its tools · Esc closes"
            ]
        | Picker _ ->
          Some [ span ~style:attention "Enter selects · Esc closes" ]
        | Login_prompt _ ->
          Some [ span ~style:attention "login: Enter answers · Esc cancels" ]
        | Text_prompt { action = Deny_reason _; _ } ->
          Some [ span ~style:attention "Enter denies · Esc goes back" ]
        | Text_prompt _ ->
          (match m.autocomplete with
           | Some ac when Autocomplete.accepts_on_enter ac ->
             Some [ span ~style:attention "Tab/Enter accept · Alt+S as typed" ]
           | Some _ ->
             Some
               [ span
                   ~style:attention
                   "Tab completes · Enter submits · Esc hides"
               ]
           | None ->
             Some [ span ~style:attention "Enter submits · Esc cancels" ])
        | Confirm { action = Tool_confirm c; _ } ->
          Some
            [ span
                ~style:attention
                ("confirm: "
                 ^ String.concat
                     ~sep:"/"
                     (List.map (Confirm_prompt.answers c) ~f:(fun (_, key, _) ->
                        key)))
            ]
        | Confirm _ -> Some [ span ~style:attention "confirm: y / n" ]
        | Question _ ->
          Some [ span ~style:attention "Enter accepts · Esc declines" ]
        | Search { query = _; matches; current } ->
          if List.is_empty matches
          then
            Some [ span ~style:attention "search: type to find · Esc closes" ]
          else
            Some
              [ span
                  ~style:attention
                  (sprintf
                     "search %d/%d · ↓↑ next/prev · Esc closes"
                     (current + 1)
                     (List.length matches))
              ]
        | Editing ->
          (match m.autocomplete with
           | Some ac when Autocomplete.accepts_on_enter ac ->
             Some [ span ~style:attention "Tab/Enter accept · Alt+S as typed" ]
           | Some _ ->
             Some
               [ span
                   ~style:attention
                   "Tab accepts · Enter as typed · Esc hides"
               ]
           | None ->
             if m.pending_quit
             then Some [ span ~style:attention "Ctrl+C again quits" ]
             else if Option.is_some m.btw
             then
               Some
                 [ span
                     ~style:attention
                     (if s.running
                      then
                        spinner_frames.(m.spinner % Array.length spinner_frames)
                        ^ " working · Esc closes btw · Enter steers"
                      else "Esc dismisses btw")
                 ]
             else if s.running
             then
               Some
                 [ span
                     ~style:attention
                     (spinner_frames.(m.spinner % Array.length spinner_frames)
                      ^ " working · Esc aborts · Enter steers")
                 ]
             else None)))
;;

let drop_left_text s ~width =
  let rec go remaining pieces =
    match pieces with
    | [] -> []
    | (p, w) :: rest ->
      if remaining >= w then go (remaining - w) rest else (p, w) :: rest
  in
  String.concat (List.map (go width (Text_width.uchars s)) ~f:fst)
;;

let rec drop_left_spans spans to_drop acc =
  match spans with
  | [] -> List.rev acc
  | (s : Content.Span.t) :: rest ->
    let sw = Text_width.string s.text in
    if sw <= to_drop
    then drop_left_spans rest (to_drop - sw) acc
    else (
      let text = drop_left_text s.text ~width:to_drop in
      List.rev_append acc ({ s with text } :: rest))
;;

let truncate_line_left line ~width =
  let w = Content.Line.width line in
  if w <= width
  then line
  else (
    let keep = Int.max 0 (width - 1) in
    span "…" :: drop_left_spans line (w - keep) [])
;;

(* [tools:laptop] when tools run on another host, [tools:offline] (error style)
   when the active host is gone. Nothing when they run on the backend or here,
   the two ordinary cases. *)
let tools_part (m : App.Model.t) (s : P.State.t) =
  let is_me id =
    Option.value_map m.client_id ~default:false ~f:(String.equal id)
  in
  match List.find s.hosts ~f:(fun h -> String.equal h.id s.active_host) with
  | None when List.is_empty s.hosts -> []
  | None -> [ 2, [ span ~style:(Style.fg Red) "tools:offline" ] ]
  | Some h when String.equal h.id P.Host.backend_id || is_me h.id -> []
  | Some h -> [ 4, [ span ~style:attention ("tools:" ^ h.name) ] ]
;;

(* Always shown, like the thinking level; plan mode in yellow. A mode that
   asks first or refuses outlasts [auto] at narrow widths. *)
let mode_part (s : P.State.t) =
  let mode = Session_mode.of_state s in
  let style = if mode.plan then attention else gray in
  let priority =
    if (not mode.plan) && P.Approval_mode.equal mode.approval Auto then 4 else 3
  in
  [ priority, [ span ~style ("mode:" ^ Session_mode.label mode) ] ]
;;

let plan_part (s : P.State.t) =
  match Option.bind s.plan ~f:Plan_view.status with
  | None -> []
  | Some text -> [ 3, [ span ~style:gray text ] ]
;;

let confirm_dialog ~width (c : P.Pending.Confirm.t) =
  let inner = Boxed.inner_width ~width in
  let question =
    span ~style:(Style.bold (Style.fg Yellow)) (Confirm_prompt.title c)
  in
  let plan =
    if Confirm_prompt.is_plan c
    then
      Content.wrap
        (List.map (String.split_lines c.summary) ~f:Content.Line.of_string)
        ~width:inner
      @ [ [] ]
    else []
  in
  let answers =
    List.map (Confirm_prompt.answers c) ~f:(fun (_, key, label) ->
      [ span ~style:(Style.bold Style.plain) key
      ; span ~style:gray ("  " ^ label)
      ])
  in
  Boxed.render
    ~title:(if Confirm_prompt.is_plan c then "Plan" else "Confirm")
    ~width
    (Content.wrap [ [ question ] ] ~width:inner @ plan @ answers)
;;

(* The keys of a question, most important first, wrapped in the box. *)
let question_keys ~(q : P.Question.t) ~total =
  let has_choices = not (List.is_empty q.choices) in
  List.filter_opt
    [ Option.some_if has_choices "↑↓ choose"
    ; Option.some_if q.multiple "Space toggles"
    ; Some (if has_choices then "type your own" else "type an answer")
    ; Some
        (if q.multiple
         then "Enter accepts"
         else if has_choices
         then "Enter picks"
         else "Enter answers")
    ; Option.some_if (total > 1) "←→ questions"
    ; Some "Esc declines (the model is told)"
    ]
;;

(* The dialog and where the cursor goes in it (the own answer's end). *)
let question_dialog ~width form =
  let inner = Boxed.inner_width ~width in
  let questions = Question_form.questions form in
  let index = Question_form.current form in
  let q : P.Question.t = List.nth_exn questions index in
  let total = List.length questions in
  let title =
    match q.header with
    | Some header when total > 1 ->
      sprintf "%s (%d/%d)" header (index + 1) total
    | Some header -> header
    | None when total > 1 -> sprintf "Question %d/%d" (index + 1) total
    | None -> "Question"
  in
  let wrapped =
    Content.wrap
      [ [ span ~style:(Style.bold (Style.fg Yellow)) q.question ] ]
      ~width:inner
  in
  let pointer row = if Question_form.cursor form = row then "› " else "  " in
  (* Long descriptions and answers wrap, indented under the label. *)
  let chunks text ~width =
    let rec go text acc =
      let head, rest = Text_width.take text ~width:(Int.max 1 width) in
      if String.is_empty rest
      then List.rev (head :: acc)
      else go rest (head :: acc)
    in
    go text []
  in
  let indent = "      " in
  let choices =
    List.mapi q.choices ~f:(fun i (c : P.Question.Choice.t) ->
      let picked = Question_form.is_picked form ~question:index ~choice:i in
      let mark =
        match q.multiple, picked with
        | true, true -> "[x] "
        | true, false -> "[ ] "
        | false, true -> "(•) "
        | false, false -> "( ) "
      in
      let line =
        [ span ~style:attention (pointer i)
        ; span mark
        ; span
            ~style:(if picked then Style.bold Style.plain else Style.plain)
            c.label
        ]
      in
      match c.description with
      | None -> [ line ]
      | Some d
        when Content.Line.width line + 2 + Text_width.string d <= inner ->
        [ line @ [ span ~style:gray ("  " ^ d) ] ]
      | Some d ->
        line
        :: List.map
             (Content.wrap
                [ [ span ~style:gray d ] ]
                ~width:(inner - String.length indent))
             ~f:(fun row -> span indent :: row))
  in
  let choice_rows = List.concat choices in
  let own_row = List.length q.choices in
  let own_prefix =
    pointer own_row ^ if List.is_empty q.choices then "" else "Other: "
  in
  let own = Question_form.own_answer form ~question:index in
  let own_indent = String.make (Text_width.string own_prefix) ' ' in
  let own_lines =
    List.mapi
      (chunks own ~width:(inner - Text_width.string own_prefix))
      ~f:(fun i chunk ->
        [ span ~style:attention (if i = 0 then own_prefix else own_indent)
        ; span chunk
        ])
  in
  let hint =
    (* Whole keys to a row. *)
    List.fold (question_keys ~q ~total) ~init:[] ~f:(fun rows key ->
      match rows with
      | row :: rest when Text_width.string (row ^ " · " ^ key) <= inner ->
        (row ^ " · " ^ key) :: rest
      | rows -> key :: rows)
    |> List.rev_map ~f:(fun row -> [ span ~style:gray row ])
  in
  let body = wrapped @ choice_rows @ own_lines @ hint in
  let cursor =
    if Question_form.on_own_answer form
    then (
      let last = List.last_exn own_lines in
      ( List.length wrapped + List.length choice_rows + List.length own_lines
      , 2 + Content.Line.width last ))
    else (
      (* after the label, not over the pointer *)
      let row = Question_form.cursor form in
      let label = (List.nth_exn q.choices row).label in
      let above =
        List.sum (module Int) (List.take choices row) ~f:List.length
      in
      1 + List.length wrapped + above, 2 + 2 + 4 + Text_width.string label)
  in
  Boxed.render ~title ~width body, cursor
;;

let status (m : App.Model.t) : Content.Line.t =
  match m.state with
  | None -> [ span ~style:gray "connecting…" ]
  | Some s ->
    let max_width = Int.max 1 m.width in
    let base = gray in
    let context =
      if s.model.context_window > 0
      then 100 * s.context_tokens / s.model.context_window
      else 0
    in
    (* Each part after the model carries a priority: at narrow widths the mode
       hint and context survive before think, the mode and the cost, those
       before view. Display order is fixed. *)
    let cwd = [ span ~style:base (format_cwd m s) ] in
    let model = [ span ~style:base s.model.id ] in
    let after_model =
      [ ( 4
        , [ span
              ~style:base
              (sprintf
                 "think:%s"
                 (if s.model.supports_thinking then s.thinking else "n/a"))
          ] )
      ]
      @ mode_part s
      @ [ 7, [ span ~style:base ("view:" ^ Verbosity.name m.verbosity) ]
        ; 2, [ span ~style:(context_style context) (context_text s) ]
        ; ( 4
          , match Loop_status.budget_part s with
            | None -> [ span ~style:base (sprintf "$%.2f" s.cost_usd) ]
            | Some (text, level) -> [ span ~style:(level_style level) text ] )
        ]
      @ Option.value_map s.goal ~default:[] ~f:(fun goal ->
        let text, level = Loop_status.goal_part goal in
        [ 3, [ span ~style:(level_style level) text ] ])
      @ Option.value_map s.resume_at ~default:[] ~f:(fun at ->
        [ ( 2
          , [ span
                ~style:attention
                (Loop_status.resume_part at ~utc_offset:m.utc_offset)
            ] )
        ])
      @ Option.value_map m.namespace ~default:[] ~f:(fun namespace ->
        let text =
          match App.Model.acting_as m, m.user with
          | Some _, Some user -> sprintf "user:%s as %s" user namespace
          | _ -> "user:" ^ namespace
        in
        [ 6, [ span ~style:base text ] ])
      @ tools_part m s
      @ plan_part s
      @ (let queued = List.length m.queued_messages in
         if queued > 0
         then [ 3, [ span ~style:base (sprintf "queued:%d" queued) ] ]
         else [])
      (* Confirmations and questions waiting behind a dialog: before
         anything but the model. *)
      @ (let waiting =
           List.length m.pending_confirms + List.length m.pending_questions
         in
         if waiting > 0
         then [ 0, [ span ~style:attention (sprintf "to answer:%d" waiting) ] ]
         else [])
      @ Option.to_list (Option.map (agents_part m) ~f:(fun p -> 3, p))
      @ Option.to_list (Option.map (jobs_part m) ~f:(fun p -> 3, p))
      @ (match m.viewport with
         | Viewport.Anchored { new_lines; _ } when new_lines > 0 ->
           [ 1, [ span ~style:attention (sprintf "↓ %d new" new_lines) ] ]
         | Viewport.Anchored _ -> [ 1, [ span ~style:attention "↑ scrolled" ] ]
         | Viewport.Follow -> [])
      @ Option.to_list (Option.map (mode_hint m) ~f:(fun p -> 0, p))
    in
    let hint_index =
      match List.last after_model with
      | Some (0, _) -> List.length after_model - 1
      | _ -> -1
    in
    let cwd_width = Content.Line.width cwd in
    let model_width = Content.Line.width model in
    (* Keep parts by priority until the width is exhausted, then restore the
       display order. *)
    let fill ?(max_prio = Int.max_value) used0 =
      (* A mode hint too wide to fit whole is cut short after the context
         (priority 2) has its place, rather than hidden. *)
      let indexed =
        List.mapi after_model ~f:(fun i (prio, part) ->
          let whole = used0 + 2 + Content.Line.width part <= max_width in
          if prio = 0 && not whole then 2, i, part else prio, i, part)
        |> List.filter ~f:(fun (prio, _, _) -> prio <= max_prio)
      in
      let by_priority =
        List.sort indexed ~compare:(fun (p1, i1, _) (p2, i2, _) ->
          match Int.compare p1 p2 with
          | 0 -> Int.compare i1 i2
          | c -> c)
      in
      let kept, used =
        List.fold
          by_priority
          ~init:([], used0)
          ~f:(fun (kept, used) (_, i, part) ->
            let w = Content.Line.width part in
            let room = max_width - used - 2 in
            if w <= room
            then (i, part) :: kept, used + 2 + w
            else if i = hint_index && room >= 12
            then (
              let part =
                match part with
                | [ (hint : Content.Span.t) ] ->
                  [ { hint with text = fit_keys hint.text ~width:room } ]
                | part -> Content.Line.truncate part ~width:room
              in
              (i, part) :: kept, max_width)
            else kept, used)
      in
      ( List.map
          (List.sort kept ~compare:(fun (i1, _) (i2, _) -> Int.compare i1 i2))
          ~f:snd
      , used )
    in
    let fits (_, used) = used + 2 + cwd_width <= max_width in
    (* The view (priority 7) gives way to the directory. *)
    let right, cwd_fits =
      match
        List.find [ fill model_width; fill ~max_prio:6 model_width ] ~f:fits
      with
      | Some (right, _) -> right, true
      | None -> fst (fill (model_width + 1)), false
    in
    let line =
      (if cwd_fits then cwd @ [ span "  " ] else [ span "…" ])
      @ model
      @ List.concat_map right ~f:(fun part -> span "  " :: part)
    in
    truncate_line_left line ~width:max_width
;;

(* Editor lines hard-wrapped after a two-column marker, returning the rows and
   the cursor position relative to the first row. Collapsed paste chips render
   as a single dim line while the cursor is elsewhere. *)
let editor_rows ?(width = 0) (m : App.Model.t) ~marker ~marker_style ~mask
  : Content.t * (int * int)
  =
  let width = if width > 0 then width else m.width in
  let inner = Int.max 1 (width - 2) in
  let pos = Editor.position m.editor in
  let lines = Editor.lines m.editor in
  let chips = Editor.chips m.editor in
  let rows = ref [] in
  let cursor = ref (0, 2) in
  let emitted = ref 0 in
  let prefix i j =
    if i = 0 && j = 0 then span ~style:marker_style marker else span "  "
  in
  let render_line i line =
    let line =
      if mask
      then String.make (List.length (Text_width.uchars line)) '*'
      else line
    in
    let chunks =
      let rec go text acc =
        let head, rest = Text_width.take text ~width:inner in
        if String.is_empty rest
        then List.rev (head :: acc)
        else go rest (head :: acc)
      in
      go line []
    in
    let first_row = !emitted in
    List.iteri chunks ~f:(fun j chunk ->
      rows := (prefix i j :: Content.Line.of_string chunk) :: !rows;
      incr emitted);
    if i = pos.line
    then (
      let before =
        String.concat
          (List.take (List.map (Text_width.uchars line) ~f:fst) pos.col)
      in
      let w = Text_width.string before in
      cursor := first_row + (w / inner), 2 + (w % inner))
  in
  let rec go i =
    if i >= List.length lines
    then ()
    else (
      let collapsed =
        List.find chips ~f:(fun (c : Editor.Chip.t) ->
          c.start.line = i && not (Editor.Chip.contains c pos))
      in
      match collapsed with
      | Some chip ->
        let line = List.nth_exn lines i in
        let before =
          String.concat
            (List.take
               (List.map (Text_width.uchars line) ~f:fst)
               chip.start.col)
        in
        let label = sprintf "[%d lines pasted]" (Editor.Chip.lines chip) in
        let shortened =
          if Text_width.string (before ^ label) <= inner
          then before ^ label
          else Text_width.truncate (before ^ label) ~width:inner
        in
        let row = List.length !rows in
        rows
        := (prefix i 0 :: Content.Line.of_string ~style:dim shortened) :: !rows;
        incr emitted;
        (* The cursor sits after the chip when it is past the pasted block. *)
        if pos.line = chip.stop.line && pos.col >= chip.stop.col
        then cursor := row, 2 + Text_width.string shortened;
        go (chip.stop.line + 1)
      | None ->
        render_line i (List.nth_exn lines i);
        go (i + 1))
  in
  go 0;
  List.rev !rows, !cursor
;;

(* The reason for denying a tool call, typed in the box under the call it
   is about; the cursor is in the box. *)
let deny_reason_dialog (m : App.Model.t) ~width ~question c =
  let inner = Boxed.inner_width ~width in
  let call =
    Content.wrap
      [ [ span ~style:(Style.bold (Style.fg Yellow)) (Confirm_prompt.title c) ]
      ]
      ~width:inner
  in
  let rows, (row, col) =
    editor_rows
      m
      ~width:inner
      ~marker:"› "
      ~marker_style:(Style.bold (Style.fg Yellow))
      ~mask:false
  in
  ( Boxed.render ~title:question ~width (call @ rows)
  , (1 + List.length call + row, 2 + col) )
;;

let max_queued_shown = 3

(** The queued steers and follow-ups above the editor, oldest first. *)
let queued_block (m : App.Model.t) : Content.t =
  let total = List.length m.queued_messages in
  if total = 0
  then []
  else (
    let header =
      [ span
          ~style:dim
          (sprintf
             "queued (%d) · Alt+Up: edit the last · /queue: edit or remove"
             total)
      ]
    in
    let shown = List.take m.queued_messages max_queued_shown in
    let rows =
      List.map shown ~f:(fun (q : Queued_message.t) ->
        [ span
            ~style:attention
            (Text_width.pad_right
               ("  " ^ Queued_message.Kind.name q.kind)
               ~width:13)
        ; span ~style:dim (Queued_message.one_line q)
        ])
    in
    let more = total - List.length shown in
    let more =
      if more > 0 && not (List.is_empty shown)
      then [ [ span ~style:dim (sprintf "  +%d more" more) ] ]
      else []
    in
    List.map ((header :: rows) @ more) ~f:(Content.Line.truncate ~width:m.width))
;;

let picker_block ?keys (p : Picker.t) ~width ~height : Content.t * int =
  let picker_rows = App.picker_rows ~height in
  let visible = Picker.visible p in
  let selected = Picker.selected p in
  let start =
    Int.max
      0
      (Int.min
         (selected - (picker_rows / 2))
         (List.length visible - picker_rows))
  in
  let shown = List.take (List.drop visible start) picker_rows in
  let total = List.length visible in
  let title =
    [ span ~style:(Style.bold Style.plain) (Picker.title p)
    ; span
        ~style:dim
        (if total <= picker_rows
         then sprintf "  (%d)" total
         else
           sprintf
             "  (%d–%d of %d)"
             (start + 1)
             (start + List.length shown)
             total)
    ]
  in
  let title =
    match keys with
    | None -> title
    | Some keys ->
      let room = width - Content.Line.width title - 2 in
      if room < 12
      then title
      else title @ [ span ~style:dim ("  " ^ fit_keys keys ~width:room) ]
  in
  let filter =
    [ span ~style:(Style.bold (Style.fg Yellow)) "/ "; span (Picker.query p) ]
  in
  let label_width =
    List.fold shown ~init:0 ~f:(fun acc (item : Picker.Item.t) ->
      Int.max acc (Text_width.string item.label))
    |> Int.min (width / 2)
  in
  let rows =
    List.mapi shown ~f:(fun i (item : Picker.Item.t) ->
      let is_selected = start + i = selected in
      let mark =
        (if is_selected then "▸" else " ")
        ^
        if Picker.multi p
        then if Set.mem (Picker.checked p) item.id then "[x] " else "[ ] "
        else if Set.mem (Picker.checked p) item.id
        then "✓ "
        else if item.marked
        then "* "
        else "  "
      in
      let label = Text_width.pad_right item.label ~width:label_width in
      let label_style =
        let s = if item.dimmed then dim else Style.plain in
        if is_selected then Style.invert (Style.bold s) else s
      in
      let line =
        [ span ~style:label_style (mark ^ label)
        ; span
            ~style:(if is_selected then Style.invert dim else dim)
            (if String.is_empty item.detail then "" else "  " ^ item.detail)
        ]
      in
      let divider =
        let previous =
          if i = 0
          then if start = 0 then None else List.nth visible (start - 1)
          else List.nth shown (i - 1)
        in
        match previous with
        | _ when String.is_empty item.section -> []
        | Some (previous : Picker.Item.t)
          when String.equal previous.section item.section -> []
        | _ -> [ [ span ~style:dim ("  ── " ^ item.section ^ " ──") ] ]
      in
      divider @ [ Content.Line.truncate line ~width ])
    |> List.concat
  in
  let rows =
    if List.is_empty rows then [ [ span ~style:dim "  no matches" ] ] else rows
  in
  title :: filter :: rows, 1
;;

let autocomplete_rows = 8

let autocomplete_block (ac : Autocomplete.t) ~width : Content.t =
  let items = Autocomplete.items ac in
  let selected = Autocomplete.selected ac in
  let total = List.length items in
  let start = Int.max 0 (Int.min (selected - 4) (total - autocomplete_rows)) in
  let shown = List.take (List.drop items start) autocomplete_rows in
  let overflow =
    if total <= autocomplete_rows
    then []
    else
      [ [ span
            ~style:dim
            (sprintf
               "  ↕ %d–%d of %d"
               (start + 1)
               (start + List.length shown)
               total)
        ]
      ]
  in
  (match Autocomplete.source ac with
   | Autocomplete.Source.Command ->
     let usage (item : Picker.Item.t) =
       Option.value_map
         (Commands.find item.id)
         ~default:("/" ^ item.label)
         ~f:Commands.usage
     in
     let usage_width =
       List.fold shown ~init:0 ~f:(fun acc item ->
         Int.max acc (Text_width.string (usage item)))
       |> Int.min (width / 2)
     in
     List.mapi shown ~f:(fun i (item : Picker.Item.t) ->
       let is_selected = start + i = selected in
       let style s = if is_selected then Style.invert s else s in
       let help =
         Option.value_map (Commands.find item.id) ~default:"" ~f:(fun s ->
           s.help)
       in
       let name = "/" ^ item.label in
       let args =
         Text_width.pad_right
           (String.drop_prefix (usage item) (String.length name))
           ~width:(usage_width - Text_width.string name)
       in
       Content.Line.truncate
         [ span ~style:(style Style.plain) (if is_selected then "▸ " else "  ")
         ; span ~style:(style (Style.bold Style.plain)) name
         ; span ~style:(style dim) args
         ; span ~style:(style dim) ("  " ^ help)
         ]
         ~width)
   | Autocomplete.Source.Argument _ | Path | Directory _ | Skill ->
     let label_width =
       List.fold shown ~init:0 ~f:(fun acc (item : Picker.Item.t) ->
         Int.max acc (Text_width.string item.label))
       |> Int.min (width / 2)
     in
     List.mapi shown ~f:(fun i (item : Picker.Item.t) ->
       let is_selected = start + i = selected in
       let style s = if is_selected then Style.invert s else s in
       Content.Line.truncate
         [ span ~style:(style Style.plain) (if is_selected then "▸ " else "  ")
         ; span
             ~style:(style (Style.bold Style.plain))
             (Text_width.pad_right item.label ~width:label_width)
         ; span
             ~style:(style dim)
             (if String.is_empty item.detail then "" else "  " ^ item.detail)
         ]
         ~width))
  @ overflow
;;

let subagent_header (m : App.Model.t) (a : Agent_view.t) : Content.Line.t =
  let total = List.length m.agents in
  let index =
    match List.findi m.agents ~f:(fun _ x -> String.equal x.id a.id) with
    | Some (i, _) -> i + 1
    | None -> 0
  in
  let status =
    match a.status with
    | Agent_view.Status.Running ->
      sprintf
        "%s running %d turns"
        spinner_frames.(m.spinner % Array.length spinner_frames)
        a.turns
    | Agent_view.Status.Done { turns; cost_usd } ->
      sprintf "✓ done %d turns $%.2f" turns cost_usd
    | Agent_view.Status.Failed _ -> "✗ failed"
  in
  let task =
    Text_width.truncate
      (String.concat ~sep:" " (String.split_lines a.task))
      ~width:40
  in
  [ span
      ~style:(Style.bold (Style.fg Cyan))
      (sprintf "◆ subagent %d/%d" index total)
  ; span ~style:dim (sprintf "  %s" a.model)
  ; span ~style:gray (sprintf "  %s" status)
  ; span (sprintf "  \"%s\"" task)
  ]
;;

let screen (m : App.Model.t) : Screen.t =
  let width = Int.max 1 m.width in
  let height = Int.max 3 m.height in
  let separator =
    [ span ~style:gray (String.concat (List.init width ~f:(fun _ -> "─"))) ]
  in
  let status_line = status m in
  let dialog, editor, cursor =
    match m.mode with
    | Picker { picker; kind } ->
      let block, cursor_row =
        picker_block ?keys:(picker_keys kind) picker ~width ~height
      in
      let query_width = Text_width.string (Picker.query picker) in
      block, [], (cursor_row, 2 + query_width)
    | Confirm { action = Tool_confirm c; _ } ->
      let rows, cursor =
        editor_rows
          m
          ~marker:"? "
          ~marker_style:(Style.bold (Style.fg Yellow))
          ~mask:false
      in
      confirm_dialog ~width c, rows, cursor
    | Question { form; _ } ->
      let dialog, cursor = question_dialog ~width form in
      dialog, [], cursor
    | Confirm { question; _ } ->
      let rows, cursor =
        editor_rows
          m
          ~marker:"? "
          ~marker_style:(Style.bold (Style.fg Yellow))
          ~mask:false
      in
      ( Boxed.render
          ~title:"Confirm"
          ~width
          [ [ span ~style:(Style.bold (Style.fg Yellow)) question ] ]
      , rows
      , cursor )
    | Login_prompt { prompt; _ } ->
      let mask =
        match prompt with
        | Secret _ -> true
        | _ -> false
      in
      let rows, cursor =
        editor_rows
          m
          ~marker:"? "
          ~marker_style:(Style.bold (Style.fg Yellow))
          ~mask
      in
      ( Boxed.render
          ~title:"Log in"
          ~width
          (Content.wrap
             (List.map m.login_lines ~f:Content.Line.of_string)
             ~width:(Boxed.inner_width ~width))
      , rows
      , cursor )
    | Text_prompt { question; action = Deny_reason { confirm; _ } } ->
      let dialog, cursor = deny_reason_dialog m ~width ~question confirm in
      dialog, [], cursor
    | Text_prompt { question; _ } ->
      let rows, cursor =
        editor_rows
          m
          ~marker:"? "
          ~marker_style:(Style.bold (Style.fg Yellow))
          ~mask:false
      in
      Boxed.render ~title:question ~width [], rows, cursor
    | Search { query; _ } ->
      let row : Content.Line.t =
        [ span ~style:(Style.bold (Style.fg Yellow)) "/ "; span query ]
      in
      [], [ row ], (0, 2 + Text_width.string query)
    | Editing ->
      let rows, cursor =
        editor_rows
          m
          ~marker:"> "
          ~marker_style:(Style.bold (Style.fg Yellow))
          ~mask:false
      in
      let btw =
        match m.btw with
        | Some box ->
          Btw_box.render box ~width ~max_rows:(Btw_box.max_rows ~height)
        | None -> []
      in
      btw, rows, cursor
  in
  let autocomplete_rows =
    match m.mode, m.autocomplete with
    | (Editing | Text_prompt _), Some ac -> autocomplete_block ac ~width
    | _ -> []
  in
  let panel =
    dialog
    @ [ separator ]
    @ queued_block m
    @ editor
    @ autocomplete_rows
    @ [ status_line ]
  in
  let panel = List.map panel ~f:(Content.Line.truncate ~width) in
  let panel_rows = List.length panel in
  let transcript_area_rows = Int.max 0 (height - panel_rows) in
  let focused =
    match m.focus with
    | `Main -> None
    | `Agent id -> Agent_view.find m.agents id
  in
  let header, body_rows =
    match focused with
    | Some agent ->
      ( Some (Content.Line.truncate (subagent_header m agent) ~width)
      , Int.max 0 (transcript_area_rows - 1) )
    | None -> None, transcript_area_rows
  in
  let transcript_source =
    match focused with
    | Some agent -> agent.transcript
    | None -> m.transcript
  in
  let transcript =
    match m.viewport with
    | Viewport.Follow ->
      Transcript.render_tail
        transcript_source
        ~width
        ~rows:body_rows
        ~skip:0
        ~verbosity:m.verbosity
    | Viewport.Anchored { top; _ } ->
      Transcript.render_window
        transcript_source
        ~width
        ~rows:body_rows
        ~top
        ~verbosity:m.verbosity
  in
  let transcript =
    match m.mode with
    | Search { query; _ } when not (String.is_empty query) ->
      List.map transcript ~f:(Content.Line.highlight ~needle:query)
    | _ -> transcript
  in
  let padding =
    List.init (Int.max 0 (body_rows - List.length transcript)) ~f:(fun _ -> [])
  in
  let transcript = Option.to_list header @ padding @ transcript in
  let lines = List.take (transcript @ panel) height in
  let cursor =
    let dialog_rows = List.length dialog in
    let queued_rows = List.length (queued_block m) in
    let row, col = cursor in
    let base =
      match m.mode with
      | Picker _ | Question _ | Text_prompt { action = Deny_reason _; _ } ->
        transcript_area_rows
      | _ -> transcript_area_rows + dialog_rows + 1 + queued_rows
    in
    Some (base + row, Int.min col (width - 1))
  in
  { lines; cursor; width; height }
;;
