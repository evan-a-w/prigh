open! Core
module P = Prigh_protocol
open App.Model

let gray = Style.fg Gray
let dim = Style.dim Style.plain
let span ?(style = Style.plain) text = { Content.Span.text; style }

let spinner_frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let format_tokens = App.format_tokens
let picker_rows = 10

let agent_symbol (m : App.Model.t) (a : Agent_view.t) =
  match a.status with
  | Agent_view.Running ->
    spinner_frames.(m.spinner % Array.length spinner_frames)
  | Agent_view.Done _ -> "✓"
  | Agent_view.Failed _ -> "✗"
;;

let status_style m =
  match m.mode with
  | Editing -> gray
  | _ -> Style.fg Yellow
;;

let format_cwd (m : App.Model.t) (s : P.State.t) =
  let cwd =
    match m.home with
    | Some home when String.equal s.cwd home -> "~"
    | Some home when String.is_prefix s.cwd ~prefix:(home ^ "/") ->
      "~" ^ String.drop_prefix s.cwd (String.length home)
    | _ -> s.cwd
  in
  let cwd =
    match s.git_branch with
    | Some branch -> sprintf "%s (%s)" cwd branch
    | None -> cwd
  in
  match s.session_name with
  | Some name -> sprintf "%s %S" cwd name
  | None -> cwd
;;

let context_style percent =
  if percent >= 80
  then Style.fg Red
  else if percent >= 50
  then Style.fg Yellow
  else Style.fg Green
;;

let agent_marker text ~active =
  let text = if active then "[" ^ text ^ "]" else text in
  if active
  then [ span ~style:(Style.invert (Style.bold Style.plain)) text ]
  else [ span ~style:gray text ]
;;

(** The M3 agent strip, folded into the status line as [agents:[main] 1⠋ 2✓]. *)
let agents_part (m : App.Model.t) : Content.Line.t option =
  if List.is_empty m.agents
  then None
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
       @ List.concat_map agents ~f:(fun a -> span " " :: a)))
;;

let mode_hint (m : App.Model.t) : Content.Line.t option =
  if m.backend_gone
  then Some [ span ~style:dim "backend exited — Ctrl+C or /quit to exit" ]
  else (
    match m.state with
    | None -> None
    | Some s ->
      (match m.mode with
       | Picker { kind = Scoped_models; _ } ->
         Some
           [ span
               ~style:dim
               "Space toggle · Ctrl+A all · Ctrl+X none · Enter save"
           ]
       | Picker { kind = Models _; _ } ->
         Some
           [ span
               ~style:dim
               "picker: type to filter, Enter selects, Esc closes · Ctrl+N \
                logged in only"
           ]
       | Picker { kind = Sessions _; _ } ->
         Some
           [ span
               ~style:dim
               "picker: type to filter, Enter selects, Esc closes · Ctrl+N \
                named only · Ctrl+D delete"
           ]
       | Picker _ ->
         Some
           [ span ~style:dim "picker: type to filter, Enter selects, Esc closes"
           ]
       | Login_prompt _ ->
         Some [ span ~style:dim "login: Enter answers, Esc cancels" ]
       | Text_prompt _ -> Some [ span ~style:dim "Enter submits, Esc cancels" ]
       | Confirm _ -> Some [ span ~style:dim "confirm: y / n" ]
       | Search { query = _; matches; current } ->
         if List.is_empty matches
         then Some [ span ~style:dim "search: type to find · Esc closes" ]
         else
           Some
             [ span
                 ~style:dim
                 (sprintf
                    "search %d/%d · ↓↑ next/prev · Esc closes"
                    (current + 1)
                    (List.length matches))
             ]
       | Editing ->
         if Option.is_some m.autocomplete
         then Some [ span ~style:dim "Tab/Enter accept · Esc close" ]
         else if m.pending_quit
         then Some [ span ~style:dim "Ctrl+C again quits" ]
         else if s.running
         then
           Some
             [ span
                 ~style:dim
                 (spinner_frames.(m.spinner % Array.length spinner_frames)
                  ^ " working (Esc aborts; Enter steers)")
             ]
         else None))
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

let status (m : App.Model.t) : Content.Line.t =
  match m.state with
  | None -> [ span ~style:gray "connecting…" ]
  | Some s ->
    let max_width = Int.max 1 m.width in
    let base = status_style m in
    let context =
      if s.model.context_window > 0
      then 100 * s.context_tokens / s.model.context_window
      else 0
    in
    (* Each part after the model carries a priority: at narrow widths the mode
       hint and context survive before think/view. Display order is fixed. *)
    let cwd = [ span ~style:base (format_cwd m s) ] in
    let model = [ span ~style:base s.model.id ] in
    let after_model =
      [ ( 5
        , [ span
              ~style:base
              (sprintf
                 "think:%s"
                 (if s.model.supports_thinking then s.thinking else "n/a"))
          ] )
      ; 6, [ span ~style:base ("view:" ^ Verbosity.name m.verbosity) ]
      ; ( 2
        , [ span
              ~style:(context_style context)
              (sprintf "ctx:%d%% %s" context (format_tokens s.context_tokens))
          ] )
      ; 4, [ span ~style:base (sprintf "$%.2f" s.cost_usd) ]
      ]
      @ (let queued = Queue_counts.total m.queued in
         if queued > 0
         then [ 3, [ span ~style:base (sprintf "queued:%d" queued) ] ]
         else [])
      @ Option.to_list (Option.map (agents_part m) ~f:(fun p -> 3, p))
      @ (match m.viewport with
         | Viewport.Anchored { new_lines; _ } when new_lines > 0 ->
           [ 1, [ span ~style:base (sprintf "↓ %d new" new_lines) ] ]
         | Viewport.Follow | Viewport.Anchored _ -> [])
      @ Option.to_list (Option.map (mode_hint m) ~f:(fun p -> 0, p))
    in
    let cwd_width = Content.Line.width cwd in
    let model_width = Content.Line.width model in
    (* Keep parts by priority until the width is exhausted, then restore the
       display order. *)
    let fill used0 =
      let indexed =
        List.mapi after_model ~f:(fun i (prio, part) -> prio, i, part)
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
            if used + 2 + w <= max_width
            then (i, part) :: kept, used + 2 + w
            else kept, used)
      in
      ( List.map
          (List.sort kept ~compare:(fun (i1, _) (i2, _) -> Int.compare i1 i2))
          ~f:snd
      , used )
    in
    let right, used = fill model_width in
    let cwd_fits = used + 2 + cwd_width <= max_width in
    let right = if cwd_fits then right else fst (fill (model_width + 1)) in
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
let editor_rows (m : App.Model.t) ~marker ~marker_style ~mask
  : Content.t * (int * int)
  =
  let inner = Int.max 1 (m.width - 2) in
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
        rows
        := (prefix i 0 :: Content.Line.of_string ~style:dim shortened) :: !rows;
        incr emitted;
        go (chip.stop.line + 1)
      | None ->
        render_line i (List.nth_exn lines i);
        go (i + 1))
  in
  go 0;
  List.rev !rows, !cursor
;;

(** The queued steer/follow-up summary above the editor. *)
let queued_block (m : App.Model.t) : Content.t =
  let total = Queue_counts.total m.queued in
  if total = 0
  then []
  else (
    let texts =
      List.map m.queued_texts ~f:(fun text ->
        let one_line = String.concat ~sep:" " (String.split_lines text) in
        Text_width.truncate one_line ~width:24)
    in
    let label =
      if List.is_empty texts
      then sprintf "queued (%d)" total
      else sprintf "queued (%d): %s" total (String.concat ~sep:" ∣ " texts)
    in
    [ Content.Line.truncate [ span ~style:dim label ] ~width:m.width ])
;;

let picker_block (p : Picker.t) ~width : Content.t * int =
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
  let title =
    [ span ~style:(Style.bold (Style.fg Cyan)) (Picker.title p)
    ; span ~style:dim (sprintf "  (%d)" (List.length visible))
    ]
  in
  let filter =
    [ span ~style:(Style.bold (Style.fg Cyan)) "/ "; span (Picker.query p) ]
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
        if Picker.multi p
        then if Set.mem (Picker.checked p) item.id then "[x] " else "[ ] "
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
      Content.Line.truncate line ~width)
  in
  let rows =
    if List.is_empty rows then [ [ span ~style:dim "  no matches" ] ] else rows
  in
  title :: filter :: rows, 1
;;

let autocomplete_block (ac : Autocomplete.t) ~width : Content.t =
  let items = Autocomplete.items ac in
  let selected = Autocomplete.selected ac in
  let start = Int.max 0 (Int.min (selected - 4) (List.length items - 8)) in
  let shown = List.take (List.drop items start) 8 in
  match Autocomplete.source ac with
  | Autocomplete.Source.Command ->
    let usage (item : Picker.Item.t) =
      let args =
        Option.value_map (Commands.find item.id) ~default:"" ~f:(fun s ->
          s.args)
      in
      "/" ^ item.label ^ if String.is_empty args then "" else " " ^ args
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
  | Autocomplete.Source.Argument _ | Autocomplete.Source.Path ->
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
        ~width)
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
    | Agent_view.Running ->
      sprintf
        "%s running %d turns"
        spinner_frames.(m.spinner % Array.length spinner_frames)
        a.turns
    | Agent_view.Done { turns; cost_usd } ->
      sprintf "✓ done %d turns $%.2f" turns cost_usd
    | Agent_view.Failed _ -> "✗ failed"
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

let pad_line_to (line : Content.Line.t) ~width =
  let w = Content.Line.width line in
  if w >= width
  then Content.Line.truncate line ~width
  else
    line
    @ [ { Content.Span.text = String.make (width - w) ' '; style = Style.plain }
      ]
;;

let border = Style.fg Gray

let boxed ~title ~(body : Content.t) ~width : Content.t =
  let inner = Int.max 1 (width - 4) in
  let title = Text_width.truncate title ~width:(Int.max 1 (width - 6)) in
  let title_w = Text_width.string title in
  let fill = Int.max 0 (width - 5 - title_w) in
  let top : Content.Line.t =
    [ span ~style:border "┌─ "
    ; span ~style:(Style.bold Style.plain) title
    ; span ~style:border " "
    ; span ~style:border (String.concat (List.init fill ~f:(fun _ -> "─")))
    ; span ~style:border "┐"
    ]
  in
  let rows =
    List.map body ~f:(fun line ->
      let line =
        pad_line_to (Content.Line.truncate line ~width:inner) ~width:inner
      in
      (span ~style:border "│ " :: line) @ [ span ~style:border " │" ])
  in
  let bottom : Content.Line.t =
    [ span ~style:border "└"
    ; span
        ~style:border
        (String.concat (List.init (Int.max 0 (width - 2)) ~f:(fun _ -> "─")))
    ; span ~style:border "┘"
    ]
  in
  (top :: rows) @ [ bottom ]
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
    | Picker { picker; _ } ->
      let block, cursor_row = picker_block picker ~width in
      let query_width = Text_width.string (Picker.query picker) in
      block, [], (cursor_row, 2 + query_width)
    | Confirm { question; _ } ->
      let rows, cursor =
        editor_rows
          m
          ~marker:"? "
          ~marker_style:(Style.bold (Style.fg Yellow))
          ~mask:false
      in
      ( boxed
          ~title:"Confirm"
          ~body:[ [ span ~style:(Style.bold (Style.fg Yellow)) question ] ]
          ~width
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
      ( boxed
          ~title:"Log in"
          ~body:(List.map m.login_lines ~f:Content.Line.of_string)
          ~width
      , rows
      , cursor )
    | Text_prompt { question; _ } ->
      let rows, cursor =
        editor_rows
          m
          ~marker:"? "
          ~marker_style:(Style.bold (Style.fg Yellow))
          ~mask:false
      in
      boxed ~title:question ~body:[] ~width, rows, cursor
    | Search { query; _ } ->
      let row : Content.Line.t =
        [ span ~style:(Style.bold (Style.fg Cyan)) "/ "; span query ]
      in
      [], [ row ], (0, 2 + Text_width.string query)
    | Editing ->
      let rows, cursor =
        editor_rows
          m
          ~marker:"> "
          ~marker_style:(Style.bold (Style.fg Cyan))
          ~mask:false
      in
      [], rows, cursor
  in
  let autocomplete_rows =
    match m.mode, m.autocomplete with
    | Editing, Some ac -> autocomplete_block ac ~width
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
      | Picker _ -> transcript_area_rows
      | _ -> transcript_area_rows + dialog_rows + 1 + queued_rows
    in
    Some (base + row, Int.min col (width - 1))
  in
  { lines; cursor; width; height }
;;
