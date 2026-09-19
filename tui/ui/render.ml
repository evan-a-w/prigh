open! Core
module P = Prigh_protocol
open App.Model

let gray = Style.fg Gray
let dim = Style.dim Style.plain
let span ?(style = Style.plain) text = { Content.Span.text; style }

let spinner_frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let format_tokens = App.format_tokens
let picker_rows = 10

let status (m : App.Model.t) : Content.Line.t =
  match m.state with
  | None -> [ span ~style:gray "connecting…" ]
  | Some s ->
    let context =
      if s.model.context_window > 0
      then 100 * s.context_tokens / s.model.context_window
      else 0
    in
    let mode_hint =
      match m.mode with
      | Picker _ -> Some "picker: type to filter, Enter selects, Esc closes"
      | Login_prompt _ -> Some "login: Enter answers, Esc cancels"
      | Confirm _ -> Some "confirm: y / n"
      | Editing ->
        if m.pending_quit
        then Some "Ctrl+C again quits"
        else if s.running
        then
          Some
            (spinner_frames.(m.spinner % Array.length spinner_frames)
             ^ " working (Esc aborts; Enter steers)")
        else None
    in
    let parts =
      [ s.model.key
      ; "thinking:" ^ s.thinking
      ; sprintf "ctx:%s (%d%%)" (format_tokens s.context_tokens) context
      ; sprintf
          "in:%s out:%s"
          (format_tokens s.usage.input)
          (format_tokens s.usage.output)
      ; sprintf "$%.4f" s.cost_usd
      ]
    in
    let queued = Queue_counts.total m.queued in
    let parts =
      if queued > 0 then parts @ [ sprintf "queued:%d" queued ] else parts
    in
    let parts =
      match m.viewport with
      | Viewport.Anchored { new_lines; _ } when new_lines > 0 ->
        parts @ [ sprintf "↓ %d new" new_lines ]
      | Viewport.Follow | Viewport.Anchored _ -> parts
    in
    let parts = parts @ Option.to_list mode_hint in
    let line = String.concat ~sep:"  " parts in
    let style =
      match m.mode with
      | Editing -> gray
      | _ -> Style.fg Yellow
    in
    [ span ~style (Text_width.truncate line ~width:m.width) ]
;;

(* Editor lines hard-wrapped after a two-column marker, returning the rows and
   the cursor position relative to the first row. *)
let editor_rows (m : App.Model.t) ~marker ~marker_style ~mask
  : Content.t * (int * int)
  =
  let inner = Int.max 1 (m.width - 2) in
  let pos = Editor.position m.editor in
  let rows = ref [] in
  let cursor = ref (0, 2) in
  List.iteri (Editor.lines m.editor) ~f:(fun i line ->
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
    let first_row = List.length !rows in
    List.iteri chunks ~f:(fun j chunk ->
      let prefix =
        if i = 0 && j = 0 then span ~style:marker_style marker else span "  "
      in
      rows := (prefix :: Content.Line.of_string chunk) :: !rows);
    if i = pos.line
    then (
      let before =
        String.concat
          (List.take (List.map (Text_width.uchars line) ~f:fst) pos.col)
      in
      let w = Text_width.string before in
      cursor := first_row + (w / inner), 2 + (w % inner)));
  List.rev !rows, !cursor
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
      let mark = if item.marked then "* " else "  " in
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
      [ [ span ~style:(Style.bold (Style.fg Yellow)) question ] ], rows, cursor
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
      [], rows, cursor
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
  let panel = dialog @ [ separator ] @ editor @ [ status_line ] in
  let panel = List.map panel ~f:(Content.Line.truncate ~width) in
  let panel_rows = List.length panel in
  let transcript_rows = Int.max 0 (height - panel_rows) in
  let transcript =
    match m.viewport with
    | Viewport.Follow ->
      Transcript.render_tail
        m.transcript
        ~width
        ~rows:transcript_rows
        ~skip:0
        ~expand_tools:m.expand_tools
    | Viewport.Anchored { top; _ } ->
      Transcript.render_window
        m.transcript
        ~width
        ~rows:transcript_rows
        ~top
        ~expand_tools:m.expand_tools
  in
  let padding =
    List.init
      (Int.max 0 (transcript_rows - List.length transcript))
      ~f:(fun _ -> [])
  in
  let transcript = padding @ transcript in
  let lines = List.take (transcript @ panel) height in
  let cursor =
    let dialog_rows = List.length dialog in
    let row, col = cursor in
    let base =
      match m.mode with
      | Picker _ -> transcript_rows
      | _ -> transcript_rows + dialog_rows + 1
    in
    Some (base + row, Int.min col (width - 1))
  in
  { lines; cursor; width; height }
;;
