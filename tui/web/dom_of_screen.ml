open! Core
open Virtual_dom
module Content = Prigh_ui.Content
module Style = Prigh_ui.Style
module Text_width = Prigh_ui.Text_width

let color_class (c : Style.Color.t) =
  match c with
  | Default -> None
  | Red -> Some "fg-red"
  | Green -> Some "fg-green"
  | Yellow -> Some "fg-yellow"
  | Blue -> Some "fg-blue"
  | Magenta -> Some "fg-magenta"
  | Cyan -> Some "fg-cyan"
  | Gray -> Some "fg-gray"
  | White -> Some "fg-white"
;;

let classes (s : Style.t) =
  List.filter_opt
    [ color_class s.fg
    ; Option.some_if s.bold "bold"
    ; Option.some_if s.dim "dim"
    ; Option.some_if s.italic "italic"
    ; Option.some_if s.underline "underline"
    ; Option.some_if s.invert "invert"
    ; Option.some_if s.strike "strike"
    ]
;;

let span ?(extra = []) (s : Content.Span.t) =
  let classes = classes s.style @ extra in
  let attrs =
    if List.is_empty classes then [] else [ Vdom.Attr.classes classes ]
  in
  match s.style.link with
  | Some url ->
    Vdom.Node.a
      ~attrs:
        (Vdom.Attr.href url
         :: Vdom.Attr.create "target" "_blank"
         :: Vdom.Attr.create "rel" "noopener"
         :: attrs)
      [ Vdom.Node.text s.text ]
  | None -> Vdom.Node.span ~attrs [ Vdom.Node.text s.text ]
;;

(* Splits the line at [col] so the cell there gets its own span. *)
let with_cursor (line : Content.Line.t) ~col =
  let rec go spans at acc =
    match spans with
    | [] ->
      let pad = col - at in
      let filler =
        if pad > 0
        then
          [ Content.Span.{ text = String.make pad ' '; style = Style.plain } ]
        else []
      in
      List.rev acc @ filler @ [ { text = " "; style = Style.plain } ]
      |> fun spans -> spans, List.length spans - 1
    | (s : Content.Span.t) :: rest ->
      let width = Text_width.string s.text in
      if col >= at + width
      then go rest (at + width) (s :: acc)
      else (
        (* The cursor is inside this span: before / cell / after. *)
        let before, from_cursor = Text_width.take s.text ~width:(col - at) in
        let cell, after =
          match Text_width.uchars from_cursor with
          | [] -> " ", ""
          | (u, _) :: _ -> u, String.drop_prefix from_cursor (String.length u)
        in
        let pieces =
          List.filter
            [ Some { s with text = before }
            ; Some { s with text = cell }
            ; Some { s with text = after }
            ]
            ~f:(function
              | Some { Content.Span.text = ""; _ } -> false
              | _ -> true)
          |> List.filter_opt
        in
        let cursor_index =
          List.length acc + if String.is_empty before then 0 else 1
        in
        List.rev acc @ pieces @ rest, cursor_index)
  in
  go line 0 []
;;

let line ?cursor_col (l : Content.Line.t) =
  let spans =
    match cursor_col with
    | None -> List.map l ~f:(fun s -> span s)
    | Some col ->
      let spans, cursor_index = with_cursor l ~col in
      List.mapi spans ~f:(fun i s ->
        if i = cursor_index then span ~extra:[ "cursor" ] s else span s)
  in
  let spans = if List.is_empty spans then [ Vdom.Node.text " " ] else spans in
  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "line" ] spans
;;

let screen (s : Prigh_ui.Screen.t) =
  let rows = List.length s.lines in
  let lines =
    List.mapi s.lines ~f:(fun row l ->
      match s.cursor with
      | Some (r, col) when r = row -> line ~cursor_col:col l
      | _ -> line l)
  in
  (* Below the cursor row nothing is drawn, so a cursor past the last line still
     needs a row. *)
  let lines =
    match s.cursor with
    | Some (r, col) when r >= rows -> lines @ [ line ~cursor_col:col [] ]
    | _ -> lines
  in
  Vdom.Node.pre ~attrs:[ Vdom.Attr.class_ "screen" ] lines
;;
