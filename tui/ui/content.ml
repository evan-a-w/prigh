open! Core

module Span = struct
  type t =
    { text : string
    ; style : Style.t
    }
  [@@deriving sexp_of, equal]

  let width t = Text_width.string t.text
end

module Line = struct
  type t = Span.t list [@@deriving sexp_of, equal]

  let of_string ?(style = Style.plain) text : t =
    if String.is_empty text then [] else [ { Span.text; style } ]
  ;;

  let width t = List.sum (module Int) t ~f:Span.width

  module Line_width = struct
    let width = width
  end

  let to_plain t = String.concat (List.map t ~f:(fun s -> s.Span.text))

  (* Splits spans into words (keeping trailing spaces attached) so that wrapping
     can prefer word boundaries. *)
  let words (t : t) : Span.t list =
    List.concat_map t ~f:(fun span ->
      let pieces = ref [] in
      let buf = Buffer.create 16 in
      let flush () =
        if Buffer.length buf > 0
        then (
          pieces := { span with text = Buffer.contents buf } :: !pieces;
          Buffer.clear buf)
      in
      String.iter span.text ~f:(fun c ->
        Buffer.add_char buf c;
        if Char.equal c ' ' then flush ());
      flush ();
      List.rev !pieces)
  ;;

  let hard_split (span : Span.t) ~width : Span.t list =
    let rec go text acc =
      if String.is_empty text
      then List.rev acc
      else (
        let head, rest = Text_width.take text ~width in
        if String.is_empty head
        then List.rev ({ span with text } :: acc)
        else go rest ({ span with text = head } :: acc))
    in
    go span.text []
  ;;

  let wrap (t : t) ~width:max_width : t list =
    let width = Int.max 1 max_width in
    if Line_width.width t <= width
    then [ t ]
    else (
      let lines = ref [] in
      let current = ref [] in
      let used = ref 0 in
      let flush () =
        lines := List.rev !current :: !lines;
        current := [];
        used := 0
      in
      let add (w : Span.t) =
        let ww = Span.width w in
        if !used + ww <= width
        then (
          current := w :: !current;
          used := !used + ww)
        else if ww > width
        then (
          if !used > 0 then flush ();
          let pieces = hard_split w ~width in
          List.iter pieces ~f:(fun p ->
            let pw = Span.width p in
            if !used + pw > width then flush ();
            current := p :: !current;
            used := !used + pw))
        else (
          (* A trailing space may hang over the edge. *)
          let trimmed = String.rstrip w.text in
          let tw = Text_width.string trimmed in
          if !used + tw <= width && not (String.is_empty trimmed)
          then (
            current := { w with text = trimmed } :: !current;
            flush ())
          else (
            flush ();
            current := [ w ];
            used := ww))
      in
      List.iter (words t) ~f:add;
      if !used > 0 || List.is_empty !lines then flush ();
      List.rev !lines)
  ;;

  let truncate (t : t) ~width:max_width : t =
    let width = max_width in
    if Line_width.width t <= width
    then t
    else (
      let rec go spans remaining acc =
        match spans with
        | [] -> List.rev acc
        | (span : Span.t) :: rest ->
          let w = Span.width span in
          if w <= remaining
          then go rest (remaining - w) (span :: acc)
          else (
            let text = Text_width.truncate span.text ~width:remaining in
            List.rev ({ span with text } :: acc))
      in
      go t width [])
  ;;

  let find_ci haystack needle start =
    let hay_len = String.length haystack in
    let needle_len = String.length needle in
    let lower = String.lowercase in
    let rec go i =
      if i + needle_len > hay_len
      then None
      else if String.equal
                (lower (String.sub haystack ~pos:i ~len:needle_len))
                (lower needle)
      then Some i
      else go (i + 1)
    in
    if needle_len = 0 then None else go start
  ;;

  let highlight (t : t) ~needle : t =
    if String.is_empty needle
    then t
    else (
      let needle_len = String.length needle in
      List.concat_map t ~f:(fun (span : Span.t) ->
        let text = span.text in
        let n = String.length text in
        let rec go pos acc =
          if pos >= n
          then List.rev acc
          else (
            match find_ci text needle pos with
            | None ->
              let rest = String.drop_prefix text pos in
              List.rev
                (if String.is_empty rest
                 then acc
                 else { span with text = rest } :: acc)
            | Some i ->
              let before = String.sub text ~pos ~len:(i - pos) in
              let matched = String.sub text ~pos:i ~len:needle_len in
              let acc =
                if String.is_empty before
                then acc
                else { span with text = before } :: acc
              in
              let acc =
                { Span.text = matched; style = Style.invert span.style } :: acc
              in
              go (i + needle_len) acc)
        in
        go 0 []))
  ;;
end

type t = Line.t list [@@deriving sexp_of, equal]

let lines ?style text : t =
  List.map (String.split text ~on:'\n') ~f:(Line.of_string ?style)
;;

let text ?style s : t = [ Line.of_string ?style s ]
let to_plain t = String.concat ~sep:"\n" (List.map t ~f:Line.to_plain)
let wrap t ~width = List.concat_map t ~f:(Line.wrap ~width)

let color_name (c : Style.Color.t) =
  match c with
  | Default -> "default"
  | Red -> "red"
  | Green -> "green"
  | Yellow -> "yellow"
  | Blue -> "blue"
  | Magenta -> "magenta"
  | Cyan -> "cyan"
  | Gray -> "gray"
  | White -> "white"
;;

let style_tags (s : Style.t) =
  List.filter_opt
    [ (match s.fg with
       | Default -> None
       | c -> Some (color_name c))
    ; Option.some_if s.bold "bold"
    ; Option.some_if s.dim "dim"
    ; Option.some_if s.italic "italic"
    ; Option.some_if s.underline "underline"
    ; Option.some_if s.invert "invert"
    ; Option.some_if s.strike "strike"
    ; Option.map s.link ~f:(fun url -> "link=" ^ url)
    ]
;;

let span_to_styled (s : Span.t) =
  match style_tags s.style with
  | [] -> s.text
  | tags ->
    String.concat (List.map tags ~f:(fun tag -> "[" ^ tag ^ "]"))
    ^ s.text
    ^ "[/]"
;;

let to_styled t =
  String.concat
    ~sep:"\n"
    (List.map t ~f:(fun line -> String.concat (List.map line ~f:span_to_styled)))
;;
