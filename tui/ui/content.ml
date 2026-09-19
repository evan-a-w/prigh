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
end

type t = Line.t list [@@deriving sexp_of, equal]

let lines ?style text : t =
  List.map (String.split text ~on:'\n') ~f:(Line.of_string ?style)
;;

let text ?style s : t = [ Line.of_string ?style s ]
let to_plain t = String.concat ~sep:"\n" (List.map t ~f:Line.to_plain)
let wrap t ~width = List.concat_map t ~f:(Line.wrap ~width)
