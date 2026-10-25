open! Core
open! Import

module Name = struct
  type t =
    | Original
    | Dark
    | Light
  [@@deriving sexp_of, equal, enumerate]

  let to_string = function
    | Original -> "original"
    | Dark -> "dark"
    | Light -> "light"
  ;;

  let of_string s =
    List.find all ~f:(fun t ->
      String.equal (to_string t) (String.lowercase (String.strip s)))
  ;;

  let describe = function
    | Original -> "graphite, warm greys"
    | Dark -> "neutral greys, only the accent in colour"
    | Light -> "paper white"
  ;;
end

module Accent = struct
  module Preset = struct
    type t =
      | Orange
      | Blue
      | Green
      | Purple
      | Pink
      | Teal
      | Yellow
    [@@deriving sexp_of, equal, enumerate]

    let to_string = function
      | Orange -> "orange"
      | Blue -> "blue"
      | Green -> "green"
      | Purple -> "purple"
      | Pink -> "pink"
      | Teal -> "teal"
      | Yellow -> "yellow"
    ;;

    (* As in theme.css's [--accent-*], on the dark themes. *)
    let hex = function
      | Orange -> "#f0b44c"
      | Blue -> "#6aa6f8"
      | Green -> "#62c27a"
      | Purple -> "#b08cf2"
      | Pink -> "#ee86b8"
      | Teal -> "#45c4bb"
      | Yellow -> "#e6d04a"
    ;;
  end

  type t =
    | Preset of Preset.t
    | Custom of string
  [@@deriving sexp_of, equal]

  let all_presets = List.map Preset.all ~f:(fun p -> Preset p)

  let hex_digits s =
    (not (String.is_empty s)) && String.for_all s ~f:Char.is_hex_digit
  ;;

  let of_string s =
    let s = String.lowercase (String.strip s) in
    match String.chop_prefix s ~prefix:"#" with
    | Some digits when String.length digits = 6 && hex_digits digits ->
      Some (Custom s)
    | Some digits when String.length digits = 3 && hex_digits digits ->
      Some
        (Custom ("#" ^ String.concat_map digits ~f:(fun c -> String.make 2 c)))
    | Some _ -> None
    | None ->
      List.find Preset.all ~f:(fun p -> String.equal (Preset.to_string p) s)
      |> Option.map ~f:(fun p -> Preset p)
  ;;

  let to_string = function
    | Preset p -> Preset.to_string p
    | Custom hex -> hex
  ;;

  let hex = function
    | Preset p -> Preset.hex p
    | Custom hex -> hex
  ;;

  (* WCAG's relative luminance: dark ink on light accents, white on dark. *)
  let ink hex =
    let channel i =
      let c =
        Float.of_int
          (Int.of_string ("0x" ^ String.sub hex ~pos:(1 + (2 * i)) ~len:2))
        /. 255.
      in
      if Float.(c <= 0.03928)
      then c /. 12.92
      else ((c +. 0.055) /. 1.055) **. 2.4
    in
    let l =
      (0.2126 *. channel 0) +. (0.7152 *. channel 1) +. (0.0722 *. channel 2)
    in
    (* Where black and white text contrast equally with it. *)
    if Float.(l > 0.179) then "#15171b" else "#ffffff"
  ;;
end

type t =
  { name : Name.t
  ; accent : Accent.t
  }
[@@deriving sexp_of, equal]

let default = { name = Original; accent = Preset Orange }

let attributes t =
  [ "data-theme", Name.to_string t.name
  ; ( "data-accent"
    , match t.accent with
      | Preset p -> Accent.Preset.to_string p
      | Custom _ -> "custom" )
  ]
;;

let vars t =
  match t.accent with
  | Preset _ -> []
  | Custom hex -> [ "--sodium", hex; "--sodium-ink", Accent.ink hex ]
;;
