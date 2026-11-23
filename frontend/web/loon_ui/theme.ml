open! Import
open Js_of_ocaml

module Mode = struct
  type t =
    | Light
    | Dark
  [@@deriving enumerate, sexp, compare, equal]

  let toggle = function
    | Light -> Dark
    | Dark -> Light
  ;;

  let preferred () =
    match
      Js.Optdef.to_option (Js.Unsafe.global##.matchMedia : _ Js.Optdef.t)
    with
    | None -> Light
    | Some _ ->
      let query : < matches : bool Js.t Js.readonly_prop > Js.t =
        Js.Unsafe.global##matchMedia (Js.string "(prefers-color-scheme: dark)")
      in
      if Js.to_bool query##.matches then Dark else Light
  ;;
end

module Style = struct
  type t =
    | Vega
    | Nova
    | Maia
    | Lyra
    | Mira
    | Luma
    | Sera
    | Rhea
  [@@deriving enumerate, sexp, compare, equal]

  let name t = String.lowercase (Sexp.to_string (sexp_of_t t))

  let of_name s =
    List.find all ~f:(fun t -> String.equal (name t) (String.lowercase s))
  ;;

  (* Literal, for Tailwind's scanner (the style-vega: variants). *)
  let class_ = function
    | Vega -> "style-vega"
    | Nova -> "style-nova"
    | Maia -> "style-maia"
    | Lyra -> "style-lyra"
    | Mira -> "style-mira"
    | Luma -> "style-luma"
    | Sera -> "style-sera"
    | Rhea -> "style-rhea"
  ;;
end

module Menu_color = struct
  type t =
    | Default
    | Inverted
    | Default_translucent
    | Inverted_translucent
  [@@deriving enumerate, sexp, compare, equal]

  let name = function
    | Default -> "default"
    | Inverted -> "inverted"
    | Default_translucent -> "default-translucent"
    | Inverted_translucent -> "inverted-translucent"
  ;;

  let of_name s = List.find all ~f:(fun t -> String.equal (name t) s)

  let classes = function
    | Default -> []
    | Inverted -> [ "menu-inverted" ]
    | Default_translucent -> [ "menu-translucent" ]
    | Inverted_translucent -> [ "menu-inverted"; "menu-translucent" ]
  ;;
end

let attrs ?mode ?style ?menu_color () =
  (match (mode : Mode.t option) with
   | Some Dark -> [ Attr.class_ "dark" ]
   | Some Light | None -> [])
  @ (match style with
     | Some style -> [ Attr.class_ (Style.class_ style) ]
     | None -> [])
  @
  match menu_color with
  | Some m when not (List.is_empty (Menu_color.classes m)) ->
    [ Attr.classes (Menu_color.classes m) ]
  | Some _ | None -> []
;;

let root ?attrs:(extra = []) ?mode ?style ?menu_color children =
  Node.div
    ~attrs:
      (attrs ?mode ?style ?menu_color ()
       @ [ Attr.classes [ "bg-background"; "text-foreground" ] ]
       @ extra)
    children
;;
