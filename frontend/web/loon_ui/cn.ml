open! Import

(* Base's [String.lsplit2]/[rsplit2] raise and catch [Not_found] when the
   character is absent, which is slow under js_of_ocaml; these do not. *)
module String = struct
  include String

  let lsplit2 s ~on =
    match String.index s on with
    | None -> None
    | Some i -> Some (String.prefix s i, String.drop_prefix s (i + 1))
  ;;

  let rsplit2 s ~on =
    match String.rindex s on with
    | None -> None
    | Some i -> Some (String.prefix s i, String.drop_prefix s (i + 1))
  ;;
end

(* A class is [variants:...:base], where a variant may hold [:] inside
   brackets or parentheses ([data-[state=open]:], [has-[>svg]:]). *)
let split_variants cls =
  let parts = ref [] in
  let start = ref 0 in
  let depth = ref 0 in
  String.iteri cls ~f:(fun i c ->
    match c with
    | '[' | '(' -> incr depth
    | ']' | ')' -> decr depth
    | ':' when !depth = 0 ->
      parts := String.sub cls ~pos:!start ~len:(i - !start) :: !parts;
      start := i + 1
    | _ -> ());
  let base = String.drop_prefix cls !start in
  List.rev !parts, base
;;

let sizes_text =
  [ "xs"; "sm"; "base"; "lg"; "xl"; "2xl"; "3xl"; "4xl"; "5xl"; "6xl"; "7xl"; "8xl"; "9xl" ]
;;

let weights =
  [ "thin"
  ; "extralight"
  ; "light"
  ; "normal"
  ; "medium"
  ; "semibold"
  ; "bold"
  ; "extrabold"
  ; "black"
  ]
;;

let shadow_sizes = [ ""; "2xs"; "xs"; "sm"; "md"; "lg"; "xl"; "2xl"; "none"; "inner" ]
let radii = [ ""; "none"; "xs"; "sm"; "md"; "lg"; "xl"; "2xl"; "3xl"; "4xl"; "full" ]

let is_number s =
  (not (String.is_empty s)) && String.for_all s ~f:(fun c -> Char.is_digit c || Char.equal c '.')
;;

(* An arbitrary value ([...] or (...)) that is a length rather than a colour. *)
let is_length_value v =
  let inner =
    if String.length v >= 2
       && ((Char.equal v.[0] '[' && Char.equal v.[String.length v - 1] ']')
           || (Char.equal v.[0] '(' && Char.equal v.[String.length v - 1] ')'))
    then Some (String.sub v ~pos:1 ~len:(String.length v - 2))
    else None
  in
  match inner with
  | None -> false
  | Some inner ->
    String.is_prefix inner ~prefix:"length:"
    || (String.length inner > 0 && (Char.is_digit inner.[0] || Char.equal inner.[0] '.'))
    || String.is_prefix inner ~prefix:"calc("
;;

(* [rest] after [prefix-], if [base] is [prefix] or starts with [prefix-]. *)
let value ~prefix base =
  if String.equal base prefix
  then Some ""
  else String.chop_prefix base ~prefix:(prefix ^ "-")
;;

let sides4 = [ "t"; "r"; "b"; "l"; "s"; "e"; "x"; "y" ]

(* The conflict group of a base class (without variants and [!]), and the
   groups it overrides besides its own. *)
let arbitrary_property base =
  (* [[--cell-size:--spacing(8)]]: an arbitrary property, grouped by its name. *)
  if String.is_prefix base ~prefix:"[" && String.is_suffix base ~suffix:"]"
  then
    Option.map
      (String.lsplit2 (String.sub base ~pos:1 ~len:(String.length base - 2)) ~on:':')
      ~f:(fun (property, _) -> "[" ^ property, [])
  else None
;;

let group base : (string * string list) option =
  match arbitrary_property base with
  | Some g -> Some g
  | None ->
  let base = String.chop_prefix_if_exists base ~prefix:"-" in
  let first_of prefixes =
    List.find_map prefixes ~f:(fun (prefix, g) ->
      Option.map (value ~prefix base) ~f:(fun v -> v, g))
  in
  let exact =
    match base with
    | "block"
    | "inline-block"
    | "inline"
    | "flex"
    | "inline-flex"
    | "grid"
    | "inline-grid"
    | "contents"
    | "table"
    | "table-row"
    | "table-cell"
    | "flow-root"
    | "list-item"
    | "hidden" -> Some ("display", [])
    | "static" | "fixed" | "absolute" | "relative" | "sticky" -> Some ("position", [])
    | "visible" | "invisible" | "collapse" -> Some ("visibility", [])
    | "flex-row" | "flex-row-reverse" | "flex-col" | "flex-col-reverse" ->
      Some ("flex-direction", [])
    | "flex-wrap" | "flex-wrap-reverse" | "flex-nowrap" -> Some ("flex-wrap", [])
    | "uppercase" | "lowercase" | "capitalize" | "normal-case" ->
      Some ("text-transform", [])
    | "italic" | "not-italic" -> Some ("font-style", [])
    | "underline" | "overline" | "line-through" | "no-underline" ->
      Some ("text-decoration-line", [])
    | "truncate" | "text-ellipsis" | "text-clip" -> Some ("text-overflow", [])
    | "text-wrap" | "text-nowrap" | "text-balance" | "text-pretty" ->
      Some ("text-wrap", [])
    | "sr-only" | "not-sr-only" -> Some ("sr", [])
    | "grow" | "grow-0" -> Some ("grow", [])
    | "shrink" | "shrink-0" -> Some ("shrink", [])
    | "border-solid"
    | "border-dashed"
    | "border-dotted"
    | "border-double"
    | "border-hidden"
    | "border-none" -> Some ("border-style", [])
    | "outline-none" | "outline-hidden" | "outline-dashed" | "outline-dotted" ->
      Some ("outline-style", [])
    | "ring-inset" -> Some ("ring-inset", [])
    | "transition"
    | "transition-all"
    | "transition-colors"
    | "transition-opacity"
    | "transition-shadow"
    | "transition-transform"
    | "transition-none"
    | "transition-[color,box-shadow]" -> Some ("transition", [])
    | _ -> None
  in
  match exact with
  | Some _ as g -> g
  | None ->
    let spacing letter name =
      match value ~prefix:letter base with
      | Some _ ->
        Some
          ( name
          , List.map sides4 ~f:(fun s -> name ^ s)
            @ List.map sides4 ~f:(fun s -> name ^ "-" ^ s) )
      | None ->
        List.find_map sides4 ~f:(fun s ->
          match value ~prefix:(letter ^ s) base with
          | None -> None
          | Some _ ->
            let overrides =
              match s with
              | "x" -> [ name ^ "r"; name ^ "l"; name ^ "s"; name ^ "e" ]
              | "y" -> [ name ^ "t"; name ^ "b" ]
              | _ -> []
            in
            Some (name ^ s, overrides))
    in
    let ( |? ) a b =
      match a with
      | Some _ -> a
      | None -> Lazy.force b
    in
    spacing "p" "p"
    |? lazy (spacing "m" "m")
    |? lazy
         (match
            first_of
              [ "space-x", "space-x"
              ; "space-y", "space-y"
              ; "min-w", "min-w"
              ; "min-h", "min-h"
              ; "max-w", "max-w"
              ; "max-h", "max-h"
              ; "w", "w"
              ; "h", "h"
              ; "z", "z"
              ; "gap-x", "gap-x"
              ; "gap-y", "gap-y"
              ; "basis", "basis"
              ; "grid-cols", "grid-cols"
              ; "grid-rows", "grid-rows"
              ; "col-span", "col"
              ; "row-span", "row"
              ; "col-start", "col-start"
              ; "row-start", "row-start"
              ; "justify-items", "justify-items"
              ; "justify-self", "justify-self"
              ; "justify", "justify"
              ; "items", "items"
              ; "self", "self"
              ; "content", "align-content"
              ; "place-items", "place-items"
              ; "place-content", "place-content"
              ; "overflow-x", "overflow-x"
              ; "overflow-y", "overflow-y"
              ; "leading", "leading"
              ; "tracking", "tracking"
              ; "opacity", "opacity"
              ; "cursor", "cursor"
              ; "pointer-events", "pointer-events"
              ; "select", "select"
              ; "duration", "duration"
              ; "ease", "ease"
              ; "delay", "delay"
              ; "whitespace", "whitespace"
              ; "break", "break"
              ; "aspect", "aspect"
              ; "object", "object"
              ; "underline-offset", "underline-offset"
              ; "ring-offset", "ring-offset"
              ; "outline-offset", "outline-offset"
              ; "animate", "animate"
              ; "order", "order"
              ; "line-clamp", "line-clamp"
              ; "translate-x", "translate-x"
              ; "translate-y", "translate-y"
              ; "inset-x", "inset-x"
              ; "inset-y", "inset-y"
              ; "top", "top"
              ; "right", "right"
              ; "bottom", "bottom"
              ; "left", "left"
              ; "start", "start"
              ; "end", "end"
              ]
          with
          | Some (_, g) ->
            let overrides =
              match g with
              | "inset-x" -> [ "left"; "right"; "start"; "end" ]
              | "inset-y" -> [ "top"; "bottom" ]
              | _ -> []
            in
            Some (g, overrides)
          | None -> None)
    |? lazy
         (match value ~prefix:"size" base with
          | Some _ -> Some ("size", [ "w"; "h" ])
          | None -> None)
    |? lazy
         (match value ~prefix:"inset" base with
          | Some _ ->
            Some
              ("inset", [ "inset-x"; "inset-y"; "top"; "right"; "bottom"; "left"; "start"; "end" ])
          | None -> None)
    |? lazy
         (match value ~prefix:"gap" base with
          | Some _ -> Some ("gap", [ "gap-x"; "gap-y" ])
          | None -> None)
    |? lazy
         (match value ~prefix:"overflow" base with
          | Some _ -> Some ("overflow", [ "overflow-x"; "overflow-y" ])
          | None -> None)
    |? lazy
         (match value ~prefix:"flex" base with
          | Some v when not (String.is_empty v) -> Some ("flex", [])
          | _ -> None)
    |? lazy
         (match value ~prefix:"text" base with
          | Some v when List.mem sizes_text v ~equal:String.equal || is_length_value v ->
            Some ("font-size", [])
          | Some ("left" | "center" | "right" | "justify" | "start" | "end") ->
            Some ("text-align", [])
          | Some v when not (String.is_empty v) -> Some ("text-color", [])
          | _ -> None)
    |? lazy
         (match value ~prefix:"font" base with
          | Some v when List.mem weights v ~equal:String.equal -> Some ("font-weight", [])
          | Some v when not (String.is_empty v) -> Some ("font-family", [])
          | _ -> None)
    |? lazy
         (match value ~prefix:"bg" base with
          | Some
              ( "fixed"
              | "local"
              | "scroll"
              | "cover"
              | "contain"
              | "auto"
              | "center"
              | "top"
              | "bottom"
              | "left"
              | "right"
              | "repeat"
              | "no-repeat" ) -> None
          | Some v
            when String.is_prefix v ~prefix:"clip-"
                 || String.is_prefix v ~prefix:"origin-"
                 || String.is_prefix v ~prefix:"linear-"
                 || String.is_prefix v ~prefix:"gradient-"
                 || String.is_prefix v ~prefix:"radial"
                 || String.is_prefix v ~prefix:"conic" -> None
          | Some v when not (String.is_empty v) -> Some ("bg-color", [])
          | _ -> None)
    |? lazy
         (let rounded_sides =
            [ "t"; "r"; "b"; "l"; "s"; "e"; "tl"; "tr"; "br"; "bl"; "ss"; "se"; "es"; "ee" ]
          in
          match value ~prefix:"rounded" base with
          | Some v when List.mem radii v ~equal:String.equal || is_length_value v ->
            Some ("rounded", List.map rounded_sides ~f:(fun s -> "rounded-" ^ s))
          | Some v ->
            let side, _ = String.lsplit2 v ~on:'-' |> Option.value ~default:(v, "") in
            let overrides =
              match side with
              | "t" -> [ "rounded-tl"; "rounded-tr" ]
              | "r" -> [ "rounded-tr"; "rounded-br" ]
              | "b" -> [ "rounded-br"; "rounded-bl" ]
              | "l" -> [ "rounded-tl"; "rounded-bl" ]
              | _ -> []
            in
            Some ("rounded-" ^ side, overrides)
          | None -> None)
    |? lazy
         (match value ~prefix:"border" base with
          | None -> None
          | Some v ->
            let width v = String.is_empty v || is_number v || is_length_value v in
            let side, rest =
              match String.lsplit2 v ~on:'-' with
              | Some (s, rest) when List.mem sides4 s ~equal:String.equal -> s, rest
              | _ when List.mem sides4 v ~equal:String.equal -> v, ""
              | _ -> "", v
            in
            let kind = if width rest then "border-w" else "border-color" in
            if String.is_empty side
            then
              Some
                ( kind
                , List.concat_map sides4 ~f:(fun s -> [ kind ^ "-" ^ s ]) )
            else
              Some
                ( kind ^ "-" ^ side
                , match side with
                  | "x" -> [ kind ^ "-r"; kind ^ "-l"; kind ^ "-s"; kind ^ "-e" ]
                  | "y" -> [ kind ^ "-t"; kind ^ "-b" ]
                  | _ -> [] ))
    |? lazy
         (match value ~prefix:"ring" base with
          | Some v when String.is_empty v || is_number v || is_length_value v ->
            Some ("ring-w", [])
          | Some _ -> Some ("ring-color", [])
          | None -> None)
    |? lazy
         (match value ~prefix:"outline" base with
          | Some v when String.is_empty v || is_number v || is_length_value v ->
            Some ("outline-w", [])
          | Some _ -> Some ("outline-color", [])
          | None -> None)
    |? lazy
         (match value ~prefix:"shadow" base with
          | Some v when List.mem shadow_sizes v ~equal:String.equal || is_length_value v ->
            Some ("shadow", [])
          | Some _ -> Some ("shadow-color", [])
          | None -> None)
    |? lazy
         (match value ~prefix:"stroke" base with
          | Some v when is_number v || is_length_value v -> Some ("stroke-w", [])
          | Some _ -> Some ("stroke", [])
          | None -> None)
    |? lazy
         (match value ~prefix:"fill" base with
          | Some _ -> Some ("fill", [])
          | None -> None)
;;

let classes strings =
  List.concat_map strings ~f:(fun s ->
    String.split_on_chars s ~on:[ ' '; '\n'; '\t' ]
    |> List.filter ~f:(fun c -> not (String.is_empty c)))
;;

let merge classes =
  (* From the last class back: a class is dropped if a later one (kept)
     claimed its group under the same variants. *)
  let claimed = Hash_set.create (module String) in
  let seen = Hash_set.create (module String) in
  let kept =
    List.fold (List.rev classes) ~init:[] ~f:(fun kept cls ->
      if Hash_set.mem seen cls
      then kept
      else (
        Hash_set.add seen cls;
        let variants, base = split_variants cls in
        let important, base =
          match String.chop_prefix base ~prefix:"!" with
          | Some b -> "!", b
          | None ->
            (match String.chop_suffix base ~suffix:"!" with
             | Some b -> "!", b
             | None -> "", base)
        in
        let base, modifier =
          (* [/50] opacity on colours does not change the group. *)
          match String.rsplit2 base ~on:'/' with
          | Some (b, _) when not (String.mem b '[') -> b, true
          | _ -> base, false
        in
        match group base with
        | None -> cls :: kept
        | Some (g, overrides) ->
          (* As tailwind-merge: only a font size with a line height
             ([text-lg/7]) sets [line-height] over an earlier [leading-*]. *)
          let overrides =
            if modifier && String.equal g "font-size" then "leading" :: overrides else overrides
          in
          let key g =
            String.concat ~sep:":" (List.sort variants ~compare:String.compare)
            ^ "|"
            ^ important
            ^ g
          in
          if Hash_set.mem claimed (key g)
          then kept
          else (
            List.iter (g :: overrides) ~f:(fun g -> Hash_set.add claimed (key g));
            cls :: kept)))
  in
  kept
;;

(* As tailwind-merge, a cache of results: the same class lists come back on
   every render. *)
let cache = Hashtbl.create (module String)

let merged strings =
  let key = String.concat ~sep:"\n" strings in
  match Hashtbl.find cache key with
  | Some r -> r
  | None ->
    let r = merge (classes strings) in
    if Hashtbl.length cache >= 2000 then Hashtbl.clear cache;
    Hashtbl.set cache ~key ~data:r;
    r
;;

let cn strings = String.concat ~sep:" " (merged strings)
let attr strings = Attr.classes (merged strings)
let with_class ?class_ strings = attr (strings @ Option.to_list class_)
