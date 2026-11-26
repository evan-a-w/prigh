open! Import

module Entry = struct
  type t =
    | Page of int
    | Ellipsis
  [@@deriving sexp_of, compare, equal]
end

let range a b =
  List.range a b ~stop:`inclusive |> List.map ~f:(fun p -> Entry.Page p)
;;

let entries ?(siblings = 1) ~page ~pages () : Entry.t list =
  let page = Int.clamp_exn page ~min:1 ~max:(Int.max 1 pages) in
  (* first, last, current, the siblings and two ellipses *)
  let slots = (2 * siblings) + 5 in
  if pages <= slots
  then range 1 pages
  else (
    let left = Int.max (page - siblings) 1 in
    let right = Int.min (page + siblings) pages in
    let show_left = left > 3 in
    let show_right = right < pages - 2 in
    match show_left, show_right with
    | false, true ->
      range 1 (3 + (2 * siblings)) @ [ Entry.Ellipsis; Page pages ]
    | true, false ->
      (Entry.Page 1 :: [ Entry.Ellipsis ])
      @ range (pages - (2 + (2 * siblings))) pages
    | _ ->
      (Entry.Page 1 :: Ellipsis :: range left right) @ [ Ellipsis; Page pages ])
;;

let root ?(attrs = []) ?class_ children =
  Node.create
    "nav"
    ~attrs:
      ([ Attr.role "navigation"
       ; aria "label" "pagination"
       ; data "slot" "pagination"
       ; Cn.with_class ?class_ [ "mx-auto flex w-full justify-center" ]
       ]
       @ attrs)
    children
;;

let content ?(attrs = []) ?class_ children =
  Node.ul
    ~attrs:
      ([ data "slot" "pagination-content"
       ; Cn.with_class ?class_ [ "gap-0.5 flex items-center" ]
       ]
       @ attrs)
    children
;;

let item ?(attrs = []) ?class_ children =
  Node.li
    ~attrs:
      ([ data "slot" "pagination-item" ]
       @ Option.value_map class_ ~default:[] ~f:(fun c -> [ Cn.attr [ c ] ])
       @ attrs)
    children
;;

let link
      ?(attrs = [])
      ?class_
      ?(is_active = false)
      ?(size = Button.Size.Icon)
      ?href
      ?on_click
      children
  =
  let variant : Button.Variant.t = if is_active then Outline else Ghost in
  Node.a
    ~attrs:
      ([ data "slot" "pagination-link"
       ; data "variant" (Button.Variant.name variant)
       ; data "size" (Button.Size.name size)
       ]
       @ (if is_active
          then [ aria "current" "page"; data "active" "true" ]
          else [])
       @ [ Attr.class_
             (Button.classes
                ~variant
                ~size
                ~class_:(Cn.cn ([ "" ] @ Option.to_list class_))
                ())
         ]
       @ Option.value_map href ~default:[] ~f:(fun h -> [ Attr.href h ])
       @ Option.value_map on_click ~default:[] ~f:(fun eff ->
         [ Attr.on_click (fun ev ->
             Effect.Many [ Dom_util.prevent_default ev; eff ])
         ])
       @ attrs)
    children
;;

let previous ?(attrs = []) ?class_ ?(text = "Previous") ?href ?on_click () =
  link
    ~attrs:([ aria "label" "Go to previous page" ] @ attrs)
    ~size:Default
    ~class_:(Cn.cn ([ "pl-1.5!" ] @ Option.to_list class_))
    ?href
    ?on_click
    [ Icon.view ~attrs:[ data "icon" "inline-start" ] ~class_:"" Chevron_left
    ; Node.span ~attrs:[ Attr.class_ "hidden sm:block" ] [ Node.text text ]
    ]
;;

let next ?(attrs = []) ?class_ ?(text = "Next") ?href ?on_click () =
  link
    ~attrs:([ aria "label" "Go to next page" ] @ attrs)
    ~size:Default
    ~class_:(Cn.cn ([ "pr-1.5!" ] @ Option.to_list class_))
    ?href
    ?on_click
    [ Node.span ~attrs:[ Attr.class_ "hidden sm:block" ] [ Node.text text ]
    ; Icon.view ~attrs:[ data "icon" "inline-end" ] ~class_:"" Chevron_right
    ]
;;

let ellipsis ?(attrs = []) ?class_ () =
  Node.span
    ~attrs:
      ([ aria "hidden" "true"
       ; data "slot" "pagination-ellipsis"
       ; Cn.with_class
           ?class_
           [ "size-8 [&_svg:not([class*='size-'])]:size-4 flex items-center \
              justify-center"
           ]
       ]
       @ attrs)
    [ Icon.view Ellipsis
    ; Node.span ~attrs:[ Attr.class_ "sr-only" ] [ Node.text "More pages" ]
    ]
;;
