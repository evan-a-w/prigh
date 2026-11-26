open! Import
open Js_of_ocaml

module Align = struct
  type t =
    | Inline_start
    | Inline_end
    | Block_start
    | Block_end
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Inline_start -> "inline-start"
    | Inline_end -> "inline-end"
    | Block_start -> "block-start"
    | Block_end -> "block-end"
  ;;

  let classes = function
    | Inline_start ->
      "pl-2 has-[>button]:ml-[-0.3rem] has-[>kbd]:ml-[-0.15rem] order-first"
    | Inline_end ->
      "pr-2 has-[>button]:mr-[-0.3rem] has-[>kbd]:mr-[-0.15rem] order-last"
    | Block_start ->
      "px-2.5 pt-2 group-has-[>input]/input-group:pt-2 [.border-b]:pb-2 \
       order-first w-full justify-start"
    | Block_end ->
      "px-2.5 pb-2 group-has-[>input]/input-group:pb-2 [.border-t]:pt-2 \
       order-last w-full justify-start"
  ;;
end

module Button_size = struct
  type t =
    | Xs
    | Sm
    | Icon_xs
    | Icon_sm
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Xs -> "xs"
    | Sm -> "sm"
    | Icon_xs -> "icon-xs"
    | Icon_sm -> "icon-sm"
  ;;

  let classes = function
    | Xs ->
      "h-6 gap-1 rounded-[calc(var(--radius)-3px)] px-1.5 \
       [&>svg:not([class*='size-'])]:size-3.5"
    | Sm -> ""
    | Icon_xs -> "size-6 rounded-[calc(var(--radius)-3px)] p-0 has-[>svg]:p-0"
    | Icon_sm -> "size-8 p-0 has-[>svg]:p-0"
  ;;
end

let root ?(attrs = []) ?class_ ?(disabled = false) children =
  Node.div
    ~attrs:
      ([ data "slot" "input-group"
       ; Attr.role "group"
       ; Cn.with_class
           ?class_
           [ "border-input dark:bg-input/30 \
              has-[[data-slot=input-group-control]:focus-visible]:border-ring \
              has-[[data-slot=input-group-control]:focus-visible]:ring-ring/50 \
              has-[[data-slot][aria-invalid=true]]:ring-destructive/20 \
              has-[[data-slot][aria-invalid=true]]:border-destructive \
              dark:has-[[data-slot][aria-invalid=true]]:ring-destructive/40 \
              has-disabled:bg-input/50 dark:has-disabled:bg-input/80 h-8 \
              rounded-lg border transition-colors \
              in-data-[slot=combobox-content]:focus-within:border-inherit \
              in-data-[slot=combobox-content]:focus-within:ring-0 \
              has-disabled:opacity-50 \
              has-[[data-slot=input-group-control]:focus-visible]:ring-3 \
              has-[[data-slot][aria-invalid=true]]:ring-3 \
              has-[>[data-align=block-end]]:h-auto \
              has-[>[data-align=block-end]]:flex-col \
              has-[>[data-align=block-start]]:h-auto \
              has-[>[data-align=block-start]]:flex-col \
              has-[>[data-align=block-end]]:[&>input]:pt-3 \
              has-[>[data-align=block-start]]:[&>input]:pb-3 \
              has-[>[data-align=inline-end]]:[&>input]:pr-1.5 \
              has-[>[data-align=inline-start]]:[&>input]:pl-1.5 \
              group/input-group relative flex w-full min-w-0 items-center \
              outline-none has-[>textarea]:h-auto"
           ]
       ]
       @ (if disabled then [ data "disabled" "true" ] else [])
       @ attrs)
    children
;;

let focus_input (ev : Dom_html.mouseEvent Js.t) =
  Effect.of_thunk (fun () ->
    let target : Dom_html.element Js.t = Js.Unsafe.get ev "target" in
    let current : Dom_html.element Js.t = Js.Unsafe.get ev "currentTarget" in
    let in_button =
      Js.Opt.test
        (Js.Unsafe.meth_call
           target
           "closest"
           [| Js.Unsafe.inject (Js.string "button") |]
         : Dom_html.element Js.t Js.Opt.t)
    in
    if not in_button
    then
      Js.Opt.iter current##.parentNode (fun parent ->
        let parent : Dom_html.element Js.t = Js.Unsafe.coerce parent in
        Js.Opt.iter
          (parent##querySelector (Js.string "input"))
          (fun input -> input##focus)))
;;

let addon ?(attrs = []) ?class_ ?(align = Align.Inline_start) children =
  Node.div
    ~attrs:
      ([ Attr.role "group"
       ; data "slot" "input-group-addon"
       ; data "align" (Align.name align)
       ; Cn.with_class
           ?class_
           [ "text-muted-foreground h-auto gap-2 py-1.5 text-sm font-medium \
              group-data-[disabled=true]/input-group:opacity-50 \
              [&>kbd]:rounded-[calc(var(--radius)-5px)] \
              [&>svg:not([class*='size-'])]:size-4 flex cursor-text \
              items-center justify-center select-none"
           ; Align.classes align
           ]
       ; Attr.on_click focus_input
       ]
       @ attrs)
    children
;;

let button
      ?(attrs = [])
      ?class_
      ?(variant = Button.Variant.Ghost)
      ?(size = Button_size.Xs)
      ?disabled
      ?type_
      ?on_click
      children
  =
  Button.view
    ~attrs:
      (Attr.suppress_merge_warnings (data "size" (Button_size.name size))
       :: attrs)
    ~class_:
      (Cn.cn
         ([ "gap-2 text-sm flex items-center shadow-none"
          ; Button_size.classes size
          ]
          @ Option.to_list class_))
    ~variant
    ?disabled
    ?type_
    ?on_click
    children
;;

let text ?(attrs = []) ?class_ children =
  Node.span
    ~attrs:
      (Cn.with_class
         ?class_
         [ "text-muted-foreground gap-2 text-sm \
            [&_svg:not([class*='size-'])]:size-4 flex items-center \
            [&_svg]:pointer-events-none"
         ]
       :: attrs)
    children
;;

let control_slot =
  Attr.suppress_merge_warnings (data "slot" "input-group-control")
;;

let input
      ?(attrs = [])
      ?class_
      ?type_
      ?id
      ?name
      ?placeholder
      ?disabled
      ?required
      ?invalid
      ?value
      ?on_input
      ()
  =
  Input.view
    ~attrs:(control_slot :: attrs)
    ~class_:
      (Cn.cn
         ([ "rounded-none border-0 bg-transparent shadow-none ring-0 \
             focus-visible:ring-0 disabled:bg-transparent aria-invalid:ring-0 \
             dark:bg-transparent dark:disabled:bg-transparent flex-1"
          ]
          @ Option.to_list class_))
    ?type_
    ?id
    ?name
    ?placeholder
    ?disabled
    ?required
    ?invalid
    ?value
    ?on_input
    ()
;;

let textarea
      ?(attrs = [])
      ?class_
      ?id
      ?name
      ?placeholder
      ?rows
      ?disabled
      ?required
      ?invalid
      ?value
      ?on_input
      ()
  =
  Textarea.view
    ~attrs:(control_slot :: attrs)
    ~class_:
      (Cn.cn
         ([ "rounded-none border-0 bg-transparent py-2 shadow-none ring-0 \
             focus-visible:ring-0 disabled:bg-transparent aria-invalid:ring-0 \
             dark:bg-transparent dark:disabled:bg-transparent flex-1 \
             resize-none"
          ]
          @ Option.to_list class_))
    ?id
    ?name
    ?placeholder
    ?rows
    ?disabled
    ?required
    ?invalid
    ?value
    ?on_input
    ()
;;
