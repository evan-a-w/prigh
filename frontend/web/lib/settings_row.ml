open! Core
open! Import
open Html

module Scope = struct
  type t =
    | Browser
    | Account
    | Backend

  let tag = function
    | Browser -> "This browser"
    | Account -> "Your account"
    | Backend -> "Backend"
  ;;

  let title = function
    | Browser -> "Kept in this browser only"
    | Account -> "Follows you to every browser you sign in on"
    | Backend -> "Kept in the backend's configuration"
  ;;
end

let cn = Loon.Cn.cn
let attr_cls parts = Attr.class_ (cn parts)

let row_cls =
  "setting flex flex-wrap items-center gap-x-4 gap-y-2 border-t border-border \
   px-4 py-3 first:border-t-0 narrow:px-3.5 narrow:py-3.5 \
   narrow:[&_:is(.btn,select,.chip-button,.memory-filter,.memory-title-input)]:min-h-11 \
   narrow:[&_.btn.icon]:size-11"
;;

let stack_cls = "flex-col flex-nowrap items-stretch gap-y-2.5"

let custom ?(cls = "") ?(stack = false) ?(attrs = []) children =
  Node.div
    ~attrs:
      (attr_cls [ row_cls; (if stack then stack_cls else ""); cls ] :: attrs)
    children
;;

let name text =
  Node.span
    ~attrs:
      [ Attr.class_
          "setting-name text-sm font-medium text-foreground \
           [overflow-wrap:anywhere]"
      ]
    [ Node.text text ]
;;

let about_cls =
  "setting-about max-w-[62ch] text-[13px] leading-snug text-muted-foreground"
;;

let about ?(cls = "") text =
  Node.span ~attrs:[ attr_cls [ about_cls; cls ] ] [ Node.text text ]
;;

let host text = about ~cls:"host font-mono text-xs" text

let note_colour = function
  | "warm" -> "text-primary"
  | "good" -> "text-[color:var(--sage)]"
  | "bad" -> "text-destructive"
  | _ -> "text-muted-foreground/80"
;;

let note ?(cls = "") (level, text) =
  Node.span
    ~attrs:
      [ attr_cls
          [ "setting-note " ^ level
          ; "text-[13px] leading-snug [overflow-wrap:anywhere]"
          ; note_colour level
          ; cls
          ]
      ]
    [ Node.text text ]
;;

let text_cls ~wide =
  if wide
  then "setting-text flex min-w-0 flex-col gap-0.5"
  else
    "setting-text flex min-w-0 flex-[1_1_260px] flex-col gap-0.5 \
     narrow:basis-full"
;;

let text ?(cls = "") ?(wide = false) children =
  Node.div ~attrs:[ attr_cls [ text_cls ~wide; cls ] ] children
;;

let control_cls ~wide =
  if wide
  then "setting-control flex min-w-0 flex-wrap items-center gap-2"
  else
    "setting-control ml-auto flex min-w-0 max-w-full shrink-0 items-center \
     gap-2 narrow:has-[.setting-segments]:ml-0 \
     narrow:has-[.setting-segments]:basis-full \
     narrow:has-[>.setting-select,>input[type=text]]:ml-0 \
     narrow:has-[>.setting-select,>input[type=text]]:basis-full \
     narrow:[&>.setting-select]:max-w-none \
     narrow:[&>.setting-select]:flex-auto narrow:[&>.setting-select]:min-w-0 \
     narrow:[&>.setting-select]:shrink \
     narrow:[&>.setting-select]:justify-between \
     narrow:[&>input[type=text]]:flex-1 \
     narrow:has-[>.default-reset,>.fallback-actions]:ml-0 \
     narrow:has-[>.default-reset,>.fallback-actions]:basis-full \
     narrow:[&>.default-reset]:min-w-11"
;;

let control ?(cls = "") ?(wide = false) children =
  Node.div ~attrs:[ attr_cls [ control_cls ~wide; cls ] ] children
;;

let hint ?(cls = "") text =
  Node.span
    ~attrs:[ attr_cls [ "text-xs text-muted-foreground/80"; cls ] ]
    [ Node.text text ]
;;

let label ?(cls = "") ?for_ text =
  Node.label
    ~attrs:
      ([ attr_cls [ "text-[13px] font-medium text-muted-foreground"; cls ] ]
       @ Option.value_map for_ ~default:[] ~f:(fun id -> [ Attr.for_ id ]))
    [ Node.text text ]
;;

let input_cls cls =
  Loon.Input.classes ~class_:("narrow:h-11 narrow:text-base " ^ cls) ()
;;

let textarea_cls cls =
  Loon.Textarea.classes ~class_:("resize-y narrow:text-base " ^ cls) ()
;;

let buttons ?(cls = "") children =
  Node.div
    ~attrs:[ attr_cls [ "flex flex-wrap items-center justify-end gap-2"; cls ] ]
    children
;;

let scope_badge scope =
  Loon.Badge.view
    ~variant:Outline
    ~class_:
      "setting-scope h-4 cursor-help px-1.5 text-[10.5px] font-normal \
       text-muted-foreground"
    ~attrs:[ Attr.title (Scope.title scope) ]
    [ Node.text (Scope.tag scope) ]
;;

let row
      ?(cls = "")
      ?note:n
      ?below
      ?scope
      ?(wide = false)
      ?keys
      ~name:title
      ~about:a
      controls
  =
  (* Not wide: a grid of the text and the control rather than a wrapping
     flex row, whose wrap flips (WebKit) on a re-layout as small as a focus
     when the two only just fit, moving a control from under the pointer
     between a click's press and its release. The control's column shrinks
     (its segments wrap) before the text's goes under 10rem. *)
  custom
    ~cls:
      (String.concat
         ~sep:" "
         [ cls
         ; (if wide
            then "setting-wide"
            else
              "grid \
               grid-cols-[minmax(10rem,1fr)_minmax(0,auto)]                \
               narrow:grid-cols-1")
         ])
    ~stack:wide
    [ text
        ~wide
        [ Node.div
            ~attrs:
              [ Attr.class_ "setting-head flex flex-wrap items-baseline gap-2" ]
            [ name title
            ; Option.value_map scope ~default:Node.none ~f:scope_badge
            ]
        ; (match keys with
           | None -> about a
           | Some keys ->
             (* The shortcut goes before the sentence's full stop. *)
             let a, stop =
               match String.chop_suffix a ~suffix:"." with
               | Some a -> a, "."
               | None -> a, ""
             in
             Node.span
               ~attrs:[ Attr.class_ about_cls ]
               [ Node.text a
               ; Node.span
                   ~attrs:[ Attr.class_ "keyboard" ]
                   [ Node.text (sprintf " (%s)" keys) ]
               ; Node.text stop
               ])
        ; Option.value_map n ~default:Node.none ~f:(fun n -> note n)
        ]
    ; control ~wide controls
    ; Option.value below ~default:Node.none
    ]
;;

let group ?title ?note:n rows =
  Node.section
    ~attrs:[ Attr.class_ "settings-group mt-6 first:mt-4" ]
    [ (match title, n with
       | None, None -> Node.none
       | _ ->
         Node.div
           ~attrs:
             [ Attr.class_
                 "settings-group-head flex flex-wrap items-baseline gap-x-2.5 \
                  gap-y-0.5 px-1 pb-2"
             ]
           [ (match title with
              | Some title ->
                Node.h4
                  ~attrs:
                    [ Attr.class_ "text-[13px] font-medium text-foreground" ]
                  [ Node.text title ]
              | None -> Node.none)
           ; (match n with
              | Some n ->
                Node.span
                  ~attrs:
                    [ Attr.class_
                        "settings-group-note text-xs text-muted-foreground \
                         narrow:basis-full"
                    ]
                  [ Node.text n ]
              | None -> Node.none)
           ])
    ; Node.div
        ~attrs:
          [ Attr.class_
              "settings-card overflow-clip rounded-lg border border-border \
               bg-card text-card-foreground"
          ]
        rows
    ]
;;

let switch ?(disabled = false) ~label ~title ~on ~on_click () =
  Loon.Switch.view
    ~class_:
      ("switch narrow:h-11! narrow:w-11! narrow:border-0 \
        narrow:bg-transparent! narrow:px-[7px] narrow:before:absolute \
        narrow:before:inset-x-[6px] narrow:before:inset-y-[13px] \
        narrow:before:rounded-full narrow:before:bg-input \
        narrow:data-checked:before:bg-primary narrow:before:transition-colors \
        narrow:[&>span]:relative narrow:-my-3"
       ^ if on then " on" else "")
    ~attrs:[ Attr.create "aria-label" label; Attr.title title ]
    ~disabled
    ~checked:on
    ~on_change:(fun _ -> on_click)
    ()
;;

let number_switch
      ?(disabled = false)
      ~id
      ~label
      ~unit
      ~value
      ~placeholder
      ~title
      ~switch_title
      ~invalid
      ~on
      ~on_input
      ~on_commit
      ~on_switch
      ()
  =
  Node.div
    ~attrs:
      [ Attr.class_ "number-switch flex items-center gap-3 narrow:gap-4"
      ; Attr.role "group"
      ; Attr.create "aria-label" label
      ]
    [ (if on
       then
         Node.label
           ~attrs:
             [ Attr.class_
                 "number-field flex items-center gap-1.5 whitespace-nowrap \
                  text-[13px] text-muted-foreground"
             ]
           [ Node.input
               ~attrs:
                 [ Attr.id id
                 ; Attr.class_
                     (input_cls
                        "number-field-input h-7 w-[calc(6ch+22px)] px-2 \
                         text-center font-mono text-[13px] tabular-nums \
                         narrow:w-[calc(6ch+26px)]")
                 ; Attr.type_ "text"
                 ; Attr.create "inputmode" "decimal"
                 ; Attr.value value
                 ; Attr.placeholder placeholder
                 ; Attr.title title
                 ; Attr.create "aria-label" (label ^ ", in " ^ unit)
                 ; Attr.create "aria-invalid" (Bool.to_string invalid)
                 ; Attr.create "autocomplete" "off"
                 ; Attr.create "spellcheck" "false"
                 ; (if disabled then Attr.disabled else Attr.empty)
                 ; Attr.on_input (fun _ text -> on_input text)
                 ; Attr.on_change (fun _ _ -> on_commit)
                 ]
               ()
           ; Node.span
               ~attrs:[ Attr.class_ "number-field-unit min-w-[6ch]" ]
               [ Node.text unit ]
           ]
       else Node.none)
    ; switch
        ~disabled
        ~label
        ~title:switch_title
        ~on
        ~on_click:(on_switch (not on))
        ()
    ]
;;

let select ?(disabled = false) ~cls ~title ~label ~on_click value =
  button
    ~cls:("small setting-select max-w-[240px] gap-1.5 " ^ cls)
    ~title
    ~label
    ~disabled
    ~on_click
    [ Node.span
        ~attrs:[ Attr.class_ "setting-select-value min-w-0 truncate" ]
        [ Node.text value ]
    ; icon
        ~cls:"chevron size-3.5 shrink-0 rotate-90 text-muted-foreground"
        Chevron
    ]
;;

(* One of a few, as loon's toggle group: the choices' effects keyed by their
   position. *)
let toggles ?(disabled = false) ?(cls = "") ~group_cls ~label ~item_cls choices =
  let current =
    List.find_mapi choices ~f:(fun i (_, _, on, _, _) ->
      if on then Some (Int.to_string i) else None)
  in
  Node.div
    ~attrs:[ attr_cls [ "setting-segments loon min-w-0 max-w-full"; cls ] ]
    [ Loon.Toggle_group.single
        ~attrs:[ Attr.create "aria-label" label ]
        ~class_:("max-w-full flex-wrap narrow:w-full " ^ group_cls)
        ~variant:Outline
        ~size:Sm
        ~disabled
        ~value:current
        ~on_change:(function
          | None -> Effect.Ignore
          | Some i ->
            (match List.nth choices (Int.of_string i) with
             | Some (_, _, _, _, on_click) -> on_click
             | None -> Effect.Ignore))
        (fun group ->
           List.mapi choices ~f:(fun i (text, title, on, face, _) ->
             Loon.Toggle_group.item
               group
               ~value:(Int.to_string i)
               ~attrs:
                 [ Attr.class_
                     (cn [ "segment"; (if on then "on" else ""); item_cls ])
                 ; Attr.create "aria-label" text
                 ; Attr.title title
                 ]
               face))
    ]
;;

let segmented ?disabled ~label choices =
  toggles
    ?disabled
    ~label
    ~group_cls:""
    ~item_cls:"narrow:h-11 narrow:flex-1 narrow:px-3"
    (List.map choices ~f:(fun (text, title, on, on_click) ->
       text, title, on, [ Node.text text ], on_click))
;;

(* On a desktop the line about each choice is only its tooltip; on a phone
   the choices are a list of rows, the line under each name. *)
let choice_list ?disabled ~label choices =
  toggles
    ?disabled
    ~label
    ~cls:"choice-list"
    ~group_cls:"narrow:flex-col narrow:items-stretch"
    ~item_cls:
      "narrow:h-auto narrow:min-h-11 narrow:w-full narrow:justify-start \
       narrow:py-2 narrow:text-left"
    (List.map choices ~f:(fun (text, about, title, on, on_click) ->
       ( text
       , title
       , on
       , [ Node.span
             ~attrs:[ Attr.class_ "flex min-w-0 flex-col items-start gap-0.5" ]
             [ Node.span
                 ~attrs:
                   [ Attr.class_
                       "segment-name narrow:text-[15px] narrow:font-medium"
                   ]
                 [ Node.text text ]
             ; Node.span
                 ~attrs:
                   [ Attr.class_
                       "segment-about hidden whitespace-normal text-[13px] \
                        leading-snug text-muted-foreground narrow:block"
                   ]
                 [ Node.text about ]
             ]
         ]
       , on_click )))
;;
