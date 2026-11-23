open! Core
open! Import
open Html
module Model = App.Model
module Action = App.Action

(* Sonner's toast (shadcn's Toaster: the popover's colours, its border, a
   lg radius and shadow), an icon by its type, the text, an action button
   and a close. Hook classes: toast, error, update-notice, toast-undo,
   toast-close, toast-repeats. *)
let look =
  "flex items-start gap-2 rounded-lg border border-border bg-popover px-3.5 \
   py-2.5 text-[13px] leading-normal text-popover-foreground shadow-lg"
;;

let action ~title ~on_click text =
  Node.button
    ~attrs:
      [ Attr.type_ "button"
      ; Attr.class_
          "toast-undo ml-1 inline-flex h-6 shrink-0 cursor-pointer items-center \
           rounded-[4px] bg-foreground px-2 text-xs font-medium text-background \
           hover:bg-foreground/85"
      ; Attr.title title
      ; Attr.create "data-button" ""
      ; Attr.on_click (fun _ -> on_click)
      ]
    [ Node.text text ]
;;

let close ~title ~on_click =
  Html.button
    ~cls:"toast-close -my-0.5 -mr-1.5 text-muted-foreground hover:text-foreground"
    ~variant:Ghost
    ~size:Icon_xs
    ~title
    ~label:"Dismiss"
    ~attrs:[ Attr.create "data-close-button" "" ]
    ~on_click
    [ icon Close ]
;;

(* Stays until answered: reloading mid-run is the user's call. *)
let update_notice (m : Model.t) ~inject =
  if not m.update_ready
  then []
  else
    [ Node.div
        ~attrs:
          [ Attr.class_ ("toast update-notice cursor-default " ^ look)
          ; Attr.role "status"
          ; Attr.create "data-sonner-type" "info"
          ]
        [ Loon.Icon.view
            ~class_:"mt-0.5 size-4 shrink-0 text-primary"
            Circle_fading_arrow_up
        ; Node.span
            ~attrs:[ Attr.class_ "min-w-0 flex-1 font-medium" ]
            [ Node.text "A new version of prigh-web is ready." ]
        ; action
            ~title:"Reload the page into it, in this session"
            ~on_click:(inject Action.Apply_update)
            "Reload"
        ; close
            ~title:"Later (the next page load uses it)"
            ~on_click:(inject Action.Dismiss_update)
        ]
    ]
;;

let toast ~inject (t : App.Toast.t) =
  let shown = Toast.short t.text in
  Node.div
    ~attrs:
      [ Attr.class_ (if t.error then "toast error " ^ look else "toast " ^ look)
      ; Attr.role (if t.error then "alert" else "status")
      ; Attr.create "data-sonner-type" (if t.error then "error" else "success")
      ; Attr.title
          (if String.equal shown t.text
           then "Dismiss"
           else t.text ^ " (click to dismiss)")
      ; Attr.on_click (fun _ -> inject (Action.Dismiss_toast t.id))
      ]
    [ (if t.error
       then
         Loon.Icon.view ~class_:"mt-0.5 size-4 shrink-0 text-destructive" Circle_x
       else Node.none)
    ; Node.span
        ~attrs:[ Attr.class_ "min-w-0 flex-1 font-medium"; Attr.create "dir" "auto" ]
        [ Node.text shown ]
    ; (if t.repeats > 0
       then
         span
           ~cls:"toast-repeats font-mono text-[11.5px] leading-[1.7] text-muted-foreground"
           (sprintf "×%d" (t.repeats + 1))
       else Node.none)
    ; Option.value_map t.undo ~default:Node.none ~f:(fun h ->
        action
          ~title:
            "Bring it back into the transcript and the model's context \
             (/delete undo)"
          ~on_click:
            (Effect.Many [ Effect.Stop_propagation; inject (Action.Restore_hidden h) ])
          "Undo")
    ; (if t.error
       then close ~title:"Dismiss" ~on_click:(inject (Action.Dismiss_toast t.id))
       else Node.none)
    ]
;;

let view (m : Model.t) ~inject =
  div
    ~cls:"toast-anchor"
    [ div
        ~cls:"toasts"
        ~attrs:[ Attr.create "aria-live" "polite" ]
        (update_notice m ~inject @ List.map m.toasts ~f:(toast ~inject))
    ]
;;
