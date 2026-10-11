open! Core
open! Import

type t =
  { verbose : bool
  ; toggled : String.Set.t
  ; scope : string
  }
[@@deriving sexp_of]

let create ~(verbosity : Prigh_ui.Verbosity.t) ~toggled =
  { verbose =
      (match verbosity with
       | Verbose -> true
       | Quiet | Normal -> false)
  ; toggled
  ; scope = ""
  }
;;

let default = create ~verbosity:Normal ~toggled:String.Set.empty
let scoped t agent_id = { t with scope = agent_id ^ "/" }
let key t id = t.scope ^ id
let expanded t id = Bool.( <> ) t.verbose (Set.mem t.toggled (key t id))

let equal a b =
  phys_equal a b
  || (Bool.equal a.verbose b.verbose
      && String.equal a.scope b.scope
      && (phys_equal a.toggled b.toggled || Set.equal a.toggled b.toggled))
;;

let attribute = "data-fold"
let toggle_attr t id = Attr.create attribute (key t id)

(* What a fold's button says: how to show all [total] lines, [shown] of which
   show while folded, or how to fold them again. *)
let label t id ~total ~shown =
  if expanded t id
  then
    if shown = 0
    then "Fold"
    else sprintf "Fold to %s" (Chat_html.plural shown "line")
  else if shown = 0
  then sprintf "Show %s" (Chat_html.plural total "line")
  else
    sprintf
      "Show all %s (%d hidden)"
      (Chat_html.plural total "line")
      (total - shown)
;;

let button t id ~total ~shown =
  let expanded = expanded t id in
  let label = label t id ~total ~shown in
  Node.button
    ~attrs:
      [ Attr.class_ "fold"
      ; Attr.type_ "button"
      ; toggle_attr t id
      ; Attr.create "aria-expanded" (Bool.to_string expanded)
      ]
    [ Node.span ~attrs:[ Attr.class_ "chev" ] []; Node.text label ]
;;
