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
   show while folded (the count is data, in the mono face), or how to fold
   them again. *)
let label t id ~total ~shown =
  let count n =
    Node.span ~attrs:[ Attr.class_ "count" ] [ Node.text (Int.to_string n) ]
  in
  let lines n = Node.text (if n = 1 then " line" else " lines") in
  if expanded t id
  then
    if shown = 0
    then [ Node.text "Fold" ]
    else [ Node.text "Fold to "; count shown; lines shown ]
  else if shown = 0
  then [ Node.text "Show "; count total; lines total ]
  else [ Node.text "Show all "; count total; lines total ]
;;

let button t id ~total ~shown =
  let expanded = expanded t id in
  Node.button
    ~attrs:
      [ Attr.class_ "fold"
      ; Attr.type_ "button"
      ; toggle_attr t id
      ; Attr.create "aria-expanded" (Bool.to_string expanded)
      ]
    [ Node.span ~attrs:[ Attr.class_ "chev" ] []
    ; Node.span (label t id ~total ~shown)
    ]
;;
