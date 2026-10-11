open! Core
open! Import

type t =
  { older : int
  ; before : string option
  ; loading : bool
  }
[@@deriving sexp_of, equal]

let all = { older = 0; before = None; loading = false }

let of_page (page : Message_page.t) =
  { older = page.older; before = page.before; loading = false }
;;

let size = 60

let next t =
  match t.before with
  | Some before when t.older > 0 && not t.loading -> Some before
  | Some _ | None -> None
;;

let view t ~load =
  if t.older = 0 || Option.is_none t.before
  then Node.none
  else if t.loading
  then
    Node.div
      ~attrs:[ Attr.class_ "older busy"; Attr.role "status" ]
      [ Html.lamp (); Node.text "Loading earlier messages…" ]
  else
    Node.div
      ~attrs:[ Attr.class_ "older"; Attr.create "data-older" "" ]
      [ Node.button
          ~attrs:
            [ Attr.class_ "load-older"
            ; Attr.type_ "button"
            ; Attr.on_click (fun _ -> load)
            ]
          [ Node.text "Show earlier messages" ]
      ; Node.span
          ~attrs:[ Attr.class_ "count" ]
          [ Node.text (sprintf "%d more" t.older) ]
      ]
;;
