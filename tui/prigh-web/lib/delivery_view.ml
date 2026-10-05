open! Core
open! Import
open Chat_html

let section (section : Prigh_ui.Delivery.Section.t) =
  let job = String.equal section.kind "job" in
  let report =
    Subagent_report.of_string (String.concat ~sep:"\n" section.body)
  in
  let body =
    if job then String.concat ~sep:"\n" section.body else report.text
  in
  let head =
    div
      "delivery-head"
      [ span "icon" "↩"
      ; span "kind" (sprintf "%s %s" section.kind section.id)
      ; span (if section.ok then "chip ok" else "chip bad") section.status
      ; span "task" section.task
      ; (match report.stats with
         | Some stats when not job -> span "stats" stats
         | _ -> Node.none)
      ]
  in
  div
    (if section.ok then "delivery-section ok" else "delivery-section bad")
    [ head
    ; (if String.is_empty (String.strip body)
       then Node.none
       else if job
       then
         folded
           ~cls:"delivery-body"
           ~label:"Output"
           ~preview:(first_line body)
           (Output_view.view ~error:(not section.ok) ~head:0 ~tail:20 body)
       else
         folded
           ~cls:"delivery-body"
           ~label:"Report"
           ~preview:(Markdown.preview body)
           (Markdown_view.render body))
    ]
;;

let view sections = div "delivery" (List.map sections ~f:section)
