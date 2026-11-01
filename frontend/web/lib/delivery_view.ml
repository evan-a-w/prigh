open! Core
open! Import
open Chat_html

let section
      ?(name = fun _ -> None)
      ~fold
      ~key
      (section : Prigh_ui.Delivery.Section.t)
  =
  let job = String.equal section.kind "job" in
  let report =
    Subagent_report.of_string (String.concat ~sep:"\n" section.body)
  in
  let body =
    if job then String.concat ~sep:"\n" section.body else report.text
  in
  (* What came back and how it went; which agent it was and what it cost
     are in the tooltip. A job keeps its id: the agent refers to it. *)
  let head =
    Node.div
      ~attrs:
        ([ Attr.class_ "delivery-head" ]
         @ (if job then [] else [ Attr.create "data-agent" section.id ])
         @ [ Attr.title
               (String.concat
                  ~sep:" · "
                  (sprintf "%s %s" section.kind section.id
                   :: Option.to_list (if job then None else report.stats)))
           ])
      (present
         [ span "icon" "↩"
         ; (if job then span "kind" (sprintf "job %s" section.id) else Node.none)
         ; span
             "task"
             (Option.value
                (if job then None else name section.id)
                ~default:section.task)
         ; span (if section.ok then "chip ok" else "chip bad") section.status
         ; (if job
            then Node.none
            else
              Node.button
                ~attrs:
                  [ Attr.class_ "open-agent"
                  ; Attr.type_ "button"
                  ; Attr.create "aria-label" "Open in the agents panel"
                  ]
                [ Node.text "Open ›" ])
         ])
  in
  div
    (if section.ok then "delivery-section ok" else "delivery-section bad")
    [ head
    ; (if String.is_empty (String.strip body)
       then Node.none
       else if job
       then
         (* A job's last lines say how it went. *)
         Output_view.view
           ~fold:(fold, key section)
           ~error:(not section.ok)
           ~from_end:true
           ~lines:5
           body
       else
         folded
           ~cls:"delivery-body"
           ~label:"Report"
           ~preview:(Markdown.preview body)
           (Markdown_view.render body))
    ]
;;

let job_key (section : Prigh_ui.Delivery.Section.t) = "job:" ^ section.id

let view ?name ~fold ?(key = job_key) sections =
  div "delivery" (List.map sections ~f:(section ?name ~fold ~key))
;;
