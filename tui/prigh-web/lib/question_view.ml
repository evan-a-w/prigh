open! Core
open! Import
open Html
module Form = Question_form

let title (form : Form.t) (q : Question.t) =
  let total = List.length (Form.questions form) in
  let index = Form.current form + 1 in
  match q.header with
  | Some header when total > 1 -> sprintf "%s (%d/%d)" header index total
  | Some header -> header
  | None when total > 1 -> sprintf "Question %d/%d" index total
  | None -> "Question"
;;

let choice form (q : Question.t) ~question ~inject i (c : Question.Choice.t) =
  let picked = Form.is_picked form ~question ~choice:i in
  Node.div
    ~attrs:
      [ classes
          [ "question-choice" ]
          [ "selected", Form.cursor form = i; "picked", picked ]
      ; Attr.role (if q.multiple then "checkbox" else "radio")
      ; Attr.create "aria-checked" (Bool.to_string picked)
      ; Attr.on_click (fun _ -> inject (Question_dialog.Action.Choose i))
      ]
    [ span
        ~cls:"question-mark"
        (match q.multiple, picked with
         | true, true -> "☑"
         | true, false -> "☐"
         | false, true -> "◉"
         | false, false -> "○")
    ; span ~cls:"question-label" c.label
    ; Option.value_map c.description ~default:Node.none ~f:(fun d ->
        span ~cls:"question-detail" d)
    ]
;;

let own form (q : Question.t) ~question ~inject =
  let row = List.length q.choices in
  div
    ~attrs:
      [ classes [ "question-own" ] [ "selected", Form.on_own_answer form ] ]
    [ (if List.is_empty q.choices
       then Node.none
       else
         Node.label ~attrs:[ Attr.for_ "question-own" ] [ Node.text "Other:" ])
    ; Node.input
        ~attrs:
          [ Attr.id "question-own"
          ; Attr.class_ "text-input"
          ; Attr.type_ "text"
          ; Attr.placeholder
              (if List.is_empty q.choices
               then "Your answer"
               else "Your own answer")
          ; Attr.value (Form.own_answer form ~question)
          ; Attr.create "autocomplete" "off"
          ; Attr.on_input (fun _ text ->
              inject (Question_dialog.Action.Set_own text))
          ; Attr.on_click (fun _ -> inject (Question_dialog.Action.Row row))
          ]
        ()
    ]
;;

let view (questions : Question_dialog.t list) ~inject =
  let inject (a : Question_dialog.Action.t) = inject a in
  match questions with
  | [] -> Node.none
  | { form; _ } :: rest ->
    let question = Form.current form in
    let total = List.length (Form.questions form) in
    let q = List.nth_exn (Form.questions form) question in
    let last = question = total - 1 in
    let hint =
      if q.multiple
      then "Space toggles"
      else if List.is_empty q.choices
      then "type your answer"
      else "Space or a click picks"
    in
    Modal.view
      ~id:"question"
      ~cls:"question-dialog"
      ~title:(title form q)
      ~on_close:(inject Decline)
      ~footer:
        [ span
            ~cls:"keys-hint"
            (String.concat
               ~sep:", "
               ([ hint; "↑↓ choose"; "Enter accepts"; "Esc declines" ]
                @
                match rest with
                | [] -> []
                | rest -> [ sprintf "%d more waiting" (List.length rest) ]))
        ; button ~title:"Esc" ~on_click:(inject Decline) [ Node.text "Decline" ]
        ; (if question > 0
           then button ~title:"←" ~on_click:(inject Prev) [ Node.text "Back" ]
           else Node.none)
        ; button
            ~cls:"primary"
            ~title:"Enter"
            ~on_click:(inject Accept)
            [ Node.text (if last then "Send" else "Next") ]
        ]
      [ Node.p ~attrs:[ Attr.class_ "question-text" ] [ Node.text q.question ]
      ; (match q.choices with
         | [] -> Node.none
         | choices ->
           div
             ~cls:"question-choices"
             ~attrs:[ Attr.role (if q.multiple then "group" else "radiogroup") ]
             (List.mapi choices ~f:(choice form q ~question ~inject)))
      ; own form q ~question ~inject
      ]
;;
