open! Core
open! Import
module Calculator = Ui_spec.Calculator

let title_node = function
  | Some t -> Node.div ~attrs:[ Attr.class_ "ui-title" ] [ Node.text t ]
  | None -> Node.none
;;

let float_to_string f = Ui_format.number f |> String.filter ~f:(fun c -> not (Char.equal c ','))
let input_value (i : Calculator.Input.t) ~value =
  Option.value (value i.name) ~default:(float_to_string i.default)
;;

let format_output (o : Calculator.Output.t) f =
  let text =
    match o.format with
    | Number -> Ui_format.number ?decimals:o.decimals f
    | Integer -> Ui_format.number ~decimals:0 f
    | Percent -> Ui_format.percent ?decimals:o.decimals f
    | Currency -> Ui_format.currency ?decimals:o.decimals ~code:"USD" f
  in
  match o.unit with
  | Some u -> text ^ " " ^ u
  | None -> text
;;

let results (t : Calculator.t) ~value =
  let env name =
    List.find t.inputs ~f:(fun i -> String.equal i.name name)
    |> Option.bind ~f:(fun i ->
      Float.of_string_opt (String.strip (input_value i ~value)))
  in
  List.map t.outputs ~f:(fun (o : Calculator.Output.t) ->
    let result =
      match Ui_expr.parse o.expr with
      | Error e -> Error ("can't read: " ^ Error.to_string_hum e)
      | Ok expr ->
        (match
           List.find (Ui_expr.names expr) ~f:(fun n -> Option.is_none (env n))
         with
         | Some n ->
           (match List.find t.inputs ~f:(fun i -> String.equal i.name n) with
            | Some i -> Error (sprintf "enter a number for %s" i.label)
            | None -> Error (sprintf "no input %S" n))
         | None ->
           (match Ui_expr.eval expr ~env with
            | Ok f when Float.is_finite f -> Ok (format_output o f)
            | Ok _ -> Error "out of range"
            | Error e -> Error (Error.to_string_hum e)))
    in
    o.label, result)
;;

let calculator (ctx : Ui_context.t) ~path (t : Calculator.t) =
  let value name = Ui_context.get ctx ~path ("in." ^ name) in
  let set name v = Ui_context.set ctx ~path ("in." ^ name) v in
  let input (i : Calculator.Input.t) =
    let v = input_value i ~value in
    let id = sprintf "ui-%s-%s-%s" ctx.call_id path i.name in
    let opt f = Option.value_map ~default:[] ~f:(fun x -> [ f x ]) in
    let bounds =
      opt (fun x -> Attr.min x) i.min
      @ opt (fun x -> Attr.max x) i.max
      @ opt (fun x -> Attr.create "step" (float_to_string x)) i.step
    in
    Node.div
      ~attrs:[ Attr.class_ "ui-calc-input" ]
      [ Node.label ~attrs:[ Attr.for_ id ] [ Node.text i.label ]
      ; Node.div
          ~attrs:[ Attr.class_ "ui-calc-field" ]
          [ Node.input
              ~attrs:
                ([ Attr.id id
                 ; Attr.type_ "number"
                 ; Attr.create "inputmode" "decimal"
                 ; Attr.value v
                 ; Attr.on_input (fun _ s -> set i.name s)
                 ]
                 @ bounds)
              ()
          ; (match i.unit with
             | Some u -> Node.span ~attrs:[ Attr.class_ "ui-unit" ] [ Node.text u ]
             | None -> Node.none)
          ]
      ; (match i.slider, i.min, i.max with
         | true, Some _, Some _ ->
           Node.input
             ~attrs:
               ([ Attr.type_ "range"
                ; Attr.class_ "ui-slider"
                ; Attr.create "aria-label" i.label
                ; Attr.value v
                ; Attr.on_input (fun _ s -> set i.name s)
                ]
                @ bounds)
             ()
         | _ -> Node.none)
      ]
  in
  let output (label, result) =
    Node.div
      ~attrs:[ Attr.class_ "ui-calc-output" ]
      [ Node.div ~attrs:[ Attr.class_ "ui-calc-label" ] [ Node.text label ]
      ; (match result with
         | Ok text ->
           Node.output ~attrs:[ Attr.class_ "ui-calc-value" ] [ Node.text text ]
         | Error why ->
           Node.output
             ~attrs:[ Attr.class_ "ui-calc-value none"; Attr.title why ]
             [ Node.text "—"; Node.span ~attrs:[ Attr.class_ "ui-calc-why" ] [ Node.text why ] ])
      ]
  in
  Node.div
    ~attrs:[ Attr.class_ "ui-calc" ]
    [ title_node t.title
    ; Node.div
        ~attrs:[ Attr.class_ "ui-calc-body" ]
        [ Node.div ~attrs:[ Attr.class_ "ui-calc-inputs" ] (List.map t.inputs ~f:input)
        ; Node.div
            ~attrs:[ Attr.class_ "ui-calc-outputs"; Attr.create "aria-live" "polite" ]
            (List.map (results t ~value) ~f:output)
        ]
    ]
;;

let int_state ctx ~path name ~default =
  Option.bind (Ui_context.get ctx ~path name) ~f:Int.of_string_opt
  |> Option.value ~default
;;

let flashcards (ctx : Ui_context.t) ~path ~title cards =
  let n = List.length cards in
  let i = Int.clamp_exn (int_state ctx ~path "i" ~default:0) ~min:0 ~max:(Int.max 0 (n - 1)) in
  let flipped = String.equal (Option.value (Ui_context.get ctx ~path "flip") ~default:"") "1" in
  let go j =
    Effect.Many
      [ Ui_context.set ctx ~path "i" (Int.to_string j); Ui_context.set ctx ~path "flip" "0" ]
  in
  match List.nth cards i with
  | None -> Node.none
  | Some (front, back) ->
    let face cls label text =
      Node.div
        ~attrs:[ Attr.class_ ("ui-face " ^ cls); Attr.create "aria-hidden" (Bool.to_string (Bool.( <> ) (String.equal cls "back") flipped)) ]
        [ Node.span ~attrs:[ Attr.class_ "ui-face-label" ] [ Node.text label ]
        ; Ui_context.md text
        ]
    in
    let nav ~label ~text ~target ~enabled =
      Node.button
        ~attrs:
          ([ Attr.type_ "button"
           ; Attr.class_ "ui-btn ghost"
           ; Attr.create "aria-label" label
           ; Attr.on_click (fun _ -> go target)
           ]
           @ if enabled then [] else [ Attr.disabled ])
        [ Node.text text ]
    in
    Node.div
      ~attrs:[ Attr.class_ "ui-flash" ]
      [ title_node title
      ; Node.button
          ~attrs:
            [ Attr.type_ "button"
            ; Attr.classes ([ "ui-flash-card" ] @ if flipped then [ "flipped" ] else [])
            ; Attr.create "aria-pressed" (Bool.to_string flipped)
            ; Attr.create "aria-label" (sprintf "Card %d of %d: %s. Flip" (i + 1) n (if flipped then "back" else "front"))
            ; Attr.on_click (fun _ ->
                Ui_context.set ctx ~path "flip" (if flipped then "0" else "1"))
            ]
          [ Node.div
              ~attrs:[ Attr.class_ "ui-flash-inner" ]
              [ face "front" "Question" front; face "back" "Answer" back ]
          ]
      ; Node.div
          ~attrs:[ Attr.class_ "ui-flash-nav" ]
          [ nav ~label:"Previous card" ~text:"‹ Prev" ~target:(i - 1) ~enabled:(i > 0)
          ; Node.span
              ~attrs:[ Attr.class_ "ui-count"; Attr.create "aria-live" "polite" ]
              [ Node.text (sprintf "%d / %d" (i + 1) n) ]
          ; nav ~label:"Next card" ~text:"Next ›" ~target:(i + 1) ~enabled:(i < n - 1)
          ]
      ]
;;

let quiz (ctx : Ui_context.t) ~path ~title (questions : Ui_spec.Question.t list) =
  let chosen k = Option.bind (Ui_context.get ctx ~path (sprintf "q%d" k)) ~f:Int.of_string_opt in
  let answered = List.filter_mapi questions ~f:(fun k q -> Option.map (chosen k) ~f:(fun c -> c = q.answer)) in
  let correct = List.count answered ~f:Fn.id in
  let total = List.length questions in
  let question k (q : Ui_spec.Question.t) =
    let choice = chosen k in
    let option j text =
      let state =
        match choice with
        | None -> []
        | Some c ->
          if j = q.answer then [ "correct" ] else if j = c then [ "wrong" ] else [ "other" ]
      in
      Node.button
        ~attrs:
          ([ Attr.type_ "button"
           ; Attr.classes ([ "ui-option" ] @ state)
           ; Attr.role "radio"
           ; Attr.create "aria-checked" (Bool.to_string (Option.equal Int.equal choice (Some j)))
           ]
           @
           match choice with
           | Some _ -> [ Attr.create "aria-disabled" "true" ]
           | None ->
             [ Attr.on_click (fun _ -> Ui_context.set ctx ~path (sprintf "q%d" k) (Int.to_string j)) ])
        [ Node.span ~attrs:[ Attr.class_ "ui-option-key" ] [ Node.text (String.of_char (Char.of_int_exn (65 + j))) ]
        ; Node.span ~attrs:[ Attr.class_ "ui-option-text" ] [ Node.text text ]
        ; (match state with
           | [ "correct" ] -> Node.span ~attrs:[ Attr.class_ "ui-option-mark" ] [ Node.text "✓" ]
           | [ "wrong" ] -> Node.span ~attrs:[ Attr.class_ "ui-option-mark" ] [ Node.text "✗" ]
           | _ -> Node.none)
        ]
    in
    Node.div
      ~attrs:[ Attr.class_ "ui-question" ]
      [ Node.div
          ~attrs:[ Attr.class_ "ui-question-head" ]
          [ Node.span ~attrs:[ Attr.class_ "ui-question-n" ] [ Node.text (sprintf "%d" (k + 1)) ]
          ; Ui_context.md q.question
          ]
      ; Node.div
          ~attrs:[ Attr.class_ "ui-options"; Attr.role "radiogroup" ]
          (List.mapi q.options ~f:option)
      ; (match choice, q.explanation with
         | Some c, explanation ->
           Node.div
             ~attrs:[ Attr.classes [ "ui-verdict"; (if c = q.answer then "right" else "wrong") ]; Attr.create "aria-live" "polite" ]
             [ Node.strong [ Node.text (if c = q.answer then "Correct." else "Not quite.") ]
             ; (match explanation with
                | Some e -> Ui_context.md e
                | None -> Node.none)
             ]
         | None, _ -> Node.none)
      ]
  in
  Node.div
    ~attrs:[ Attr.class_ "ui-quiz" ]
    [ Node.div
        ~attrs:[ Attr.class_ "ui-quiz-head" ]
        [ title_node title
        ; Node.span
            ~attrs:[ Attr.class_ "ui-score"; Attr.create "aria-live" "polite" ]
            [ Node.text
                (if List.is_empty answered
                 then sprintf "%d question%s" total (if total = 1 then "" else "s")
                 else sprintf "%d / %d correct" correct (List.length answered)
                      ^ if List.length answered < total then sprintf " · %d left" (total - List.length answered) else "")
            ]
        ; (if List.is_empty answered
           then Node.none
           else
             Node.button
               ~attrs:
                 [ Attr.type_ "button"
                 ; Attr.class_ "ui-btn ghost small"
                 ; Attr.on_click (fun _ ->
                     Effect.Many (List.mapi questions ~f:(fun k _ -> Ui_context.set ctx ~path (sprintf "q%d" k) "")))
                 ]
               [ Node.text "Retry" ])
        ]
    ; Node.div ~attrs:[ Attr.class_ "ui-questions" ] (List.mapi questions ~f:question)
    ]
;;
