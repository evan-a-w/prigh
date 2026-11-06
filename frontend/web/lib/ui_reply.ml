open! Core
open! Import
module Field = Ui_spec.Field

let title_node = function
  | Some t -> Node.div ~attrs:[ Attr.class_ "ui-title" ] [ Node.text t ]
  | None -> Node.none
;;

let number_text f =
  Ui_format.number f |> String.filter ~f:(fun c -> not (Char.equal c ','))
;;

(* A field's state is a string: a checkbox's is its ticked options as a
   JSON array, a toggle's [true] or [false]. *)
let encode (f : Field.t) (j : Json.t) =
  match f.kind, j with
  | Checkbox, `Array items ->
    Json.to_string
      (`Array
          (List.filter items ~f:(function
             | `String _ -> true
             | _ -> false)))
  | Checkbox, `String s -> Json.to_string (`Array [ `String s ])
  | Toggle, (`True | `String "true") -> "true"
  | Toggle, _ -> "false"
  | _, `String s -> s
  | _, `Number n -> n
  | _, `True -> "true"
  | _, `False -> "false"
  | _, _ -> ""
;;

let initial (f : Field.t) =
  match f.default with
  | Some j -> encode f j
  | None ->
    (match f.kind with
     | Checkbox -> "[]"
     | Toggle -> "false"
     | Slider -> number_text (Option.value f.min ~default:0.)
     | Select -> Option.value (List.hd f.options) ~default:""
     | Text | Textarea | Number | Radio | Date -> "")
;;

let ticked s =
  match Json.parse s with
  | Ok (`Array items) ->
    List.filter_map items ~f:(function
      | `String s -> Some s
      | _ -> None)
  | _ -> []
;;

let current (f : Field.t) ~value =
  Option.value (value f.name) ~default:(initial f)
;;

let form_values fields ~value =
  let rec go acc = function
    | [] -> Ok (`Object (List.rev acc))
    | (f : Field.t) :: rest ->
      let s = current f ~value in
      let v : (Json.t option, string) Result.t =
        match f.kind with
        | Number | Slider ->
          (match String.strip s with
           | "" -> Ok None
           | s ->
             (match Float.of_string_opt s with
              | Some x -> Ok (Some (Json.float x))
              | None -> Error (sprintf "%s: enter a number" f.label)))
        | Toggle -> Ok (Some (Json.bool (String.equal s "true")))
        | Checkbox ->
          (match ticked s with
           | [] -> Ok None
           | l -> Ok (Some (`Array (List.map l ~f:Json.str))))
        | Text | Textarea | Select | Radio | Date ->
          if String.is_empty (String.strip s)
          then Ok None
          else Ok (Some (Json.str s))
      in
      (match v with
       | Error e -> Error e
       | Ok None when f.required -> Error (sprintf "%s is required" f.label)
       | Ok None ->
         let empty : Json.t =
           match f.kind with
           | Checkbox -> `Array []
           | Number | Slider -> `Null
           | _ -> Json.str ""
         in
         go ((f.name, empty) :: acc) rest
       | Ok (Some v) -> go ((f.name, v) :: acc) rest)
  in
  go [] fields
;;

let sent_chip = function
  | true -> Node.span ~attrs:[ Attr.class_ "ui-sent" ] [ Node.text "✓ Sent" ]
  | false -> Node.none
;;

let send (ctx : Ui_context.t) ~component ~title values =
  ctx.inject (Send { call_id = ctx.call_id; component; title; values })
;;

let field (ctx : Ui_context.t) ~path ~sent ~value ~set (f : Field.t) =
  let id = sprintf "ui-%s-%s-%s" ctx.call_id path f.name in
  let v = current f ~value in
  let disabled = if sent then [ Attr.disabled ] else [] in
  let on_input = Attr.on_input (fun _ s -> set f.name s) in
  let opt g = Option.value_map ~default:[] ~f:(fun x -> [ g x ]) in
  let bounds =
    opt (fun x -> Attr.min x) f.min
    @ opt (fun x -> Attr.max x) f.max
    @ opt (fun x -> Attr.create "step" (number_text x)) f.step
  in
  let common =
    [ Attr.id id; Attr.name f.name ]
    @ opt Attr.placeholder f.placeholder
    @ (if f.required then [ Attr.create "aria-required" "true" ] else [])
    @ disabled
  in
  let label =
    Node.label
      ~attrs:[ Attr.for_ id; Attr.class_ "ui-label" ]
      [ Node.text f.label
      ; (if f.required
         then
           Node.span
             ~attrs:[ Attr.class_ "ui-req"; Attr.title "Required" ]
             [ Node.text " *" ]
         else Node.none)
      ]
  in
  let group ~role items =
    Node.div
      ~attrs:
        [ Attr.class_ "ui-group"
        ; Attr.role role
        ; Attr.create "aria-labelledby" (id ^ "-l")
        ]
      items
  in
  let group_label =
    Node.div
      ~attrs:[ Attr.id (id ^ "-l"); Attr.class_ "ui-label" ]
      [ Node.text f.label
      ; (if f.required
         then Node.span ~attrs:[ Attr.class_ "ui-req" ] [ Node.text " *" ]
         else Node.none)
      ]
  in
  let control =
    match f.kind with
    | Text | Date | Number ->
      [ label
      ; Node.input
          ~attrs:
            (common
             @ [ Attr.type_
                   (match f.kind with
                    | Date -> "date"
                    | Number -> "number"
                    | _ -> "text")
               ; Attr.class_ "ui-input"
               ; Attr.value v
               ; on_input
               ]
             @
             match f.kind with
             | Number -> Attr.create "inputmode" "decimal" :: bounds
             | _ -> [])
          ()
      ]
    | Textarea ->
      [ label
      ; Node.textarea
          ~attrs:
            (common
             @ [ Attr.class_ "ui-input"
               ; Attr.value v
               ; Attr.create "rows" "3"
               ; on_input
               ])
          []
      ]
    | Slider ->
      [ label
      ; Node.div
          ~attrs:[ Attr.class_ "ui-slider-row" ]
          [ Node.input
              ~attrs:
                (common
                 @ [ Attr.type_ "range"
                   ; Attr.class_ "ui-slider"
                   ; Attr.value v
                   ; on_input
                   ]
                 @ bounds)
              ()
          ; Node.output
              ~attrs:[ Attr.for_ id; Attr.class_ "ui-slider-value" ]
              [ Node.text v ]
          ]
      ]
    | Select ->
      [ label
      ; Node.select
          ~attrs:
            (common
             @ [ Attr.class_ "ui-input"
               ; Attr.on_change (fun _ s -> set f.name s)
               ])
          (List.map f.options ~f:(fun o ->
             Node.option
               ~attrs:
                 ([ Attr.value o ]
                  @ if String.equal o v then [ Attr.selected ] else [])
               [ Node.text o ]))
      ]
    | Radio ->
      [ group_label
      ; group
          ~role:"radiogroup"
          (List.mapi f.options ~f:(fun k o ->
             let checked = String.equal o v in
             Node.label
               ~attrs:
                 [ Attr.classes
                     ([ "ui-check" ] @ if checked then [ "on" ] else [])
                 ]
               [ Node.input
                   ~attrs:
                     ([ Attr.type_ "radio"
                      ; Attr.name id
                      ; Attr.id (sprintf "%s-%d" id k)
                      ; Attr.checked_prop checked
                      ; Attr.on_change (fun _ _ -> set f.name o)
                      ]
                      @ disabled)
                   ()
               ; Node.span [ Node.text o ]
               ]))
      ]
    | Checkbox ->
      let on = ticked v in
      [ group_label
      ; group
          ~role:"group"
          (List.mapi f.options ~f:(fun k o ->
             let checked = List.mem on o ~equal:String.equal in
             let next =
               if checked
               then List.filter on ~f:(fun x -> not (String.equal x o))
               else
                 List.filter f.options ~f:(fun x ->
                   String.equal x o || List.mem on x ~equal:String.equal)
             in
             Node.label
               ~attrs:
                 [ Attr.classes
                     ([ "ui-check" ] @ if checked then [ "on" ] else [])
                 ]
               [ Node.input
                   ~attrs:
                     ([ Attr.type_ "checkbox"
                      ; Attr.id (sprintf "%s-%d" id k)
                      ; Attr.checked_prop checked
                      ; Attr.on_change (fun _ _ ->
                          set
                            f.name
                            (Json.to_string
                               (`Array (List.map next ~f:Json.str))))
                      ]
                      @ disabled)
                   ()
               ; Node.span [ Node.text o ]
               ]))
      ]
    | Toggle ->
      let on = String.equal v "true" in
      [ Node.label
          ~attrs:[ Attr.class_ "ui-toggle-row"; Attr.for_ id ]
          [ Node.span ~attrs:[ Attr.class_ "ui-label" ] [ Node.text f.label ]
          ; Node.button
              ~attrs:
                ([ Attr.id id
                 ; Attr.type_ "button"
                 ; Attr.role "switch"
                 ; Attr.class_ "ui-switch"
                 ; Attr.create "aria-checked" (Bool.to_string on)
                 ; Attr.on_click (fun _ ->
                     set f.name (if on then "false" else "true"))
                 ]
                 @ disabled)
              [ Node.span ~attrs:[ Attr.class_ "ui-knob" ] [] ]
          ]
      ]
  in
  Node.div
    ~attrs:
      [ Attr.classes
          [ "ui-field"
          ; "ui-field-"
            ^ String.lowercase (Sexp.to_string (Field.Kind.sexp_of_t f.kind))
          ]
      ]
    (control
     @ [ (match f.help with
          | Some h -> Node.div ~attrs:[ Attr.class_ "ui-help" ] [ Node.text h ]
          | None -> Node.none)
       ])
;;

let form
      (ctx : Ui_context.t)
      ~path
      ~component
      ~title
      ~description
      fields
      ~submit
      ~heading
  =
  let sent_values = Ui_context.sent_values ctx ~component in
  let sent = Option.is_some sent_values in
  let value =
    match sent_values with
    | Some values ->
      fun name ->
        (match
           List.find fields ~f:(fun (f : Field.t) -> String.equal f.name name)
         with
         | Some f ->
           Some
             (Option.value_map
                (Json.field values name)
                ~default:""
                ~f:(encode f))
         | None -> None)
    | None -> fun name -> Ui_context.get ctx ~path ("f." ^ name)
  in
  let set name v = Ui_context.set ctx ~path ("f." ^ name) v in
  let tried = Option.is_some (Ui_context.get ctx ~path "tried") in
  let result = form_values fields ~value in
  Node.div
    ~attrs:
      [ Attr.classes
          ([ "ui-form"; "ui-reply" ] @ if sent then [ "sent" ] else [])
      ]
    [ Node.div
        ~attrs:[ Attr.class_ "ui-reply-head" ]
        [ title_node heading; sent_chip sent ]
    ; (match description with
       | Some d -> Ui_context.md ~cls:"ui-sub" d
       | None -> Node.none)
    ; Node.div
        ~attrs:[ Attr.class_ "ui-fields" ]
        (List.map fields ~f:(field ctx ~path ~sent ~value ~set))
    ; (if sent
       then Node.none
       else
         Node.div
           ~attrs:[ Attr.class_ "ui-actions" ]
           [ (match result with
              | Error why when tried ->
                Node.span
                  ~attrs:[ Attr.class_ "ui-invalid"; Attr.role "alert" ]
                  [ Node.text why ]
              | _ -> Node.none)
           ; Node.button
               ~attrs:
                 [ Attr.type_ "button"
                 ; Attr.class_ "ui-btn primary"
                 ; Attr.on_click (fun _ ->
                     match result with
                     | Ok values -> send ctx ~component ~title values
                     | Error _ -> Ui_context.set ctx ~path "tried" "1")
                 ]
               [ Node.text submit ]
           ])
    ]
;;

let tone_cls (t : Ui_spec.Tone.t) = "tone-" ^ Ui_spec.Tone.to_string t

let choices
      (ctx : Ui_context.t)
      ~path
      ~component
      ~title
      ~prompt
      (options : Ui_spec.Choice.t list)
      ~multiple
  =
  let sent_values = Ui_context.sent_values ctx ~component in
  let sent = Option.is_some sent_values in
  let picked =
    match sent_values with
    | Some (`String s) -> [ s ]
    | Some (`Array items) ->
      List.filter_map items ~f:(function
        | `String s -> Some s
        | _ -> None)
    | Some _ -> []
    | None ->
      Option.value_map (Ui_context.get ctx ~path "sel") ~default:[] ~f:ticked
  in
  let option (o : Ui_spec.Choice.t) =
    let on = List.mem picked o.value ~equal:String.equal in
    let click =
      if sent
      then []
      else if multiple
      then (
        let next =
          if on
          then List.filter picked ~f:(fun v -> not (String.equal v o.value))
          else
            List.filter_map options ~f:(fun (x : Ui_spec.Choice.t) ->
              Option.some_if
                (String.equal x.value o.value
                 || List.mem picked x.value ~equal:String.equal)
                x.value)
        in
        [ Attr.on_click (fun _ ->
            Ui_context.set
              ctx
              ~path
              "sel"
              (Json.to_string (`Array (List.map next ~f:Json.str))))
        ])
      else
        [ Attr.on_click (fun _ -> send ctx ~component ~title (Json.str o.value))
        ]
    in
    Node.button
      ~attrs:
        ([ Attr.type_ "button"
         ; Attr.classes
             ([ "ui-choice"; tone_cls o.tone ] @ if on then [ "on" ] else [])
         ]
         @ (if multiple
            then [ Attr.create "aria-pressed" (Bool.to_string on) ]
            else [])
         @ (if sent then [ Attr.disabled ] else [])
         @ click)
      [ (if multiple then Html.check on else Node.none)
      ; Node.span
          ~attrs:[ Attr.class_ "ui-choice-text" ]
          [ Node.span
              ~attrs:[ Attr.class_ "ui-choice-label" ]
              [ Node.text o.label ]
          ; (match o.description with
             | Some d ->
               Node.span ~attrs:[ Attr.class_ "ui-choice-desc" ] [ Node.text d ]
             | None -> Node.none)
          ]
      ]
  in
  Node.div
    ~attrs:
      [ Attr.classes
          ([ "ui-choices"; "ui-reply" ] @ if sent then [ "sent" ] else [])
      ]
    [ Node.div
        ~attrs:[ Attr.class_ "ui-reply-head" ]
        [ (match prompt with
           | Some p -> Ui_context.md ~cls:"ui-prompt" p
           | None -> Node.none)
        ; sent_chip sent
        ]
    ; Node.div
        ~attrs:[ Attr.class_ "ui-choice-list"; Attr.role "group" ]
        (List.map options ~f:option)
    ; (if multiple && not sent
       then
         Node.div
           ~attrs:[ Attr.class_ "ui-actions" ]
           [ Node.button
               ~attrs:
                 ([ Attr.type_ "button"; Attr.class_ "ui-btn primary" ]
                  @
                  if List.is_empty picked
                  then [ Attr.disabled ]
                  else
                    [ Attr.on_click (fun _ ->
                        send
                          ctx
                          ~component
                          ~title
                          (`Array (List.map picked ~f:Json.str)))
                    ])
               [ Node.text "Send" ]
           ]
       else Node.none)
    ]
;;

let checklist
      (ctx : Ui_context.t)
      ~path
      ~component
      ~title
      ~heading
      items
      ~submit
  =
  let replies = Option.both component submit in
  let sent_values =
    Option.bind component ~f:(fun component ->
      if Option.is_some submit
      then Ui_context.sent_values ctx ~component
      else None)
  in
  let sent = Option.is_some sent_values in
  let checked k (text, default) =
    match sent_values with
    | Some (`Array items) -> List.mem items (`String text) ~equal:Json.equal
    | Some _ -> false
    | None ->
      (match Ui_context.get ctx ~path (sprintf "c%d" k) with
       | Some v -> String.equal v "1"
       | None -> default)
  in
  let states = List.mapi items ~f:checked in
  let done_ = List.count states ~f:Fn.id in
  Node.div
    ~attrs:
      [ Attr.classes
          ([ "ui-checklist" ]
           @ (if Option.is_some replies then [ "ui-reply" ] else [])
           @ if sent then [ "sent" ] else [])
      ]
    [ Node.div
        ~attrs:[ Attr.class_ "ui-reply-head" ]
        [ title_node heading
        ; Node.span
            ~attrs:[ Attr.class_ "ui-count" ]
            [ Node.text (sprintf "%d/%d" done_ (List.length items)) ]
        ; sent_chip sent
        ]
    ; Node.ul
        (List.mapi items ~f:(fun k (text, _) ->
           let on = List.nth_exn states k in
           Node.li
             [ Node.button
                 ~attrs:
                   ([ Attr.type_ "button"
                    ; Attr.classes
                        ([ "ui-check-item" ] @ if on then [ "on" ] else [])
                    ; Attr.role "checkbox"
                    ; Attr.create "aria-checked" (Bool.to_string on)
                    ]
                    @
                    if sent
                    then [ Attr.disabled ]
                    else
                      [ Attr.on_click (fun _ ->
                          Ui_context.set
                            ctx
                            ~path
                            (sprintf "c%d" k)
                            (if on then "0" else "1"))
                      ])
                 [ Html.check on; Ui_context.md text ]
             ]))
    ; (match replies with
       | Some (component, label) when not sent ->
         Node.div
           ~attrs:[ Attr.class_ "ui-actions" ]
           [ Node.button
               ~attrs:
                 [ Attr.type_ "button"
                 ; Attr.class_ "ui-btn primary"
                 ; Attr.on_click (fun _ ->
                     send
                       ctx
                       ~component
                       ~title
                       (`Array
                           (List.filter_mapi items ~f:(fun k (text, _) ->
                              Option.some_if
                                (List.nth_exn states k)
                                (Json.str text)))))
                 ]
               [ Node.text label ]
           ]
       | _ -> Node.none)
    ]
;;
