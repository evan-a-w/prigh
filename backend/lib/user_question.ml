open! Core
open! Import

module Choice = struct
  type t =
    { label : string
    ; description : string option
    }
  [@@deriving sexp_of, equal]
end

type t =
  { question : string
  ; header : string option
  ; choices : Choice.t list
  ; multiple : bool
  }
[@@deriving sexp_of, equal]

module Reply = struct
  type t =
    | Answered of string list list
    | Declined
    | Cancelled
    | Unavailable of string
  [@@deriving sexp_of]
end

let max_questions = 4

let nonempty s =
  Option.bind s ~f:(fun s ->
    let s = String.strip s in
    if String.is_empty s then None else Some s)
;;

let of_arguments args =
  Or_error.try_with_join (fun () ->
    let open Or_error.Let_syntax in
    let items =
      Option.value (Tool_args.list_opt args "questions") ~default:[]
    in
    let%bind () =
      match List.length items with
      | 0 -> Or_error.error_string "give at least one question in \"questions\""
      | n when n > max_questions ->
        Or_error.errorf
          "%d questions is too many at once; ask at most %d"
          n
          max_questions
      | _ -> Ok ()
    in
    Or_error.all
      (List.mapi items ~f:(fun i item ->
         match nonempty (Tool_args.string_opt item "question") with
         | None -> Or_error.errorf "questions[%d] has no question text" i
         | Some question ->
           let choices =
             List.filter_map
               (Option.value (Tool_args.list_opt item "options") ~default:[])
               ~f:(fun o ->
                 Option.map
                   (nonempty (Tool_args.string_opt o "label"))
                   ~f:(fun label ->
                     { Choice.label
                     ; description =
                         nonempty (Tool_args.string_opt o "description")
                     }))
           in
           let labels = List.map choices ~f:(fun c -> c.label) in
           if List.contains_dup labels ~compare:String.compare
           then
             Or_error.errorf
               "questions[%d] has two options with the same label"
               i
           else
             Ok
               { question
               ; header = nonempty (Tool_args.string_opt item "header")
               ; choices
               ; multiple =
                   Option.value
                     (Tool_args.bool_opt item "multiple")
                     ~default:false
               })))
;;

let to_json t =
  `Object
    [ "question", `String t.question
    ; "header", Option.value_map t.header ~default:`Null ~f:(fun s -> `String s)
    ; ( "options"
      , `Array
          (List.map t.choices ~f:(fun c ->
             `Object
               [ "label", `String c.label
               ; ( "description"
                 , Option.value_map c.description ~default:`Null ~f:(fun s ->
                     `String s) )
               ])) )
    ; ("multiple", if t.multiple then `True else `False)
    ]
;;

let check_answers questions answers =
  if List.length questions <> List.length answers
  then
    Or_error.errorf
      "expected %d answers (one list per question), got %d"
      (List.length questions)
      (List.length answers)
  else
    Or_error.all_unit
      (List.mapi (List.zip_exn questions answers) ~f:(fun i (q, answer) ->
         let picked =
           List.count answer ~f:(fun a ->
             List.exists q.choices ~f:(fun c -> String.equal c.label a))
         in
         if (not q.multiple) && picked > 1
         then
           Or_error.errorf "question %d takes one choice, got %d" (i + 1) picked
         else Ok ()))
;;

let result_text questions answers =
  let lines =
    List.concat_map (List.zip_exn questions answers) ~f:(fun (q, answer) ->
      let answer =
        match List.filter_map answer ~f:(fun a -> nonempty (Some a)) with
        | [] -> "(no answer)"
        | answers -> String.concat ~sep:"; " answers
      in
      [ "Q: " ^ q.question; "A: " ^ answer ])
  in
  String.concat_lines ("The user answered:" :: lines)
;;
