open! Core
open! Import

module Mode = struct
  type t =
    | Off
    | Bell
    | Desktop
  [@@deriving sexp_of, equal, enumerate]

  let to_string = function
    | Off -> "off"
    | Bell -> "bell"
    | Desktop -> "desktop"
  ;;

  let of_string s = List.find all ~f:(fun t -> String.equal (to_string t) s)

  let of_config (config : P.Config.t option) =
    Option.bind config ~f:(fun c -> of_string c.notify)
    |> Option.value ~default:Bell
  ;;
end

type t =
  { title : string
  ; body : string
  }
[@@deriving sexp_of, equal]

let one_line s =
  match String.split_lines (String.strip s) with
  | line :: _ -> line
  | [] -> ""
;;

let last_reply messages =
  List.find_map (List.rev messages) ~f:(function
    | P.Message.Assistant a ->
      List.find_map (List.rev a.content) ~f:(function
        | P.Content.Text text when not (String.is_empty (String.strip text)) ->
          Some (one_line text)
        | _ -> None)
    | User _ | Tool_result _ -> None)
;;

let of_event (event : P.Event.t) ~where =
  match event with
  | Agent_end messages ->
    Some
      { title = "prigh: done in " ^ where
      ; body = Option.value (last_reply messages) ~default:"The turn ended."
      }
  | Tool_confirm { name; summary; _ } ->
    Some
      { title = "prigh: waiting for you in " ^ where
      ; body = sprintf "Allow %s: %s?" name (one_line summary)
      }
  | Ask_user { questions; _ } ->
    Some
      { title = "prigh: a question for you in " ^ where
      ; body =
          (match questions with
           | [] -> "The model asks you something."
           | q :: _ -> one_line q.question)
      }
  | Message_start (User user) ->
    (match P.Message.User.source user with
     | Some { kind = Delivery; title } ->
       Some { title = "prigh: background work finished in " ^ where; body = title }
     | Some _ | None -> None)
  | _ -> None
;;

let where (state : P.State.t option) =
  match state with
  | None -> "prigh"
  | Some s ->
    (match s.session_name with
     | Some name when not (String.is_empty (String.strip name)) -> name
     | _ ->
       (match List.last (String.split s.cwd ~on:'/') with
        | Some base when not (String.is_empty base) -> base
        | _ -> s.cwd))
;;
