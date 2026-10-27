open! Core
open! Import

module Target = struct
  type t =
    | Entry of int
    | Message of int
    | Trimmed
  [@@deriving sexp_of, equal]

  let to_string = function
    | Entry n -> sprintf "e%d" n
    | Message i -> sprintf "m%d" i
    | Trimmed -> "trimmed"
  ;;

  let of_string s =
    match s with
    | "trimmed" -> Some Trimmed
    | s ->
      (match
         ( String.length s > 1
         , Option.try_with (fun () -> Int.of_string (String.drop_prefix s 1)) )
       with
       | true, Some n when Char.equal s.[0] 'e' -> Some (Entry n)
       | true, Some i when Char.equal s.[0] 'm' && i >= 0 -> Some (Message i)
       | _ -> None)
  ;;
end

module Kind = struct
  type t =
    | User
    | Compaction
    | Error
    | Trimmed
  [@@deriving sexp_of, equal]

  let of_outline : Outline.Kind.t -> t = function
    | User -> User
    | Compaction -> Compaction
    | Error -> Error
  ;;
end

module Point = struct
  type t =
    { target : Target.t
    ; kind : Kind.t
    ; text : string
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]
end

type t =
  { outline : (string * Outline.t) option
  ; fetching : string option
  ; pending : Target.t option
  }
[@@deriving sexp_of]

let empty = { outline = None; fetching = None; pending = None }
let first_line = Chat_html.first_line

(* A skill's invocation as typed, also from the start of one cut short. *)
let headline text =
  match Skill_message.parse text with
  | Some skill -> first_line (Skill_message.invocation skill)
  | None ->
    (match String.chop_prefix (String.lstrip text) ~prefix:"<skill name=\"" with
     | Some rest ->
       "/skill:"
       ^ Option.value_map (String.lsplit2 rest ~on:'"') ~default:rest ~f:fst
     | None -> first_line text)
;;

let user_point target (u : Message.User.t) : Point.t option =
  let point kind text = Some { Point.target; kind; text; at = u.at } in
  match Message.User.source u with
  | Some { kind = Summary; _ } -> point Compaction ""
  | Some { kind = Goal | Review | Init; title } -> point User (first_line title)
  | Some _ -> None
  | None ->
    let body = Message.User.body u in
    if String.is_empty (String.strip body) && List.is_empty u.images
    then None
    else point User (headline body)
;;

let entry_point n (entry : Chat.Entry.t) : Point.t option =
  match entry with
  | User u -> user_point (Entry n) u
  | Compaction _ ->
    Some { target = Entry n; kind = Compaction; text = ""; at = None }
  | Assistant { message = { stop_reason = Error e; at; _ }; streaming = false }
    -> Some { target = Entry n; kind = Error; text = first_line e; at }
  | Assistant _ | Shell _ -> None
;;

let landmark entry = Option.is_some (entry_point 0 entry)

let points
      ?(deleting = Int.Set.empty)
      t
      ~path
      ~(paging : Paging.t)
      ~(trim : Trim.t option)
      chat
  =
  let trimmed =
    Option.value_map trim ~default:[] ~f:(fun (trim : Trim.t) ->
      [ { Point.target = Trimmed
        ; kind = Trimmed
        ; text = Trim.label ~messages:trim.messages ~tokens:trim.tokens
        ; at = trim.at
        }
      ])
  in
  let unloaded =
    match t.outline, path with
    | Some (of_, outline), Some path
      when String.equal of_ path && paging.older > 0 ->
      List.filter_map outline ~f:(fun (p : Outline.Point.t) ->
        Option.some_if
          (p.index < paging.older)
          { Point.target = Message p.index
          ; kind = Kind.of_outline p.kind
          ; text =
              (match p.kind with
               | User -> headline p.text
               | Error -> first_line p.text
               | Compaction -> "")
          ; at = p.at
          })
    | _ -> []
  in
  trimmed
  @ unloaded
  @ List.filter_mapi (Chat.entries chat) ~f:(fun i entry ->
    let n = Chat.first chat + i in
    if Set.mem deleting n then None else entry_point n entry)
;;

(* Messages and entries go one for one, but tool results, which join their
   calls' entries. *)
let entry_of_index ~(page : Message_page.t) ~first index =
  let offset = index - page.older in
  if offset < 0 || offset >= List.length page.messages
  then None
  else
    Some
      (first
       + List.count (List.take page.messages offset) ~f:(function
         | Message.Tool_result _ -> false
         | User _ | Assistant _ -> true))
;;

let trimmable ~(paging : Paging.t) chat (p : Point.t) =
  match p.kind, p.target with
  | User, Message index -> index > 0
  | User, Entry n -> paging.older > 0 || n > Chat.first chat
  | (User | Compaction | Error | Trimmed), _ -> false
;;

let from_end points target =
  List.rev points
  |> List.filter ~f:(fun (p : Point.t) -> Kind.equal p.kind User)
  |> List.findi ~f:(fun _ (p : Point.t) -> Target.equal p.target target)
  |> Option.map ~f:(fun (i, p) -> i, p)
;;

let picker points ~times =
  let users =
    List.filter points ~f:(fun (p : Point.t) ->
      match p.kind with
      | User -> true
      | Compaction | Error | Trimmed -> false)
  in
  let count = List.length users in
  let items =
    List.rev
      (List.mapi users ~f:(fun i (p : Point.t) ->
         Picker.Item.create
           ~id:(Target.to_string p.target)
           ~detail:
             (String.concat
                ~sep:"  "
                (sprintf "%d of %d" (i + 1) count
                 :: Option.to_list
                      (Option.map p.at ~f:(Message_time.short times))))
           ~search:p.text
           (if String.is_empty p.text then "(an image)" else p.text)))
  in
  let trimmed =
    List.filter_map points ~f:(fun (p : Point.t) ->
      match p.kind with
      | Trimmed ->
        Some
          (Picker.Item.create
             ~id:(Target.to_string p.target)
             ~detail:"out of the model's context"
             ~dimmed:true
             ("Trimmed: "
              ^ String.substr_replace_first p.text ~pattern:" trimmed" ~with_:""
             ))
      | User | Compaction | Error -> None)
  in
  Picker.create ~title:"Jump to a message" (items @ trimmed)
;;
