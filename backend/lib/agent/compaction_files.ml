open! Core
open! Import

type t =
  { read : string list
  ; modified : string list
  }
[@@deriving sexp_of]

let empty = { read = []; modified = [] }
let header = "## Files touched (recorded from tool calls)"
let read_label = "Files read: "
let modified_label = "Files modified: "

let call_files (call : Content.Tool_call.t) =
  let string_arg name =
    match Json.parse call.arguments with
    | Ok (`Object fields) ->
      (match List.Assoc.find fields ~equal:String.equal name with
       | Some (`String s) -> Some s
       | _ -> None)
    | Ok _ | Error _ -> None
  in
  match call.name with
  | "read" -> Option.to_list (string_arg "path"), []
  | "write" | "edit" -> [], Option.to_list (string_arg "path")
  | "apply_patch" ->
    [], Option.value_map (string_arg "input") ~default:[] ~f:Apply_patch.paths
  | _ -> [], []
;;

let dedup paths = List.stable_dedup paths ~compare:String.compare

let normalise ~read ~modified =
  let modified = dedup modified in
  { read =
      List.filter (dedup read) ~f:(fun p ->
        not (List.mem modified p ~equal:String.equal))
  ; modified
  }
;;

let add_calls t messages =
  let read, modified =
    List.concat_map messages ~f:(function
      | Message.Assistant a ->
        List.map (Message.Assistant.tool_calls a) ~f:call_files
      | User _ | Tool_result _ -> [])
    |> List.unzip
  in
  normalise
    ~read:(t.read @ List.concat read)
    ~modified:(t.modified @ List.concat modified)
;;

let append summary t =
  let line label = function
    | [] -> []
    | paths -> [ label ^ String.concat ~sep:", " paths ]
  in
  match line read_label t.read @ line modified_label t.modified with
  | [] -> summary
  | lines ->
    String.concat ~sep:"\n" ((String.rstrip summary ^ "\n\n" ^ header) :: lines)
;;

let of_summary summary =
  match
    String.substr_index_all
      summary
      ~may_overlap:false
      ~pattern:("\n" ^ header ^ "\n")
    |> List.last
  with
  | None -> summary, empty
  | Some i ->
    let section = String.drop_prefix summary (i + String.length header + 2) in
    let paths label =
      List.concat_map (String.split_lines section) ~f:(fun line ->
        match String.chop_prefix line ~prefix:label with
        | Some paths ->
          String.Search_pattern.split_on
            (String.Search_pattern.create ", ")
            paths
        | None -> [])
      |> List.filter ~f:(Fn.non String.is_empty)
    in
    ( String.rstrip (String.prefix summary i)
    , normalise ~read:(paths read_label) ~modified:(paths modified_label) )
;;
