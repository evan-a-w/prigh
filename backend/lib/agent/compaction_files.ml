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

(* Each once, where it was last seen. *)
let dedup paths =
  List.rev (List.stable_dedup (List.rev paths) ~compare:String.compare)
;;

let normalise ~read ~modified =
  let modified = dedup modified in
  { read =
      List.filter (dedup read) ~f:(fun p ->
        not (List.mem modified p ~equal:String.equal))
  ; modified
  }
;;

(* Like the tools resolve them; "~" is left as it is, since the home that
   matters is the tool host's. *)
let resolve ~cwd path =
  match cwd with
  | Some cwd when not (String.is_prefix path ~prefix:"~") ->
    Tool.resolve ~cwd path
  | Some _ | None -> path
;;

let add_calls ?(cwd = fun _ -> None) t messages =
  let succeeded =
    List.filter_map messages ~f:(function
      | Message.Tool_result r when not r.is_error -> Some r.tool_call_id
      | User _ | Assistant _ | Tool_result _ -> None)
    |> String.Set.of_list
  in
  let read, modified =
    List.concat_mapi messages ~f:(fun i -> function
      | Message.Assistant a ->
        List.filter_map (Message.Assistant.tool_calls a) ~f:(fun call ->
          if Set.mem succeeded call.id
          then (
            let read, modified = call_files call in
            let resolve = List.map ~f:(resolve ~cwd:(cwd i)) in
            Some (resolve read, resolve modified))
          else None)
      | User _ | Tool_result _ -> [])
    |> List.unzip
  in
  normalise
    ~read:(t.read @ List.concat read)
    ~modified:(t.modified @ List.concat modified)
;;

let max_listed = 100
let more_prefix = " (and "
let more_suffix = " more)"

let append summary t =
  let line label = function
    | [] -> []
    | paths ->
      let more = List.length paths - max_listed in
      let shown, more =
        if more > 0
        then List.drop paths more, sprintf "%s%d%s" more_prefix more more_suffix
        else paths, ""
      in
      [ label ^ String.concat ~sep:", " shown ^ more ]
  in
  match line read_label t.read @ line modified_label t.modified with
  | [] -> summary
  | lines ->
    String.concat ~sep:"\n" ((String.rstrip summary ^ "\n\n" ^ header) :: lines)
;;

let drop_more line =
  match
    ( String.substr_index_all line ~may_overlap:false ~pattern:more_prefix
      |> List.last
    , String.chop_suffix line ~suffix:more_suffix )
  with
  | Some i, Some rest ->
    let count = String.drop_prefix rest (i + String.length more_prefix) in
    (match Int.of_string_opt count with
     | Some _ -> String.prefix line i
     | None -> line)
  | None, _ | _, None -> line
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
          let paths = drop_more paths in
          String.Search_pattern.split_on
            (String.Search_pattern.create ", ")
            paths
        | None -> [])
      |> List.filter ~f:(Fn.non String.is_empty)
    in
    ( String.rstrip (String.prefix summary i)
    , normalise ~read:(paths read_label) ~modified:(paths modified_label) )
;;
