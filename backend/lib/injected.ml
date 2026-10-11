open! Core
open! Import

let first_line s =
  match String.split_lines (String.strip s) with
  | line :: _ :: _ -> line ^ " …"
  | [ line ] -> line
  | [] -> ""
;;

module Note = struct
  module Kind = struct
    type t =
      | Host of { name : string }
      | Cwd of { dir : string }
      | Tools of
          { added : string list
          ; removed : string list
          }
      | Editor of
          { before : string
          ; after : string
          }
      | Plan_mode of { on : bool }
    [@@deriving sexp, jsonaf, equal]

    let wire = function
      | Host _ -> "host"
      | Cwd _ -> "cwd"
      | Tools _ | Editor _ -> "tools"
      | Plan_mode _ -> "plan_mode"
    ;;
  end

  type t =
    { kind : Kind.t
    ; text : string
    }
  [@@deriving sexp, jsonaf, equal]

  let title t =
    match t.kind with
    | Host { name } -> sprintf "Tools now run on %s" name
    | Cwd { dir } -> sprintf "Working directory is now %s" dir
    | Tools { added; removed } ->
      let names verb = function
        | [] -> None
        | tools -> Some (String.concat ~sep:", " tools ^ " " ^ verb)
      in
      "Tools changed: "
      ^ String.concat
          ~sep:"; "
          (List.filter_opt [ names "added" added; names "removed" removed ])
    | Editor { before; after } ->
      sprintf "File edits now use %s instead of %s" after before
    | Plan_mode { on = true } ->
      "Plan mode on: the model was told to plan, not change files"
    | Plan_mode { on = false } ->
      "Plan mode off: the model was told it can change files"
  ;;

  let between s ~prefix ~suffix =
    let open Option.Let_syntax in
    let%bind rest = String.chop_prefix s ~prefix in
    let%map i = String.substr_index rest ~pattern:suffix in
    String.prefix rest i
  ;;

  (* The wordings of [System_prompt]'s notes. *)
  let infer line : t option =
    let kind : Kind.t option =
      if not (String.is_suffix line ~suffix:"]")
      then None
      else (
        match
          between
            line
            ~prefix:"[Environment: the tool host is now "
            ~suffix:", so tools run there"
        with
        | Some name -> Some (Host { name })
        | None when String.is_prefix line ~prefix:"[Plan mode is on: " ->
          Some (Plan_mode { on = true })
        | None when String.is_prefix line ~prefix:"[Plan mode is off: " ->
          Some (Plan_mode { on = false })
        | None ->
          (match
             between
               line
               ~prefix:"[Environment: the working directory is now "
               ~suffix:". Project instructions"
           with
           | Some dir -> Some (Cwd { dir })
           | None
             when String.is_prefix
                    line
                    ~prefix:"[Tools: the tools changed with the model." ->
             let names label =
               match String.substr_index line ~pattern:label with
               | None -> []
               | Some i ->
                 let rest = String.drop_prefix line (i + String.length label) in
                 String.prefix
                   rest
                   (String.index rest '.' |> Option.value ~default:0)
                 |> String.split ~on:','
                 |> List.map ~f:String.strip
             in
             Some
               (Tools
                  { added = names "Now available: "
                  ; removed = names "No longer available: "
                  })
           | None ->
             let%bind.Option replaces =
               between line ~prefix:"[Tools: " ~suffix:" for editing files"
             in
             let%map.Option after, before =
               String.lsplit2 replaces ~on:' '
               |> Option.bind ~f:(fun (after, rest) ->
                 Option.map
                   (String.chop_prefix rest ~prefix:"replaces ")
                   ~f:(fun before -> after, before))
             in
             Kind.Editor { before; after }))
    in
    Option.map kind ~f:(fun kind -> { kind; text = line })
  ;;
end

module Attachment = struct
  type t =
    { path : string
    ; length : int
    }
  [@@deriving sexp, jsonaf, equal]
end

module Source = struct
  type t =
    | Delivery of { reports : string list }
    | Handover of
        { from : string
        ; to_ : string
        ; error : string
        }
    | Goal_start of { goal : string }
    | Goal_continuation of
        { continuation : int
        ; max : int
        }
    | Resume
    | Shell of { command : string }
    | Summary
    | Review of { target : string }
    | Init
  [@@deriving sexp, jsonaf, equal]

  let kind = function
    | Delivery _ -> "delivery"
    | Handover _ -> "handover"
    | Goal_start _ -> "goal"
    | Goal_continuation _ -> "goal_continuation"
    | Resume -> "resume"
    | Shell _ -> "shell"
    | Summary -> "summary"
    | Review _ -> "review"
    | Init -> "init"
  ;;

  let title = function
    | Delivery { reports } ->
      String.capitalize (String.concat ~sep:", " reports)
    | Handover { from; to_; error = _ } ->
      sprintf "Handed over from %s to %s" from to_
    | Goal_start { goal } -> "Goal: " ^ first_line goal
    | Goal_continuation { continuation; max } ->
      sprintf "Goal: continuing (%d/%d)" continuation max
    | Resume -> "Usage limit reset: carrying on"
    | Shell { command } ->
      sprintf "Output of $ %s added to the context" (first_line command)
    | Summary -> "Summary of the earlier conversation"
    | Review { target } -> "Review of " ^ target
    | Init -> "Write or improve AGENTS.md"
  ;;

  (* [\[subagent a1 finished\] task] or [\[job j1 exited 0\] command]. *)
  let report_header line =
    let open Option.Let_syntax in
    let%bind inside, _ =
      String.chop_prefix line ~prefix:"[" >>= String.lsplit2 ~on:']'
    in
    match String.split inside ~on:' ' with
    | [ "subagent"; _; ("finished" | "failed") ] -> Some inside
    | "job" :: id :: status :: _
      when String.is_prefix id ~prefix:"j"
           && String.length id > 1
           && String.for_all (String.drop_prefix id 1) ~f:Char.is_digit
           && List.mem
                [ "exited"; "killed"; "timed"; "failed" ]
                status
                ~equal:String.equal -> Some inside
    | _ -> None
  ;;

  let delivery text =
    match String.split_lines text with
    | first :: _ as lines when Option.is_some (report_header first) ->
      Some (Delivery { reports = List.filter_map lines ~f:report_header })
    | _ -> None
  ;;

  (* [\[prigh: FROM cannot continue (ERROR), so TO takes over this
     conversation ...\]]; the error may contain anything, so it ends at the
     last [), so ] before the last [ takes over]. *)
  let handover text =
    let open Option.Let_syntax in
    let cannot = " cannot continue (" in
    let so = "), so " in
    let%bind rest = String.chop_prefix text ~prefix:"[prigh: " in
    let%bind () = Option.some_if (String.is_suffix rest ~suffix:"]") () in
    let%bind i = String.substr_index rest ~pattern:cannot in
    let from = String.prefix rest i in
    let rest = String.drop_prefix rest (i + String.length cannot) in
    let last pattern text =
      List.last (String.substr_index_all text ~may_overlap:false ~pattern)
    in
    let%bind j = last " takes over this conversation" rest in
    let before = String.prefix rest j in
    let%bind k = last so before in
    let error = String.prefix before k in
    let to_ = String.drop_prefix before (k + String.length so) in
    let model_key s =
      not (String.is_empty s || String.exists s ~f:Char.is_whitespace)
    in
    let%map () = Option.some_if (model_key from && model_key to_) () in
    Handover { from; to_; error }
  ;;

  let goal text =
    let open Option.Let_syntax in
    match String.chop_prefix text ~prefix:"[prigh goal] " with
    | Some rest ->
      let%bind _, rest = String.lsplit2 rest ~on:'\n' in
      let%bind rest =
        String.substr_index rest ~pattern:"<goal>\n"
        >>| fun i -> String.drop_prefix rest (i + String.length "<goal>\n")
      in
      let%map i = String.substr_index rest ~pattern:"\n</goal>" in
      Goal_start { goal = String.prefix rest i }
    | None ->
      let%bind rest =
        String.chop_prefix text ~prefix:"[prigh goal: continuation "
      in
      let%bind numbers, _ = String.lsplit2 rest ~on:']' in
      (match String.split numbers ~on:' ' with
       | [ n; "of"; m ] ->
         let%bind continuation = Int.of_string_opt n in
         let%map max = Int.of_string_opt m in
         Goal_continuation { continuation; max }
       | _ -> None)
  ;;

  let infer text =
    if String.is_prefix text ~prefix:"Summary of the conversation so far:\n"
    then Some Summary
    else if String.is_prefix text ~prefix:"[prigh: the usage limit has reset."
    then Some Resume
    else List.find_map [ delivery; handover; goal ] ~f:(fun parse -> parse text)
  ;;
end

type t =
  { notes : Note.t list [@sexp.list] [@jsonaf.default []]
  ; attachments : Attachment.t list [@sexp.list] [@jsonaf.default []]
  ; source : Source.t option [@sexp.option] [@jsonaf.option]
  }
[@@deriving sexp, jsonaf, equal]

let empty = { notes = []; attachments = []; source = None }
let is_empty = equal empty

let notes_text (notes : Note.t list) =
  String.concat ~sep:"\n" (List.map notes ~f:(fun n -> n.text))
;;

let notes_prefix notes = notes_text notes ^ "\n\n"

let prefix_notes notes text =
  match notes with
  | [] -> text
  | notes -> notes_prefix notes ^ text
;;

let body t text =
  match t.source with
  | Some _ -> text
  | None ->
    let text =
      match t.notes with
      | [] -> text
      | notes ->
        if String.equal text (notes_text notes)
        then ""
        else
          Option.value
            (String.chop_prefix text ~prefix:(notes_prefix notes))
            ~default:text
    in
    (match t.attachments with
     | [] -> text
     | attachments ->
       let suffix =
         List.sum
           (module Int)
           attachments
           ~f:(fun (a : Attachment.t) -> a.length + 2)
       in
       if suffix <= String.length text
       then String.drop_suffix text suffix
       else text)
;;

(* [<file path="P">\n...</file>] or [<file path="P" error="E"/>] blocks, each
   after a blank line, to the end of [text]. *)
let infer_attachments text =
  let start = "\n\n<file path=\"" in
  let parse_path s =
    match Scanf.sscanf s "<file path=%S%n" (fun path n -> path, n) with
    | path, n -> Some (path, String.drop_prefix s n)
    | exception _ -> None
  in
  (* The blocks from [s] (which starts at a block) to the end, if they reach
     it. *)
  let rec blocks s =
    let open Option.Let_syntax in
    let%bind path, rest = parse_path s in
    let block_end =
      if String.is_prefix rest ~prefix:" error="
      then
        String.substr_index rest ~pattern:"/>"
        |> Option.map ~f:(fun i -> i + 2)
        |> Option.to_list
      else if String.is_prefix rest ~prefix:">\n"
      then
        List.map
          (String.substr_index_all rest ~may_overlap:false ~pattern:"</file>")
          ~f:(fun i -> i + String.length "</file>")
      else []
    in
    List.find_map block_end ~f:(fun i ->
      let length = String.length s - String.length rest + i in
      let after = String.drop_prefix rest i in
      let this = { Attachment.path; length } in
      if String.is_empty after
      then Some [ this ]
      else (
        match String.chop_prefix after ~prefix:"\n\n" with
        | Some next when String.is_prefix next ~prefix:"<file path=\"" ->
          Option.map (blocks next) ~f:(fun more -> this :: more)
        | Some _ | None -> None))
  in
  List.find_map
    (String.substr_index_all text ~may_overlap:false ~pattern:start)
    ~f:(fun i -> blocks (String.drop_prefix text (i + 2)))
;;

(* Notes are lines in front of a blank line, or the whole message (one sent
   between turns); a message of the harness's may follow them. *)
let infer text =
  let notes_in s =
    match Option.all (List.map (String.split_lines s) ~f:Note.infer) with
    | Some (_ :: _ as notes) -> Some notes
    | Some [] | None -> None
  in
  let notes, rest =
    match String.substr_index text ~pattern:"\n\n" with
    | None ->
      (match notes_in text with
       | Some notes -> notes, ""
       | None -> [], text)
    | Some i ->
      (match notes_in (String.prefix text i) with
       | Some notes -> notes, String.drop_prefix text (i + 2)
       | None -> [], text)
  in
  match Source.infer rest with
  | Some source -> Some { empty with notes; source = Some source }
  | None ->
    let attachments = Option.value (infer_attachments rest) ~default:[] in
    let t = { empty with notes; attachments } in
    if is_empty t then None else Some t
;;

let of_message ~origin text =
  match origin with
  | Some _ -> origin
  | None -> infer text
;;

let to_json t ~text : Json.t =
  let list name items ~f =
    match items with
    | [] -> []
    | items -> [ name, `Array (List.map items ~f) ]
  in
  `Object
    (List.concat
       [ (match t.source with
          | Some s ->
            [ ( "source"
              , `Object
                  [ "kind", `String (Source.kind s)
                  ; "title", `String (Source.title s)
                  ] )
            ]
          | None -> [ "body", `String (body t text) ])
       ; list "notes" t.notes ~f:(fun (n : Note.t) ->
           `Object
             [ "kind", `String (Note.Kind.wire n.kind)
             ; "title", `String (Note.title n)
             ; "text", `String n.text
             ])
       ; list "attachments" t.attachments ~f:(fun (a : Attachment.t) ->
           `Object [ "path", `String a.path ])
       ])
;;
