open! Core
open! Import

type t =
  { name : string
  ; description : string
  ; path : string
  ; model_invocable : bool
  }
[@@deriving sexp_of, jsonaf]

let file_name = "SKILL.md"
let max_description = 1024
let subdirs = [ ".prigh/skills"; ".claude/skills"; ".agents/skills" ]

let ancestors dir =
  let rec go dir acc =
    let parent = Filename.dirname dir in
    if String.equal parent dir
    then List.rev (dir :: acc)
    else go parent (dir :: acc)
  in
  go dir []
;;

let roots ~cwd ~home =
  List.concat_map
    (ancestors cwd @ [ home ])
    ~f:(fun dir -> List.map subdirs ~f:(Filename.concat dir))
  |> List.stable_dedup ~compare:String.compare
;;

let is_directory path =
  match Sys_unix.is_directory path with
  | `Yes -> true
  | `No | `Unknown -> false
;;

let is_file path =
  match Sys_unix.is_file path with
  | `Yes -> true
  | `No | `Unknown -> false
;;

let bool_field fields key =
  match Option.map (Frontmatter.find fields key) ~f:String.lowercase with
  | Some ("true" | "yes") -> true
  | Some _ | None -> false
;;

let parse ~path text =
  let fields, body = Frontmatter.split text in
  let name =
    match Frontmatter.find fields "name" with
    | Some name when not (String.is_empty (String.strip name)) ->
      String.strip name
    | Some _ | None -> Filename.basename (Filename.dirname path)
  in
  let name =
    String.map name ~f:(fun c -> if Char.is_whitespace c then '-' else c)
  in
  match Frontmatter.find fields "description" with
  | None -> Or_error.errorf "%s: no description in its frontmatter" path
  | Some description ->
    (match String.strip description with
     | "" -> Or_error.errorf "%s: the description is empty" path
     | description ->
       Ok
         ( { name
           ; description = String.prefix description max_description
           ; path
           ; model_invocable =
               not (bool_field fields "disable-model-invocation")
           }
         , body ))
;;

let load path =
  match In_channel.read_all path with
  | text -> Result.ok (parse ~path text) |> Option.map ~f:fst
  | exception _ -> None
;;

(* A directory with a SKILL.md is a skill; otherwise its subdirectories are
   searched, a few levels deep (e.g. skills grouped by source). *)
let rec find_in dir ~depth =
  let file = Filename.concat dir file_name in
  if is_file file
  then Option.to_list (load file)
  else if depth = 0
  then []
  else (
    match Sys_unix.ls_dir dir with
    | exception _ -> []
    | names ->
      List.sort names ~compare:String.compare
      |> List.filter ~f:(fun name ->
        not
          (String.is_prefix name ~prefix:"." || String.equal name "node_modules"))
      |> List.concat_map ~f:(fun name ->
        let path = Filename.concat dir name in
        if is_directory path then find_in path ~depth:(depth - 1) else []))
;;

let discover ~cwd ~home =
  List.concat_map (roots ~cwd ~home) ~f:(fun root ->
    if is_directory root then find_in root ~depth:3 else [])
  |> List.stable_dedup ~compare:(fun a b -> String.compare a.name b.name)
  |> List.sort ~compare:(fun a b -> String.compare a.name b.name)
;;

let invocation text =
  match String.chop_prefix (String.lstrip text) ~prefix:"/skill:" with
  | None -> None
  | Some rest ->
    let name, args =
      match String.lsplit2 rest ~on:' ' with
      | Some (name, args) -> name, args
      | None ->
        (match String.lsplit2 rest ~on:'\n' with
         | Some (name, args) -> name, args
         | None -> rest, "")
    in
    let name = String.strip name in
    if String.is_empty name then None else Some (name, String.strip args)
;;

let expand t ~body ~args =
  let block =
    sprintf
      "<skill name=\"%s\" location=\"%s\">\n\
       References are relative to %s.\n\n\
       %s\n\
       </skill>"
      t.name
      t.path
      (Filename.dirname t.path)
      (String.strip body)
  in
  if String.is_empty args then block else block ^ "\n\n" ^ args
;;

let as_typed text =
  let re = Re.Perl.compile_pat ~opts:[ `Dotall ] {|^<skill name="([^"]*)" location="[^"]*">\n.*?\n</skill>(\n\n(.*))?$|} in
  match Re.exec_opt re text with
  | None -> text
  | Some g ->
    (match Re.Group.get_opt g 3 with
     | Some args -> sprintf "/skill:%s %s" (Re.Group.get g 1) args
     | None -> "/skill:" ^ Re.Group.get g 1)
;;

let prompt_section skills =
  match List.filter skills ~f:(fun t -> t.model_invocable) with
  | [] -> None
  | skills ->
    Some
      (String.concat
         ~sep:"\n"
         ("Skills hold instructions for particular tasks. When a task matches \
           a skill's description, read its file with read before starting, and \
           follow it; paths in it are relative to its directory."
          :: List.map skills ~f:(fun t ->
            sprintf "- %s (%s): %s" t.name t.path t.description)))
;;

let unknown skills name =
  match skills with
  | [] ->
    Error.createf
      "unknown skill %S: there are none here; add one as \
       .prigh/skills/<name>/SKILL.md (or .claude/skills/) in the project or \
       ~/.prigh"
      name
  | skills ->
    Error.createf
      "unknown skill %S; did you mean: %s (/skills lists them all)"
      name
      (String.concat
         ~sep:", "
         (Edit_distance.closest (List.map skills ~f:(fun t -> t.name)) name))
;;
