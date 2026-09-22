open! Core
open! Import

let candidates = [ "AGENTS.md"; "CLAUDE.md" ]

let ancestors dir =
  let rec go dir acc =
    let acc = dir :: acc in
    let parent = Filename.dirname dir in
    if String.equal parent dir then acc else go parent acc
  in
  go dir []
;;

let first_existing dir =
  List.find_map candidates ~f:(fun name ->
    let path = Filename.concat dir name in
    if Sys_unix.file_exists_exn path then Some path else None)
;;

let instruction_files ~cwd ~home =
  let global = first_existing (Filename.concat home ".prigh") in
  let project = List.filter_map (ancestors cwd) ~f:first_existing in
  Option.to_list global @ project
;;

let base ~tools =
  let tool_list =
    String.concat
      ~sep:"\n"
      (List.map tools ~f:(fun (t : Tool_spec.t) ->
         sprintf "- %s: %s" t.name (String.prefix t.description 80)))
  in
  String.concat
    ~sep:"\n"
    [ "You are prigh, a coding agent working in the user's project from the \
       command line."
    ; ""
    ; "Guidelines:"
    ; "- Use the tools to inspect and change the project; do not guess file \
       contents."
    ; "- Prefer edit over write for existing files. Keep changes minimal and \
       focused."
    ; "- After making changes, verify them (build, tests) when a way to do so \
       exists."
    ; "- Be concise. Explain non-trivial decisions briefly. No filler."
    ; "- Ask before destructive or irreversible actions."
    ; ""
    ; (if List.is_empty tools
       then "No tools are available in this session."
       else "Available tools:\n" ^ tool_list)
    ]
;;

let read_instructions ~cwd ~home =
  List.map (instruction_files ~cwd ~home) ~f:(fun path ->
    path, String.strip (In_channel.read_all path))
;;

let build ?date ?instructions ~cwd ~home ~tools () =
  let date =
    match date with
    | Some d -> d
    | None -> Date.to_string (Date.today ~zone:Time_float.Zone.utc)
  in
  let environment =
    sprintf
      "Environment:\n- Working directory: %s\n- Date: %s\n- OS: %s"
      cwd
      date
      (Core_unix.Utsname.sysname (Core_unix.uname ()))
  in
  let instructions =
    match instructions with
    | Some files -> files
    | None -> read_instructions ~cwd ~home
  in
  let instructions =
    List.map instructions ~f:(fun (path, text) ->
      sprintf "Instructions from %s:\n%s" path text)
  in
  String.concat ~sep:"\n\n" ([ base ~tools; environment ] @ instructions)
;;

let discretion =
  "Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often \
   unchanged, and missing an update is not serious."
;;

let host_changed_note ~host =
  sprintf
    "[Environment: the tool host is now %s, so tools run there and its \
     filesystem may differ from the one described above. %s]"
    host
    discretion
;;

let cwd_changed_note ~cwd =
  sprintf
    "[Environment: the working directory is now %s. Project instructions for \
     it may differ from the ones above. %s]"
    cwd
    discretion
;;
