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

let base ~nix ~auto_background ~tools =
  let tool_list =
    String.concat
      ~sep:"\n"
      (List.map tools ~f:(fun (t : Tool_spec.t) ->
         sprintf "- %s: %s" t.name (String.prefix t.description 80)))
  in
  let has name =
    List.exists tools ~f:(fun (t : Tool_spec.t) -> String.equal t.name name)
  in
  String.concat
    ~sep:"\n"
    ([ "You are prigh, a coding agent working in the user's project from the \
        command line."
     ; ""
     ; "Guidelines:"
     ; "- Use the tools to inspect and change the project; do not guess file \
        contents."
     ; sprintf
         "- Prefer %s over write for existing files. Keep changes minimal and \
          focused."
         (if has "apply_patch" then "apply_patch" else "edit")
     ; "- After making changes, verify them (build, tests) when a way to do so \
        exists."
     ; "- Be concise. Explain non-trivial decisions briefly. No filler."
     ; "- Ask before destructive or irreversible actions."
     ; ""
     ]
     @ (if has "subagent_wait" || has "job_wait"
        then
          [ "Background work: almost never block on it." ]
          @ (if has "job_wait"
             then
               [ "- Commands that may take long (builds, test suites, \
                  installs, image builds, deployments) or that keep running \
                  (servers, watchers) go in the background: bash with \
                  background: true, then continue with other useful work or \
                  end your turn. Stop servers with job_kill when done."
               ]
               @ Option.value_map auto_background ~default:[] ~f:(fun s ->
                 [ sprintf
                     "- A foreground bash command still running after %s is \
                      moved to the background automatically (it keeps running \
                      as a job, not restarted), so a long command never stalls \
                      you: you get its job id and output so far at once."
                     (Background_tasks.seconds_text s)
                 ])
             else [])
          @ (if has "subagent_wait"
             then
               [ "- Subagents always run in the background: subagent returns \
                  at once with the agent id."
               ]
             else [])
          @ [ "- Each finished job or subagent reports automatically, in a \
               message starting with \"[job <id> exited <code>]\" (or killed, \
               failed) or \"[subagent <id> finished]\" (or failed), sent by \
               prigh, not typed by the user. You are woken by it even after \
               you end your turn."
            ; sprintf
                "- So while they run, keep working on whatever does not depend \
                 on them, or end your turn. Do not call job_wait or \
                 subagent_wait to pass the time, to collect results you could \
                 take as they arrive, or to poll; never poll with sleep \
                 loops.%s Wait only when you truly cannot do anything useful \
                 without one result, and then wait for that one (ids, \
                 all=false)."
                (Option.value_map auto_background ~default:"" ~f:(fun s ->
                   sprintf
                     " A wait returns after %s anyway."
                     (Background_tasks.seconds_text s)))
            ; ""
            ]
        else [])
     @ (if nix
        then
          [ "Get tools that are not installed from nixpkgs, not with apt or \
             sudo (you may have no root): `nix shell nixpkgs#python3 \
             nixpkgs#nodejs -c <cmd>` runs a command with those packages (any \
             number of them), `nix run nixpkgs#<pkg> -- <args>` runs a \
             package's program, `nix search nixpkgs <term>` finds package \
             names (slow the first time), and `nix profile install \
             nixpkgs#<pkg>` keeps a tool on PATH across sessions. Packages are \
             cached and shared, so repeating these is cheap."
          ; ""
          ]
        else [])
     @ [ (if List.is_empty tools
          then "No tools are available in this session."
          else "Available tools:\n" ^ tool_list)
       ])
;;

let read_instructions ~cwd ~home =
  List.map (instruction_files ~cwd ~home) ~f:(fun path ->
    path, String.strip (In_channel.read_all path))
;;

let build
      ?date
      ?instructions
      ?(nix = false)
      ?(skills = [])
      ?auto_background
      ~cwd
      ~home
      ~tools
      ()
  =
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
  let skills =
    if
      List.exists tools ~f:(fun (t : Tool_spec.t) -> String.equal t.name "read")
    then Option.to_list (Skill.prompt_section skills)
    else []
  in
  String.concat
    ~sep:"\n\n"
    ([ base ~nix ~auto_background ~tools ]
     @ skills
     @ [ environment ]
     @ instructions)
;;

let discretion =
  "Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often \
   unchanged, and missing an update is not serious."
;;

let tools_changed_note ~added ~removed =
  let section label = function
    | [] -> []
    | tools -> [ sprintf " %s: %s." label (String.concat ~sep:", " tools) ]
  in
  String.concat
    ([ "[Tools: the tools changed with the model." ]
     @ section "Now available" added
     @ section "No longer available" removed
     @ [ "]" ])
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

let plan_mode_note ~on =
  if on
  then
    "[Plan mode is on: you are planning, not implementing. Investigate with \
     read-only tools (bash only for read-only commands; the user confirms each \
     one), ask the user about anything unclear with ask_user, and do not \
     change any files: edits are refused. Keep the steps in update_plan. When \
     the plan is ready, call exit_plan_mode with it; the user approves it \
     before you implement it.]"
  else
    "[Plan mode is off: you can change files and run commands again, as the \
     user's approval mode allows.]"
;;
