(* Wall seconds of the slowest files (their tests mostly wait on processes
   and timers); every other file counts as [default_cost]. *)
let costs =
  [ "test_agent", 2.6
  ; "test_connection", 1.6
  ; "test_http", 1.5
  ; "test_cli", 1.4
  ; "test_acp", 1.4
  ; "test_rpc", 1.2
  ; "test_ask_user_timeout", 0.9
  ; "test_process", 0.8
  ; "test_loop_features", 0.8
  ; "test_job_input", 0.8
  ; "test_acp_interaction", 0.7
  ; "test_reminders", 0.7
  ; "test_tool_host", 0.7
  ; "test_auto_background", 0.7
  ; "test_subagent", 0.6
  ; "test_background_jobs", 0.6
  ; "test_forge_rpc", 0.6
  ; "test_host_abort", 0.6
  ; "test_nix_store", 0.5
  ; "test_smart_approval", 0.5
  ; "test_review_compare", 0.5
  ; "test_worktree", 0.5
  ; "test_tools", 0.4
  ; "test_git_review", 0.4
  ; "test_forge_tools", 0.4
  ; "test_lock_dir", 0.4
  ; "test_agent_loop", 0.3
  ; "test_mcp_client", 0.3
  ; "test_host_heartbeat", 0.3
  ; "test_computer_consent", 0.3
  ]
;;

let default_cost = 0.1
let list_files_var = "PRIGH_INLINE_TEST_LIST_FILES"

let cost file =
  match List.assoc_opt (Filename.remove_extension file) costs with
  | Some c -> c
  | None -> default_cost
;;

(* As many shards as keep each within the slowest file's time, filled
   slowest first into the least loaded. *)
let shards files =
  let files =
    List.stable_sort (fun a b -> Float.compare (cost b) (cost a)) files
  in
  match files with
  | [] -> []
  | slowest :: _ ->
    let total = List.fold_left (fun acc f -> acc +. cost f) 0. files in
    let n = max 1 (int_of_float (Float.ceil (total /. cost slowest))) in
    let shards = Array.make n (0., []) in
    List.iter
      (fun file ->
         let least = ref 0 in
         Array.iteri
           (fun i (load, _) -> if load < fst shards.(!least) then least := i)
           shards;
         let load, files = shards.(!least) in
         shards.(!least) <- load +. cost file, file :: files)
      files;
    Array.to_list shards
    |> List.filter_map (function
      | _, [] -> None
      | _, files -> Some (List.sort String.compare files))
;;

let partitions () =
  let env = Array.append [| list_files_var ^ "=1" |] (Unix.environment ()) in
  let ((ic, oc, ec) as process) =
    Unix.open_process_args_full Sys.executable_name Sys.argv env
  in
  close_out oc;
  let files = In_channel.input_lines ic in
  prerr_string (In_channel.input_all ec);
  match Unix.close_process_full process with
  | WEXITED 0 -> files
  | _ -> exit 1
;;

let rec partition_arg = function
  | "-partition" :: p :: _ -> Some p
  | _ :: rest -> partition_arg rest
  | [] -> None
;;

let rec without_partition = function
  | "-partition" :: _ :: rest -> rest
  | x :: rest -> x :: without_partition rest
  | [] -> []
;;

let run () =
  let args = Array.to_list Sys.argv in
  if Sys.getenv_opt list_files_var <> None
  then Ppx_inline_test_lib.exit ()
  else if List.mem "-list-partitions" args
  then (
    List.iter
      (fun files -> print_endline (String.concat "," files))
      (shards (partitions ()));
    exit 0)
  else (
    match partition_arg args with
    | Some p when String.contains p ',' ->
      (* No file is named [p]: this process ran no test. *)
      let only =
        List.concat_map
          (fun file -> [ "-only-test"; file ])
          (String.split_on_char ',' p)
      in
      Unix.execv
        Sys.executable_name
        (Array.of_list (without_partition args @ only))
    | _ -> Ppx_inline_test_lib.exit ())
;;
