open! Core
open! Import

(* "pid (comm) state ppid ...", where comm may hold spaces and parens. *)
let parent_of_stat stat =
  let open Option.Let_syntax in
  let%bind close = String.rindex stat ')' in
  match
    String.split (String.strip (String.drop_prefix stat (close + 1))) ~on:' '
  with
  | _state :: ppid :: _ -> Int.of_string_opt ppid
  | _ -> None
;;

(* The kernel's threads and init have parent 0. *)
let pair pid ppid =
  if pid > 0 && ppid > 0 then Some (Pid.of_int pid, Pid.of_int ppid) else None
;;

let from_proc () =
  match Sys_unix.readdir "/proc" with
  | exception _ -> None
  | entries ->
    Some
      (Array.to_list entries
       |> List.filter_map ~f:(fun name ->
         let open Option.Let_syntax in
         let%bind pid = Int.of_string_opt name in
         let%bind ppid =
           match In_channel.read_all (sprintf "/proc/%d/stat" pid) with
           | stat -> parent_of_stat stat
           | exception _ -> None
         in
         pair pid ppid))
;;

let from_ps ~env =
  match
    Eio.Process.parse_out
      (Eio.Stdenv.process_mgr env)
      Eio.Buf_read.take_all
      [ "ps"; "-A"; "-o"; "pid="; "-o"; "ppid=" ]
  with
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception _ -> None
  | out ->
    Some
      (String.split_lines out
       |> List.filter_map ~f:(fun line ->
         match
           String.split (String.strip line) ~on:' '
           |> List.filter ~f:(Fn.non String.is_empty)
         with
         | [ pid; ppid ] ->
           (match Int.of_string_opt pid, Int.of_string_opt ppid with
            | Some pid, Some ppid -> pair pid ppid
            | _ -> None)
         | _ -> None))
;;

let parents ~env =
  match from_proc () with
  | Some (_ :: _ as parents) -> Some parents
  | Some [] | None -> from_ps ~env
;;

let descendants parents ~root =
  let children = Pid.Table.create () in
  List.iter parents ~f:(fun (pid, ppid) ->
    if not (Pid.equal pid ppid)
    then Hashtbl.add_multi children ~key:ppid ~data:pid);
  let rec walk seen = function
    | [] -> seen
    | pid :: rest ->
      let fresh =
        Hashtbl.find_multi children pid
        |> List.filter ~f:(fun child -> not (Set.mem seen child))
      in
      walk (List.fold fresh ~init:seen ~f:Set.add) (fresh @ rest)
  in
  Set.to_list (walk Pid.Set.empty [ root ])
;;

let signal s target =
  try Signal_unix.send_i s target with
  | _ -> (* not ours to signal (setuid) *) ()
;;

let kill ~env ~root =
  signal Signal.stop (`Group root);
  (* Each round stops what the stopped ones had started meanwhile. *)
  let rec freeze stopped rounds =
    match parents ~env with
    | None -> stopped
    | Some parents ->
      let fresh =
        List.filter (descendants parents ~root) ~f:(fun pid ->
          not (Set.mem stopped pid))
      in
      List.iter fresh ~f:(fun pid -> signal Signal.stop (`Pid pid));
      let stopped = List.fold fresh ~init:stopped ~f:Set.add in
      if List.is_empty fresh || rounds = 0
      then stopped
      else freeze stopped (rounds - 1)
  in
  let tree = freeze Pid.Set.empty 4 in
  signal Signal.kill (`Group root);
  Set.iter tree ~f:(fun pid -> signal Signal.kill (`Pid pid))
;;
