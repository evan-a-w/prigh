open! Core
open! Import

let variable = "PRIGH_BASH_CALL"
let counter = ref 0

let marker () =
  incr counter;
  sprintf
    "%d-%d-%Ld"
    (Pid.to_int (Core_unix.getpid ()))
    !counter
    (Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) |> Int63.to_int64)
;;

module Process_info = struct
  type t =
    { pid : Pid.t
    ; command : string
    }
end

let read path =
  match In_channel.read_all path with
  | s -> Some s
  | exception _ -> None
;;

let find ~marker =
  let entry = variable ^ "=" ^ marker in
  let self = Pid.to_int (Core_unix.getpid ()) in
  match Sys_unix.readdir "/proc" with
  | exception _ -> []
  | names ->
    Array.to_list names
    |> List.filter_map ~f:Int.of_string_opt
    |> List.filter ~f:(fun pid -> pid <> self)
    |> List.sort ~compare:Int.compare
    |> List.filter_map ~f:(fun pid ->
      let open Option.Let_syntax in
      let%bind environ = read (sprintf "/proc/%d/environ" pid) in
      if List.mem (String.split environ ~on:'\000') entry ~equal:String.equal
      then (
        let%map cmdline = read (sprintf "/proc/%d/cmdline" pid) in
        { Process_info.pid = Pid.of_int pid
        ; command =
            String.split cmdline ~on:'\000'
            |> List.filter ~f:(Fn.non String.is_empty)
            |> String.concat ~sep:" "
        })
      else None)
;;

let max_shown = 5
let max_command = 100

let note processes =
  match processes with
  | [] -> None
  | processes ->
    let n = List.length processes in
    let shown =
      List.take processes max_shown
      |> List.map ~f:(fun { Process_info.pid; command } ->
        let command =
          if String.length command > max_command
          then String.prefix command max_command ^ "..."
          else command
        in
        sprintf "  %d %s" (Pid.to_int pid) command)
    in
    Some
      (sprintf
         "[the command left %s running, detached from this call:\n\
          %s%s\n\
          They are not jobs: nothing reports when they exit, so do not poll \
          for them. To run something long or lasting, use bash with \
          background: true instead, and you are notified when it exits. Stop \
          these with kill PID if they are not needed.]"
         (if n = 1 then "1 process" else sprintf "%d processes" n)
         (String.concat ~sep:"\n" shown)
         (if n > max_shown then sprintf "\n  ... and %d more" (n - max_shown)
          else ""))
;;
