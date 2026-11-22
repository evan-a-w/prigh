open! Core
open! Import

let variable = "PRIGH_BASH_CALL"

let read path =
  match In_channel.read_all path with
  | s -> Some s
  | exception _ -> None
;;

let start_ticks stat =
  (* After the command's name, which may hold spaces and parentheses: the
     start time is the 20th field after it. Read for every process at each
     call, so a scan that allocates nothing. *)
  let length = String.length stat in
  let rec last_close i =
    if i < 0
    then None
    else if Char.equal stat.[i] ')'
    then Some i
    else last_close (i - 1)
  in
  let rec field i ~left =
    (* [i] is at a separator: the next field's start, [left] fields on. *)
    if i >= length
    then None
    else if Char.equal stat.[i] ' '
    then field (i + 1) ~left
    else if left = 0
    then number i 0 ~digits:0
    else skip i ~left
  and skip i ~left =
    if i >= length
    then None
    else if Char.equal stat.[i] ' '
    then field i ~left:(left - 1)
    else skip (i + 1) ~left
  and number i acc ~digits =
    if i < length && Char.is_digit stat.[i]
    then
      number
        (i + 1)
        ((acc * 10) + Char.get_digit_exn stat.[i])
        ~digits:(digits + 1)
    else if
      digits > 0
      && (i = length || Char.equal stat.[i] ' ' || Char.equal stat.[i] '\n')
    then Some acc
    else None
  in
  let%bind.Option close = last_close (length - 1) in
  field (close + 1) ~left:19
;;

(* Clock ticks since boot, the unit of [start_ticks]. *)
let now_ticks () =
  let%bind.Option uptime = read "/proc/uptime" in
  let%bind.Option seconds =
    List.hd (String.split uptime ~on:' ') |> Option.bind ~f:Float.of_string_opt
  in
  let%map.Option per_second = Core_unix.sysconf CLK_TCK in
  Float.iround_down_exn (seconds *. Int64.to_float per_second)
;;

module Marker = struct
  type t =
    { value : string
    ; since : int option
    }

  let counter = ref 0

  let create () =
    (* Read before the value exists: no process carrying it can have started
       before. Two ticks' slack covers the clocks' rounding. *)
    let since = Option.map (now_ticks ()) ~f:(fun ticks -> ticks - 2) in
    incr counter;
    { value =
        sprintf
          "%d-%d-%Ld"
          (Pid.to_int (Core_unix.getpid ()))
          !counter
          (Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) |> Int63.to_int64)
    ; since
    }
  ;;

  let value t = t.value
end

module Process_info = struct
  type t =
    { pid : Pid.t
    ; command : string
    }
end

let has_entry environ ~entry =
  let length = String.length entry in
  let rec from pos =
    match String.substr_index environ ~pos ~pattern:entry with
    | None -> false
    | Some i ->
      ((i = 0 || Char.equal environ.[i - 1] '\000')
       && (i + length = String.length environ
           || Char.equal environ.[i + length] '\000'))
      || from (i + 1)
  in
  from 0
;;

let find (marker : Marker.t) =
  let entry = variable ^ "=" ^ marker.value in
  let self = Pid.to_int (Core_unix.getpid ()) in
  (* One small buffer for every process's stat (a few hundred bytes): most
     processes are older than the call, and this is all that is read of
     them. *)
  let buffer = Bytes.create 1024 in
  let read_stat pid =
    match
      Core_unix.openfile (sprintf "/proc/%d/stat" pid) ~mode:[ O_RDONLY ]
    with
    | exception _ -> None
    | fd ->
      Exn.protect
        ~finally:(fun () -> Core_unix.close fd)
        ~f:(fun () ->
          match Core_unix.read fd ~buf:buffer with
          | exception _ -> None
          | n -> Some (Bytes.To_string.sub buffer ~pos:0 ~len:n))
  in
  let recent pid =
    match marker.since with
    | None -> true
    | Some since ->
      (match Option.bind (read_stat pid) ~f:start_ticks with
       | Some started -> started >= since
       | None -> false)
  in
  match Sys_unix.readdir "/proc" with
  | exception _ -> []
  | names ->
    Array.to_list names
    |> List.filter_map ~f:Int.of_string_opt
    |> List.filter ~f:(fun pid -> pid <> self && recent pid)
    |> List.sort ~compare:Int.compare
    |> List.filter_map ~f:(fun pid ->
      let open Option.Let_syntax in
      let%bind environ = read (sprintf "/proc/%d/environ" pid) in
      if has_entry environ ~entry
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
         (if n > max_shown
          then sprintf "\n  ... and %d more" (n - max_shown)
          else ""))
;;
