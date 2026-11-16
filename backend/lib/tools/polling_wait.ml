open! Core
open! Import

let max_bounded_seconds = 120.

(* Words with quotes and shell punctuation removed, so that a loop inside
   [bash -c '...'] is seen like one outside it. *)
let words command =
  String.split_on_chars
    command
    ~on:
      [ ' '; '\t'; '\n'; '\r'; ';'; '&'; '|'; '('; ')'; '{'; '}'; '`'; '\''
      ; '"'; '<'; '>'; '\\'; '$'
      ]
  |> List.filter ~f:(Fn.non String.is_empty)
  |> Array.of_list
;;

(* Seconds, as [sleep] and [timeout] read them. *)
let duration word =
  if String.equal word "infinity"
  then Some Float.infinity
  else (
    let number, scale =
      match String.suffix word 1 with
      | "s" -> String.drop_suffix word 1, 1.
      | "m" -> String.drop_suffix word 1, 60.
      | "h" -> String.drop_suffix word 1, 3600.
      | "d" -> String.drop_suffix word 1, 86400.
      | _ -> word, 1.
    in
    Option.map (Float.of_string_opt number) ~f:(fun n -> n *. scale))
;;

let word_at words i = if i < Array.length words then Some words.(i) else None

(* What each [sleep] in [words.(first) .. words.(last)] sleeps; an unknown
   time (a variable) counts as 1 second. *)
let sleeps words ~first ~last =
  List.filter_map (List.range first (last + 1)) ~f:(fun i ->
    if String.equal words.(i) "sleep"
    then
      Some
        (Option.value
           (Option.bind (word_at words (i + 1)) ~f:duration)
           ~default:1.)
    else None)
;;

let timeout_seconds words i =
  let rec arg j =
    match word_at words j with
    | Some ("-s" | "-k" | "--signal" | "--kill-after") -> arg (j + 2)
    | Some flag when String.is_prefix flag ~prefix:"-" -> arg (j + 1)
    | Some word -> duration word
    | None -> None
  in
  arg (i + 1)
;;

(* A [timeout] of at most [max_bounded_seconds] before [words.(i)]. *)
let bounded words i =
  List.exists (List.range 0 i) ~f:(fun m ->
    String.equal words.(m) "timeout"
    &&
    match timeout_seconds words m with
    | Some s -> Float.( <= ) s max_bounded_seconds
    | None -> false)
;;

let find_from words i ~f =
  List.find (List.range i (Array.length words)) ~f:(fun j -> f words.(j))
;;

(* The index of the [done] closing the [do] at [j]. *)
let matching_done words j =
  let rec go k depth =
    if k >= Array.length words
    then None
    else (
      match words.(k) with
      | "do" -> go (k + 1) (depth + 1)
      | "done" when depth = 1 -> Some k
      | "done" -> go (k + 1) (depth - 1)
      | _ -> go (k + 1) depth)
  in
  go (j + 1) 1
;;

let int_word w = Int.of_string_opt w

(* How many items [for VAR in LIST] goes through, when that is known. *)
let item_count list =
  let ints = List.filter_map list ~f:int_word in
  match list with
  | _ when List.mem list "seq" ~equal:String.equal ->
    (match ints with
     | [ n ] -> Some n
     | [ a; b ] -> Some (b - a + 1)
     | [ a; step; b ] when step <> 0 -> Some (((b - a) / step) + 1)
     | _ -> None)
  | [ range ] when String.is_substring range ~substring:".." ->
    (match String.Search_pattern.split_on (String.Search_pattern.create "..") range with
     | [ a; b ] ->
       (match int_word a, int_word b with
        | Some a, Some b -> Some (abs (b - a) + 1)
        | _ -> None)
     | _ -> None)
  | list
    when List.exists list ~f:(fun w ->
           String.exists w ~f:(fun c -> Char.equal c '*' || Char.equal c '?'))
    -> None
  | list -> Some (List.length list)
;;

let polling_loop ?timeout command =
  let words = words command in
  let call_bounded =
    match timeout with
    | Some s -> Float.( <= ) (Float.of_int s) max_bounded_seconds
    | None -> false
  in
  if call_bounded
  then None
  else
    List.find_map (List.range 0 (Array.length words)) ~f:(fun i ->
      let loop ~body_from ~each =
        Option.bind
          (find_from words body_from ~f:(String.equal "do"))
          ~f:(fun j ->
            Option.bind (matching_done words j) ~f:(fun last ->
              each (sleeps words ~first:(i + 1) ~last) ~list_end:j))
      in
      match words.(i) with
      | ("while" | "until") as keyword when not (bounded words i) ->
        loop ~body_from:(i + 1) ~each:(fun sleeps ~list_end:_ ->
          Option.some_if
            (List.exists sleeps ~f:(fun s -> Float.( >= ) s 1.))
            (sprintf
               "%s %s loop that sleeps"
               (if String.equal keyword "until" then "an" else "a")
               keyword))
      | "for" when not (bounded words i) ->
        (match word_at words (i + 2) with
         | Some "in" ->
           loop ~body_from:(i + 3) ~each:(fun sleeps ~list_end ->
             let list =
               List.map (List.range (i + 3) list_end) ~f:(fun k -> words.(k))
             in
             let per_item = List.sum (module Float) sleeps ~f:Fn.id in
             match item_count list with
             | Some n
               when Float.( > ) (Float.of_int n *. per_item) max_bounded_seconds
               ->
               Some
                 (sprintf
                    "a for loop that sleeps %s in all"
                    (Background_tasks.seconds_text (Float.of_int n *. per_item)))
             | _ -> None)
         | _ -> None)
      | _ -> None)
;;

(* Shell words from [s] up to the end of its simple command: quotes
   removed, backslashes escaping. *)
let shell_words s =
  let words = Queue.create () in
  let word = Buffer.create 16 in
  let in_word = ref false in
  let flush () =
    if !in_word then Queue.enqueue words (Buffer.contents word);
    Buffer.clear word;
    in_word := false
  in
  let n = String.length s in
  let rec go i quote =
    if i < n
    then (
      let c = s.[i] in
      match quote, c with
      | None, (';' | '|' | '&' | ')' | '\n') -> ()
      | None, (' ' | '\t') ->
        flush ();
        go (i + 1) None
      | None, ('\'' | '"') ->
        in_word := true;
        go (i + 1) (Some c)
      | None, '\\' when i + 1 < n ->
        in_word := true;
        Buffer.add_char word s.[i + 1];
        go (i + 2) None
      | Some q, c when Char.equal q c -> go (i + 1) None
      | Some '"', '\\' when i + 1 < n ->
        Buffer.add_char word s.[i + 1];
        go (i + 2) quote
      | _, c ->
        in_word := true;
        Buffer.add_char word c;
        go (i + 1) quote)
  in
  go 0 None;
  flush ();
  Queue.to_list words
;;

let takes_value =
  [ "-d"; "-g"; "-G"; "-P"; "-s"; "-t"; "-u"; "-U"; "-F"; "--delimiter"
  ; "--pgroup"; "--group"; "--parent"; "--session"; "--terminal"; "--euid"
  ; "--uid"; "--pidfile"; "--ns"; "--nslist"; "--signal"
  ]
;;

(* The pattern of a [pgrep -f]/[pkill -f] in [args]. *)
let full_pattern args =
  let rec go args ~full =
    match args with
    | [] -> None
    | flag :: _ :: rest when List.mem takes_value flag ~equal:String.equal ->
      go rest ~full
    | "--full" :: rest -> go rest ~full:true
    | flag :: rest when String.is_prefix flag ~prefix:"--" -> go rest ~full
    | flag :: rest when String.is_prefix flag ~prefix:"-" ->
      go rest ~full:(full || String.mem flag 'f')
    | pattern :: _ -> Option.some_if full pattern
  in
  go args ~full:false
;;

let process_tool_re = Re.compile (Re.Perl.re {|\b(pgrep|pkill)\b|})

let matches_itself ~pattern command =
  let own = "bash -c " ^ command in
  match Re.Posix.compile_pat pattern with
  | re -> Re.execp re own
  | exception _ -> String.is_substring own ~substring:pattern
;;

(* [pattern] with its first letter or digit in brackets, which still matches
   what it did but not its own text. *)
let bracketed pattern =
  match String.lfindi pattern ~f:(fun _ c -> Char.is_alphanum c) with
  | Some i when i = 0 || not (Char.equal pattern.[i - 1] '\\') ->
    sprintf
      "%s[%c]%s"
      (String.prefix pattern i)
      pattern.[i]
      (String.drop_prefix pattern (i + 1))
  | _ -> pattern
;;

let self_match command =
  List.find_map (Re.all process_tool_re command) ~f:(fun group ->
    let tool = Re.Group.get group 1 in
    let rest = String.drop_prefix command (Re.Group.stop group 1) in
    Option.bind (full_pattern (shell_words rest)) ~f:(fun pattern ->
      Option.some_if (matches_itself ~pattern command) (tool, pattern)))
;;

let wait_instead =
  "To wait for a command that takes long (a build, tests, an install), start \
   it with bash and background: true, then keep working or end your turn: its \
   report ([job ID exited N] and its last lines) arrives when it exits. For a \
   process already running that is not a job, wait on its pid: timeout 600 \
   tail --pid=PID -f /dev/null."
;;

let check ?timeout command =
  match self_match command with
  | Some (tool, pattern) ->
    Some
      (sprintf
         "Not run: %s -f '%s' matches this command's own command line (the \
          shell running it is bash -c with this text, which contains the \
          pattern), so %s. Bracket a character so the pattern cannot match \
          itself: %s -f '%s'. %s"
         tool
         pattern
         (if String.equal tool "pkill"
          then "it would kill this very shell"
          else "it always finds a process, even when nothing else matches")
         tool
         (bracketed pattern)
         wait_instead)
  | None ->
    Option.map (polling_loop ?timeout command) ~f:(fun what ->
      sprintf
        "Not run: this command waits by polling (%s), which blocks you for as \
         long as it waits and spins until its timeout if what it waits for \
         never comes. %s To wait for a server to come up, bound the loop with \
         a short timeout: timeout 60 bash -c 'until curl -sf URL; do sleep 1; \
         done' (loops under a timeout of at most %s run)."
        what
        wait_instead
        (Background_tasks.seconds_text max_bounded_seconds))
;;
