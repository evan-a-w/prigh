open! Core
open! Import

let max_length = 48
let task_chars = 4_000

let of_given name =
  match Naming.clean ~max_length name with
  | "" -> None
  | name -> Some name
;;

let first_sentence line =
  let n = String.length line in
  let rec go i =
    if i >= n - 1
    then line
    else (
      match line.[i] with
      | '.' | '!' | '?' when Char.is_whitespace line.[i + 1] -> String.prefix line i
      | _ -> go (i + 1))
  in
  go 0
;;

(* ["Implement on a branch `x`: the rest"]: the rest, when the clause before
   the colon is short and something follows it. *)
let drop_lead_in line =
  match String.lsplit2 line ~on:':' with
  | Some (lead, rest)
    when String.length lead <= 60 && String.length (String.strip rest) >= 8 ->
    String.strip rest
  | Some _ | None -> line
;;

let of_task task =
  let line =
    String.split_lines task
    |> List.map ~f:String.strip
    |> List.find ~f:(Fn.non String.is_empty)
    |> Option.value ~default:""
  in
  let line = drop_lead_in line in
  match Naming.clean ~max_length (first_sentence line) with
  | "" -> "Subagent"
  | name -> name
;;

let instructions =
  "You name the subagents a coding agent starts, for the user's list of \
   them. Write a name of two to five words saying what this subagent does, \
   in the imperative and sentence case, e.g. \"Fix completion after a \
   space\", \"Review the parser changes\", \"Survey host naming code\". Make \
   it differ from the other subagents' names, if any are listed: say what \
   sets this one apart. Reply with the name only: no quotes, no trailing \
   period, no preamble."
;;

let prompt ~task ~siblings =
  let task =
    let task = String.strip task in
    if String.length task <= task_chars
    then task
    else String.prefix task task_chars ^ "…"
  in
  sprintf
    "Task:\n%s%s"
    task
    (match siblings with
     | [] -> ""
     | names ->
       "\n\nOther subagents:\n"
       ^ String.concat ~sep:"\n" (List.map names ~f:(fun n -> "- " ^ n)))
;;

let ask ~provider ~model ?cancel ~task ~siblings () =
  Or_error.map
    (Naming.ask
       ~provider
       ~model
       ?cancel
       ~what:"naming the subagent"
       ~system:instructions
       (prompt ~task ~siblings))
    ~f:(Naming.clean ~max_length)
;;
