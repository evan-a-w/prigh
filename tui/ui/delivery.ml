open! Core

module Section = struct
  type t =
    { kind : string
    ; id : string
    ; ok : bool
    ; status : string
    ; task : string
    ; body : string list
    }
  [@@deriving sexp_of]
end

let subagent_header line =
  let open Option.Let_syntax in
  let%bind rest = String.chop_prefix line ~prefix:"[subagent " in
  let%bind id, rest = String.lsplit2 rest ~on:' ' in
  let%bind ok, task =
    match String.chop_prefix rest ~prefix:"finished]" with
    | Some task -> Some (true, task)
    | None ->
      String.chop_prefix rest ~prefix:"failed]"
      |> Option.map ~f:(fun task -> false, task)
  in
  Some
    { Section.kind = "subagent"
    ; id
    ; ok
    ; status = (if ok then "finished" else "failed")
    ; task = String.strip task
    ; body = []
    }
;;

let job_header line =
  let open Option.Let_syntax in
  let%bind rest = String.chop_prefix line ~prefix:"[job " in
  let%bind id, rest = String.lsplit2 rest ~on:' ' in
  let%bind digits = String.chop_prefix id ~prefix:"j" in
  let%bind status, command = String.lsplit2 rest ~on:']' in
  if String.is_empty digits
     || (not (String.for_all digits ~f:Char.is_digit))
     || not
          (List.exists
             [ "exited "; "killed"; "timed out"; "failed" ]
             ~f:(fun prefix -> String.is_prefix status ~prefix))
  then None
  else
    Some
      { Section.kind = "job"
      ; id
      ; ok = String.equal status "exited 0"
      ; status
      ; task = String.strip command
      ; body = []
      }
;;

let header line = Option.first_some (subagent_header line) (job_header line)

let parse text =
  match String.split_lines text with
  | first :: _ as lines when Option.is_some (header first) ->
    let sections =
      List.fold lines ~init:[] ~f:(fun acc line ->
        match header line, acc with
        | Some section, _ -> section :: acc
        | None, current :: rest ->
          { current with body = line :: current.body } :: rest
        | None, [] -> acc)
    in
    Some
      (List.rev_map sections ~f:(fun s ->
         { s with
           body =
             List.rev s.body
             |> List.drop_while ~f:String.is_empty
             |> List.rev
             |> List.drop_while ~f:String.is_empty
             |> List.rev
         }))
  | _ -> None
;;
