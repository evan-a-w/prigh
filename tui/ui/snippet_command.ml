open! Core
open! Import

module Action = struct
  type t =
    | Pick of P.Snippet.t list
    | Insert of P.Snippet.t
    | Save of
        { snippets : P.Snippet.t list
        ; notice : string
        }
    | Info of string
    | Error of string
  [@@deriving sexp_of]
end

let usage = "/snippet [NAME] · /snippet add NAME [TEXT] · /snippet rm NAME"
let none_yet = "no snippets yet: /snippet add NAME TEXT saves one"
let not_loaded = "the config has not loaded yet; try again in a moment"

let pick : P.Snippet.t list option -> Action.t = function
  | None -> Error not_loaded
  | Some [] -> Info none_yet
  | Some snippets -> Pick snippets
;;

(* The first word (up to any whitespace, newlines too) and what follows it,
   stripped. *)
let split_word s =
  let s = String.lstrip s in
  match String.lfindi s ~f:(fun _ c -> Char.is_whitespace c) with
  | None -> s, ""
  | Some i -> String.prefix s i, String.strip (String.drop_prefix s i)
;;

let closest snippets name =
  let names = List.map snippets ~f:(fun (s : P.Snippet.t) -> s.name) in
  let near =
    List.filter_map names ~f:(fun n ->
      let d = Edit_distance.distance n name in
      Option.some_if (d <= 2) (d, n))
    |> List.stable_sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> List.map ~f:snd
  in
  near @ Fuzzy.rank ~query:name names ~key:Fn.id
  |> List.stable_dedup ~compare:String.compare
  |> Fn.flip List.take 3
;;

let unknown snippets name : Action.t =
  match snippets with
  | [] -> Error (sprintf "unknown snippet %S; %s" name none_yet)
  | _ ->
    (match closest snippets name with
     | [] ->
       Error
         (sprintf "unknown snippet %S; /snippet lists the %d saved" name
            (List.length snippets))
     | names ->
       Error
         (sprintf "unknown snippet %S; did you mean %s? (/snippet lists them)"
            name
            (String.concat ~sep:", " names)))
;;

let add snippets ~last_prompt rest : Action.t =
  let name, text = split_word rest in
  let text =
    if String.is_empty text
    then Option.map last_prompt ~f:String.strip
    else Some text
  in
  match P.Snippet.name_problem name, text with
  | _, _ when String.is_empty name ->
    Error "usage: /snippet add NAME [TEXT] (without TEXT: your last prompt)"
  | Some problem, _ -> Error problem
  | None, _ when List.mem [ "add"; "rm" ] name ~equal:String.equal ->
    Error (sprintf "%S is a /snippet subcommand; pick another name" name)
  | None, (None | Some "") ->
    Error
      (sprintf
         "no TEXT and no prompt sent yet: /snippet add %s TEXT"
         name)
  | None, Some text ->
    let verb =
      if Option.is_some (P.Snippet.find snippets name) then "replaced" else "saved"
    in
    Save
      { snippets = P.Snippet.add snippets { name; text }
      ; notice =
          sprintf
            "snippet %s %s: %s (/snippet %s inserts it)"
            name
            verb
            (P.Snippet.first_line { name; text })
            name
      }
;;

let run ~snippets ~last_prompt rest : Action.t =
  let word, after = split_word rest in
  match word, snippets with
  | "", _ -> pick snippets
  | ("add" | "rm"), None -> Error not_loaded
  | "add", Some snippets -> add snippets ~last_prompt after
  | "rm", Some snippets ->
    (match split_word after with
     | "", _ -> Error "usage: /snippet rm NAME"
     | name, "" ->
       if Option.is_some (P.Snippet.find snippets name)
       then
         Save
           { snippets = P.Snippet.remove snippets name
           ; notice = sprintf "snippet %s removed" name
           }
       else unknown snippets name
     | _ -> Error "usage: /snippet rm NAME")
  | _, None -> Error not_loaded
  | name, Some snippets when String.is_empty after ->
    (match P.Snippet.find snippets name with
     | Some snippet -> Insert snippet
     | None -> unknown snippets name)
  | _ -> Error ("usage: " ^ usage)
;;

let config_fields snippets = [ "snippets", P.Snippet.list_to_json snippets ]
