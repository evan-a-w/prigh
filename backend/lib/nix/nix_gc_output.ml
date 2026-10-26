open! Core
open! Import

let parse_size s =
  match
    String.split (String.strip s) ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
  with
  | [ number; unit ] ->
    let scale =
      match unit with
      | "B" | "bytes" -> Some 1.
      | "KiB" -> Some 1024.
      | "MiB" -> Some (1024. ** 2.)
      | "GiB" -> Some (1024. ** 3.)
      | "TiB" -> Some (1024. ** 4.)
      | _ -> None
    in
    (match Float.of_string_opt number, scale with
     | Some n, Some scale when Float.(n >= 0.) ->
       Some (Float.iround_nearest_exn (n *. scale))
     | _ -> None)
  | _ -> None
;;

module Line = struct
  type t =
    | Deleting of string
    | Removing_generation
    | Summary of
        { paths : int
        ; bytes : int
        }
    | Other of string
  [@@deriving sexp_of]

  let summary_re =
    Re.Perl.compile_pat {|^(\d+) store paths? deleted, (.+) freed$|}
  ;;

  let classify line =
    let line = String.strip line in
    match String.chop_prefix line ~prefix:"deleting '" with
    | Some rest when String.is_suffix rest ~suffix:"'" ->
      Deleting (String.drop_suffix rest 1)
    | _ ->
      if String.is_prefix line ~prefix:"removing profile version "
      then Removing_generation
      else (
        match Re.exec_opt summary_re line with
        | Some groups ->
          (match
             ( Int.of_string_opt (Re.Group.get groups 1)
             , parse_size (Re.Group.get groups 2) )
           with
           | Some paths, Some bytes -> Summary { paths; bytes }
           | _ -> Other line)
        | None -> Other line)
  ;;
end

module Tally = struct
  type t =
    { mutable paths_deleted : int
    ; mutable generations_deleted : int
    ; mutable freed_bytes : int option
    }
  [@@deriving sexp_of]

  let create () =
    { paths_deleted = 0; generations_deleted = 0; freed_bytes = None }
  ;;

  let add t : Line.t -> unit = function
    | Deleting _ -> t.paths_deleted <- t.paths_deleted + 1
    | Removing_generation -> t.generations_deleted <- t.generations_deleted + 1
    | Summary { paths; bytes } ->
      (* Nix's count also covers paths it deleted without saying so. *)
      t.paths_deleted <- Int.max t.paths_deleted paths;
      t.freed_bytes <- Some bytes
    | Other _ -> ()
  ;;
end

module Splitter = struct
  type t =
    { buffer : Buffer.t
    ; on_line : string -> unit
    }

  let create on_line = { buffer = Buffer.create 256; on_line }

  let feed t chunk =
    String.iter chunk ~f:(fun c ->
      if Char.equal c '\n'
      then (
        let line = Buffer.contents t.buffer in
        Buffer.clear t.buffer;
        t.on_line line)
      else Buffer.add_char t.buffer c)
  ;;

  let flush t =
    if Buffer.length t.buffer > 0
    then (
      let line = Buffer.contents t.buffer in
      Buffer.clear t.buffer;
      t.on_line line)
  ;;
end
