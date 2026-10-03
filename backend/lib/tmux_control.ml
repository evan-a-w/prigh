open! Core
open! Import

module Event = struct
  type t =
    | Output of string
    | Reply of (string list, string list) Result.t
    | Exit
  [@@deriving sexp_of, equal]
end

module Block = struct
  type t =
    { id : string (** "<time> <number> <flags>", repeated by the closing line *)
    ; from_us : bool
    ; lines : string Queue.t
    }
end

type t =
  { partial : Buffer.t
  ; mutable block : Block.t option
  }

let create () = { partial = Buffer.create 256; block = None }

(* tmux escapes bytes below 0x20 and backslash as three octal digits. *)
let decode_output s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let is_octal c = Char.( >= ) c '0' && Char.( <= ) c '7' in
  let rec go i =
    if i < n
    then
      if
        Char.equal s.[i] '\\'
        && i + 3 < n
        && is_octal s.[i + 1]
        && is_octal s.[i + 2]
        && is_octal s.[i + 3]
      then (
        let digit j = Char.to_int s.[j] - Char.to_int '0' in
        Buffer.add_char
          buf
          (Char.of_int_exn
             (((digit (i + 1) * 64) + (digit (i + 2) * 8) + digit (i + 3))
              land 0xff));
        go (i + 4))
      else (
        Buffer.add_char buf s.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents buf
;;

let line t line =
  match t.block with
  | Some block ->
    (match String.lsplit2 line ~on:' ' with
     | Some ((("%end" | "%error") as kind), id) when String.equal id block.id ->
       t.block <- None;
       if block.from_us
       then (
         let lines = Queue.to_list block.lines in
         Some
           (Event.Reply
              (if String.equal kind "%end" then Ok lines else Error lines)))
       else None
     | _ ->
       Queue.enqueue block.lines line;
       None)
  | None ->
    (match String.lsplit2 line ~on:' ' with
     | Some ("%begin", id) ->
       let from_us =
         match String.split id ~on:' ' with
         | [ _; _; flags ] ->
           Option.value_map
             (Int.of_string_opt flags)
             ~default:false
             ~f:(fun f -> f land 1 = 1)
         | _ -> false
       in
       t.block <- Some { id; from_us; lines = Queue.create () };
       None
     | Some ("%output", rest) ->
       (match String.lsplit2 rest ~on:' ' with
        | Some (_pane, data) -> Some (Event.Output (decode_output data))
        | None -> None)
     | Some ("%exit", _) -> Some Exit
     | _ -> if String.equal line "%exit" then Some Exit else None)
;;

let feed t data =
  Buffer.add_string t.partial data;
  let contents = Buffer.contents t.partial in
  match String.rsplit2 contents ~on:'\n' with
  | None -> []
  | Some (complete, rest) ->
    Buffer.clear t.partial;
    Buffer.add_string t.partial rest;
    String.split complete ~on:'\n' |> List.filter_map ~f:(line t)
;;

let send_keys ~target data =
  String.to_list data
  |> List.chunks_of ~length:256
  |> List.map ~f:(fun chunk ->
    sprintf
      "send-keys -t %s -H %s"
      target
      (String.concat
         ~sep:" "
         (List.map chunk ~f:(fun c -> sprintf "%02x" (Char.to_int c)))))
;;
