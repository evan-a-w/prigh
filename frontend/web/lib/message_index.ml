open! Core
open! Import

module Span = struct
  type t =
    { first : int
    ; next : int
    }
  [@@deriving sexp_of, equal]
end

type t =
  { first_entry : int
  ; older : int
  ; spans : Span.t Int.Map.t
  ; of_index : int Int.Map.t
  }
[@@deriving sexp_of]

let results chat (message : Message.Assistant.t) =
  List.count message.content ~f:(function
    | Tool_call call ->
      Option.exists (Chat.tool chat call.id) ~f:(fun tool ->
        Option.is_some tool.result)
    | Text _ | Thinking _ | Server_tool _ -> false)
;;

let compute ~(paging : Paging.t) chat =
  (* A command's output ([!command]: in the context; [!!command]: not) and a
     compaction since the transcript was loaded leave the indices after them
     unknown. *)
  let known =
    List.take_while (Chat.entries chat) ~f:(function
      | User _ | Assistant _ -> true
      | Shell _ | Compaction _ -> false)
  in
  let spans =
    List.folding_mapi
      known
      ~init:paging.older
      ~f:(fun i index (entry : Chat.Entry.t) ->
        let size =
          match entry with
          | Assistant { message; _ } -> 1 + results chat message
          | User _ | Shell _ | Compaction _ -> 1
        in
        ( index + size
        , (Chat.first chat + i, { Span.first = index; next = index + size }) ))
  in
  { first_entry = Chat.first chat
  ; older = paging.older
  ; spans = Int.Map.of_alist_exn spans
  ; of_index =
      Int.Map.of_alist_exn
        (List.map spans ~f:(fun (n, (span : Span.t)) -> span.first, n))
  }
;;

let last = ref None

let create ~paging chat =
  match !last with
  | Some (paging', chat', t)
    when Paging.equal paging paging' && phys_equal chat chat' -> t
  | _ ->
    let t = compute ~paging chat in
    last := Some (paging, chat, t);
    t
;;

let span t n = Map.find t.spans n
let entry_of_index t index = Map.find t.of_index index

let entry_at t index =
  match Map.closest_key t.of_index `Less_or_equal_to index with
  | Some (_, n) when (Map.find_exn t.spans n).next > index -> Some n
  | _ -> None
;;

let newest t = Option.map (Map.max_elt t.spans) ~f:fst

let after t ~divider =
  if divider = t.older
  then Some (t.first_entry - 1)
  else (
    match Map.closest_key t.of_index `Less_than divider with
    | Some (_, n) when (Map.find_exn t.spans n).next = divider -> Some n
    | _ -> None)
;;
