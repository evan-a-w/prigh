open! Core
open! Import

module Entry = struct
  type t =
    { chat : Chat.t
    ; paging : Paging.t
    ; newest : Message.t list
    }
  [@@deriving sexp_of]
end

type t =
  { entries : (string * Entry.t) list (** most recently used first *)
  ; loading : String.Set.t
  ; queued : string option
  }
[@@deriving sexp_of]

let empty = { entries = []; loading = String.Set.empty; queued = None }
let capacity = 8
let find t path = List.Assoc.find t.entries ~equal:String.equal path

let remove t path =
  { t with entries = List.Assoc.remove t.entries ~equal:String.equal path }
;;

let retain t ~paths =
  { t with
    entries = List.filter t.entries ~f:(fun (path, _) -> Set.mem paths path)
  }
;;

let set t path entry =
  { t with
    entries =
      List.take
        ((path, entry) :: List.Assoc.remove t.entries ~equal:String.equal path)
        capacity
  }
;;

let add_page t path (page : Message_page.t) ~running =
  match find t path with
  | Some cached when List.equal Message.equal cached.newest page.messages ->
    set t path cached, cached, `Unchanged
  | cached ->
    let entry =
      { Entry.chat = Chat.of_messages ~running page.messages
      ; paging = Paging.of_page page
      ; newest = page.messages
      }
    in
    set t path entry, entry, if Option.is_some cached then `Changed else `New
;;

let loading t path = Set.mem t.loading path

let set_loading t path loading =
  { t with
    loading =
      (if loading then Set.add t.loading path else Set.remove t.loading path)
  }
;;

let prefetch t path =
  if loading t path || Option.is_some (find t path)
  then t, None
  else if Set.is_empty t.loading
  then set_loading t path true, Some path
  else { t with queued = Some path }, None
;;

let next_prefetch t =
  match t.queued with
  | None -> t, None
  | Some path -> prefetch { t with queued = None } path
;;
