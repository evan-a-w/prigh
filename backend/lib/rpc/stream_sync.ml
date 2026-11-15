open! Core
open! Import

module Update = struct
  type t =
    { key : string
    ; stream : int
    ; seq : int
    }
  [@@deriving sexp_of]

  let rec of_event_at key : Agent_event.t -> t option = function
    | Message_update { stream; seq; _ } -> Some { key; stream; seq }
    | Subagent { agent_id; event; _ } ->
      of_event_at (key ^ "/" ^ agent_id) event
    | _ -> None
  ;;

  let of_event = of_event_at ""
end

module Send = struct
  type t =
    | Delta
    | Resync
    | Skip
  [@@deriving sexp_of]
end

module Sent = struct
  type t =
    { stream : int
    ; seq : int
    }
end

type t = Sent.t String.Table.t

let create () = String.Table.create ()
let reset t = Hashtbl.clear t

let send t ({ key; stream; seq } : Update.t) : Send.t =
  let decision : Send.t =
    match Hashtbl.find t key with
    | Some (sent : Sent.t) when sent.stream = stream && seq <= sent.seq -> Skip
    | Some sent when sent.stream = stream && seq = sent.seq + 1 -> Delta
    | Some _ | None -> if seq = 1 then Delta else Resync
  in
  (match decision with
   | Skip -> ()
   | Delta | Resync -> Hashtbl.set t ~key ~data:{ stream; seq });
  decision
;;
