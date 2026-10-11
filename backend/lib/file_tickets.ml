open! Core
open! Import

module Kind = struct
  type t =
    | Download of { path : string }
    | Inline of
        { path : string
        ; mime : string
        }
    | Archive of { path : string }
    | Upload of
        { path : string
        ; conflict : File_ops.Conflict.t
        ; size : int
        ; make_dir : bool
        }
  [@@deriving sexp_of]
end

module Ticket = struct
  type t =
    { agent : Agent.t
    ; kind : Kind.t
    ; expires : float
    }
end

let lifetime = 300.
let tickets : Ticket.t String.Table.t = String.Table.create ()
let clock () = Core_unix.gettimeofday ()

let expire now =
  Hashtbl.filter_inplace tickets ~f:(fun (t : Ticket.t) ->
    Float.(t.expires > now))
;;

let issue ?(now = clock ()) agent kind =
  expire now;
  let id = Pkce.random_hex 16 in
  Hashtbl.set tickets ~key:id ~data:{ agent; kind; expires = now +. lifetime };
  id
;;

let take ?(now = clock ()) id =
  expire now;
  match Hashtbl.find tickets id with
  | None -> None
  | Some ticket ->
    (match ticket.kind with
     | Inline _ -> ()
     | Download _ | Archive _ | Upload _ -> Hashtbl.remove tickets id);
    Some ticket
;;

let url (kind : Kind.t) id =
  match kind with
  | Upload _ -> "/files/upload?ticket=" ^ id
  | Download _ | Inline _ | Archive _ -> "/files/download?ticket=" ^ id
;;
