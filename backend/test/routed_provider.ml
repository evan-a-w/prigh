open! Core
open! Prigh

(* A scripted provider with one script per agent, so concurrent background
   subagents do not steal each other's replies: requests whose system prompt
   is a subagent's go to the script named by their task (the first user
   message), everything else to [main]. [before ~key] runs before each reply,
   e.g. to hold a subagent until the test releases it. *)

let key_of (request : Provider.Request.t) =
  let is_subagent =
    Option.exists request.system ~f:(fun s ->
      String.is_substring s ~substring:"You are a subagent")
  in
  if not is_subagent
  then "main"
  else (
    match request.messages with
    | Message.User { text } :: _ -> text
    | _ -> "?")
;;

let create
      ?(before = fun ~key:_ -> ())
      ?(on_request = fun ~key:_ _ -> ())
      ~main
      children
  =
  let providers =
    String.Map.of_alist_exn
      (("main", Faux_provider.create main)
       :: List.map children ~f:(fun (key, replies) ->
         key, Faux_provider.create replies))
  in
  { Provider.name = "routed"
  ; stream =
      (fun request ~cancel ~on_event ->
        let key = key_of request in
        on_request ~key request;
        ignore
          (Cancellation.protect cancel ~f:(fun () -> before ~key) : unit option);
        match Map.find providers key with
        | Some provider -> provider.stream request ~cancel ~on_event
        | None -> raise_s [%message "no script for agent" (key : string)])
  }
;;

(* A gate per key: [hold] makes that agent's requests block until [release]. *)
module Gates = struct
  type t = (unit Eio.Promise.t * unit Eio.Promise.u) String.Table.t

  let create () : t = String.Table.create ()
  let hold (t : t) key = Hashtbl.set t ~key ~data:(Eio.Promise.create ())

  let release (t : t) key =
    Option.iter (Hashtbl.find_and_remove t key) ~f:(fun (_, u) ->
      Eio.Promise.resolve u ())
  ;;

  let before (t : t) ~key =
    Option.iter (Hashtbl.find t key) ~f:(fun (p, _) -> Eio.Promise.await p)
  ;;
end
