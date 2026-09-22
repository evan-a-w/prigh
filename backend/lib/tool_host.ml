open! Core
open! Import

(* The tool-host worker (`prigh tool-host`): a JSON-lines loop the frontend
   spawns on its own machine when a session's tools should run there. The
   frontend forwards the backend's [tool_exec]/[tool_exec_cancel] events as

     {"type":"exec","exec_id":..,"name":..,"arguments":{..},"cwd":..}
     {"type":"cancel","exec_id":..}

   and relays our replies back to the backend as [tool_exec_output] and
   [tool_exec_result] requests:

     {"type":"output","exec_id":..,"chunk":..}
     {"type":"result","exec_id":..,"text":..,"is_error":..}

   Each exec runs in its own fiber, so parallel-safe tools overlap. *)

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let string_field json name =
  match field json name with
  | Some (`String s) -> Some s
  | _ -> None
;;

let run ~env ~input ~output =
  Switch.run
  @@ fun sw ->
  let outbox : string option Eio.Stream.t = Eio.Stream.create 1024 in
  let send json = Eio.Stream.add outbox (Some (Json.to_string json)) in
  Fiber.fork ~sw (fun () ->
    let rec loop () =
      match Eio.Stream.take outbox with
      | None -> ()
      | Some line ->
        Eio.Flow.copy_string (line ^ "\n") output;
        loop ()
    in
    loop ());
  let running : Cancellation.t String.Table.t = String.Table.create () in
  let exec json =
    match string_field json "exec_id", string_field json "name" with
    | Some exec_id, Some name ->
      let arguments =
        Option.value (field json "arguments") ~default:(`Object [])
      in
      let cwd =
        Option.value (string_field json "cwd") ~default:(Core_unix.getcwd ())
      in
      let cancel = Cancellation.create () in
      Hashtbl.set running ~key:exec_id ~data:cancel;
      Fiber.fork ~sw (fun () ->
        let result =
          match
            Host_ops.execute
              ~env
              ~cancel
              ~on_output:(fun chunk ->
                send
                  (`Object
                      [ "type", `String "output"
                      ; "exec_id", `String exec_id
                      ; "chunk", `String chunk
                      ]))
              ~cwd
              ~name
              ~arguments
          with
          | result -> result
          | exception exn ->
            Tool.Result.error (sprintf "%s failed: %s" name (Exn.to_string exn))
        in
        Hashtbl.remove running exec_id;
        send
          (`Object
              [ "type", `String "result"
              ; "exec_id", `String exec_id
              ; "text", `String result.text
              ; ("is_error", if result.is_error then `True else `False)
              ]))
    | _ -> ()
  in
  let reader = Eio.Buf_read.of_flow input ~max_size:(64 * 1024 * 1024) in
  let rec loop () =
    match Eio.Buf_read.line reader with
    | exception End_of_file -> ()
    | line ->
      (match Json.parse line with
       | Error _ -> ()
       | Ok json ->
         (match string_field json "type" with
          | Some "exec" -> exec json
          | Some "cancel" ->
            Option.iter (string_field json "exec_id") ~f:(fun exec_id ->
              Option.iter (Hashtbl.find running exec_id) ~f:Cancellation.cancel)
          | _ -> ()));
      loop ()
  in
  loop ();
  Hashtbl.iter running ~f:Cancellation.cancel;
  Eio.Stream.add outbox None
;;
