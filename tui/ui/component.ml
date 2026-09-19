open! Core
open Bonsai.Let_syntax
module P = Prigh_protocol

module Platform = struct
  type t =
    { rpc :
        string
        -> (string * P.Json.t) list
        -> (P.Json.t, string) Result.t Bonsai.Effect.t
    ; open_browser : string -> unit Bonsai.Effect.t
    ; quit : unit Bonsai.Effect.t
    }
end

let perform ctx (platform : Platform.t) (command : App.Command.t) =
  let effect =
    match command with
    | Rpc { method_; params; tag } ->
      let%bind.Bonsai.Effect result = platform.rpc method_ params in
      Bonsai.Apply_action_context.inject ctx (App.Action.Reply (tag, result))
    | Open_browser url -> platform.open_browser url
    | Quit -> platform.quit
  in
  Bonsai.Apply_action_context.schedule_event ctx effect
;;

let create platform (local_ graph) =
  let model, inject =
    Bonsai.state_machine_with_input
      ~sexp_of_action:App.Action.sexp_of_t
      ~default_model:App.init
      ~apply_action:(fun ctx input model action ->
        let model, commands = App.update model action in
        (match input with
         | Bonsai.Computation_status.Active platform ->
           List.iter commands ~f:(perform ctx platform)
         | Inactive -> ());
        model)
      platform
      graph
  in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr inject in
       inject App.Action.Start)
    graph;
  let running =
    let%arr model in
    App.Model.running model && not (Mode.is_dialog model.mode)
  in
  let (_ : unit Bonsai.t) =
    match%sub running with
    | true ->
      Bonsai.Clock.every
        ~when_to_start_next_effect:`Every_multiple_of_period_blocking
        (Bonsai.return (Time_ns.Span.of_int_ms 100))
        (let%arr inject in
         inject App.Action.Tick)
        graph;
      Bonsai.return ()
    | false -> Bonsai.return ()
  in
  model, inject
;;
