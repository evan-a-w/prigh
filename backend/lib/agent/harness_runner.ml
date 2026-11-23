open! Core
open! Import

type t =
  { provider : Provider.t
  ; config : cancel:Cancellation.t -> Agent_loop.Config.t
  }

let engine = Engine_id.Harness
let capabilities (_ : t) = Capabilities.harness
let models (_ : t) = None
let context (_ : t) = None, None

let mask_session session ~through =
  match List.nth (Session.context_ids session) through with
  | Some (Some id) ->
    ignore (Session.append_masked session ~through:id : Session.Entry.t);
    Ok (Session.messages session)
  | Some None | None ->
    Or_error.error_s [%message "no entry to mask up to" (through : int)]
;;

let run_turn
      t
      (services : Engine_intf.Services.t)
      ~cancel
      (input : Engine_intf.Turn.Input.t)
  =
  let config = t.config ~cancel in
  let session = services.session in
  Agent_loop.run
    ~env:services.env
    ~provider:t.provider
    ~config
    ~cwd:services.cwd
    ~cancel
    ~approve:services.approve
    ~ask_user:services.ask_user
    ~ui:services.ui
    ~chat:input.chat
    ~execute:services.execute
    ~background:services.background
    ~steer:services.steer
    ~compact:(fun ~model ~keep_recent_tokens ~overflowed ~cancel _messages ->
      match
        Compaction.compact
          ~env:services.env
          ~provider:t.provider
          ~model
          ~prompt:(Agent_loop.Config.compaction_prompt config)
          ~overflowed
          ~keep_recent_tokens
          ~cancel
          session
      with
      | Ok outcome -> Ok (Session.messages session, outcome)
      | Error e ->
        services.notice ("auto-compaction failed: " ^ Error.to_string_hum e);
        Error e)
    ~mask:(fun ~through _messages -> mask_session session ~through)
    ~halt:services.halt
    ~emit:services.emit
    ~context:input.context
    ~prompts:input.prompts
    ()
;;

let create ~provider ~config =
  Runner.create
    (module struct
      type nonrec t = t

      let engine = engine
      let capabilities = capabilities
      let models = models
      let context = context
      let run_turn = run_turn
    end)
    { provider; config }
;;
