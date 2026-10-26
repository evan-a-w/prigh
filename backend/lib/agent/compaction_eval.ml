open! Core
open! Import

module Strategy = struct
  type t =
    | None_
    | Mask
    | Summarise
    | Mask_and_summarise
  [@@deriving equal, enumerate]

  let to_string = function
    | None_ -> "none"
    | Mask -> "mask"
    | Summarise -> "summarise"
    | Mask_and_summarise -> "mask+summarise"
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)
  let jsonaf_of_t t : Json.t = `String (to_string t)

  let of_string s =
    match String.lowercase (String.strip s) with
    | "none" | "raw" -> Ok None_
    | "mask" -> Ok Mask
    | "summarise" | "summarize" -> Ok Summarise
    | "mask+summarise" | "mask+summarize" | "both" -> Ok Mask_and_summarise
    | _ ->
      Or_error.error_string
        (sprintf
           "unknown strategy %S: one of %s"
           s
           (String.concat ~sep:", " (List.map all ~f:to_string)))
  ;;

  let masks = function
    | Mask | Mask_and_summarise -> true
    | None_ | Summarise -> false
  ;;

  let summarises = function
    | Summarise | Mask_and_summarise -> true
    | None_ | Mask -> false
  ;;
end

module Config = struct
  type t =
    { window : int option
    ; threshold_tokens : int option
    ; keep_recent_tokens : int
    ; keep_turns : int
    ; mask_min_bytes : int
    ; mask_at : float
    ; mask_min_saving : float
    ; summary_tokens : int
    ; overhead_tokens : int
    ; strategies : Strategy.t list
    }
  [@@deriving sexp_of]

  let default =
    { window = None
    ; threshold_tokens = None
    ; keep_recent_tokens = Compaction.default_keep_recent_tokens
    ; keep_turns = Observation_mask.keep_turns
    ; mask_min_bytes = Observation_mask.min_bytes
    ; mask_at = 0.5
    ; mask_min_saving = 0.1
    ; summary_tokens = 2000
    ; overhead_tokens = 0
    ; strategies = Strategy.all
    }
  ;;
end

module Fraction = struct
  type t =
    { hit : int
    ; total : int
    }
  [@@deriving sexp_of, jsonaf_of]

  let zero = { hit = 0; total = 0 }
  let add a b = { hit = a.hit + b.hit; total = a.total + b.total }
  let count items ~f = { hit = List.count items ~f; total = List.length items }
  let to_string t = if t.total = 0 then "-" else sprintf "%d/%d" t.hit t.total
end

module Live = struct
  type t =
    { summaries : int
    ; failures : int
    ; usage : Usage.t
    ; cost_usd : float
    ; probed : bool
    ; files_modified : Fraction.t
    ; constraints : Fraction.t
    ; last_error : Fraction.t
    ; answer : string
    }
  [@@deriving sexp_of, jsonaf_of]

  module Setup = struct
    type t =
      { provider : Provider.t
      ; model : Model.t
      ; cancel : Cancellation.t
      }
  end

  let add a b =
    { summaries = a.summaries + b.summaries
    ; failures = a.failures + b.failures
    ; usage = Usage.add a.usage b.usage
    ; cost_usd = a.cost_usd +. b.cost_usd
    ; probed = a.probed || b.probed
    ; files_modified = Fraction.add a.files_modified b.files_modified
    ; constraints = Fraction.add a.constraints b.constraints
    ; last_error = Fraction.add a.last_error b.last_error
    ; answer = ""
    }
  ;;
end

module Result = struct
  type t =
    { strategy : Strategy.t
    ; requests : int
    ; peak_tokens : int
    ; over_window : int
    ; input_tokens : int
    ; uncached_tokens : int
    ; cached_cost_tokens : int
    ; cache_invalidations : int
    ; mask_batches : int
    ; compactions : int
    ; final_tokens : int
    ; users_verbatim : int
    ; users_shortened : int
    ; users_total : int
    ; files_read : Fraction.t
    ; files_modified : Fraction.t
    ; errors : Fraction.t
    ; live : Live.t option
    }
  [@@deriving sexp_of, jsonaf_of]

  let add a b =
    { strategy = a.strategy
    ; requests = a.requests + b.requests
    ; peak_tokens = Int.max a.peak_tokens b.peak_tokens
    ; over_window = a.over_window + b.over_window
    ; input_tokens = a.input_tokens + b.input_tokens
    ; uncached_tokens = a.uncached_tokens + b.uncached_tokens
    ; cached_cost_tokens = a.cached_cost_tokens + b.cached_cost_tokens
    ; cache_invalidations = a.cache_invalidations + b.cache_invalidations
    ; mask_batches = a.mask_batches + b.mask_batches
    ; compactions = a.compactions + b.compactions
    ; final_tokens = a.final_tokens + b.final_tokens
    ; users_verbatim = a.users_verbatim + b.users_verbatim
    ; users_shortened = a.users_shortened + b.users_shortened
    ; users_total = a.users_total + b.users_total
    ; files_read = Fraction.add a.files_read b.files_read
    ; files_modified = Fraction.add a.files_modified b.files_modified
    ; errors = Fraction.add a.errors b.errors
    ; live =
        (match a.live, b.live with
         | Some a, Some b -> Some (Live.add a b)
         | (Some _ as l), None | None, l -> l)
    }
  ;;
end

module Session_report = struct
  type t =
    { id : string
    ; title : string
    ; model : string
    ; window : int
    ; threshold_tokens : int
    ; messages : int
    ; recorded_input_tokens : int
    ; results : Result.t list
    }
  [@@deriving sexp_of, jsonaf_of]
end

(* A message of the simulated context: as it is sent (masked, shortened) and
   as it was recorded, for the files list. *)
module Item = struct
  type t =
    { sent : Message.t
    ; original : Message.t
    ; tokens : int
    }

  let of_message m =
    { sent = m; original = m; tokens = Compaction.estimate_tokens [ m ] }
  ;;

  let tokens items = List.sum (module Int) items ~f:(fun t -> t.tokens)
end

(* What the agent knows of the session, the ground truth for the probes. *)
module Truth = struct
  type t =
    { users : Message.t list
    ; files : Compaction_files.t
    ; errors : string list (** first lines *)
    ; constraints : string list
    }

  let first_line text =
    List.find (String.split_lines text) ~f:(fun l ->
      not (String.is_empty (String.strip l)))
    |> Option.map ~f:(fun l -> String.prefix (String.strip l) 200)
  ;;

  let constraint_markers =
    [ "don't"
    ; "do not"
    ; "never"
    ; "must"
    ; "always"
    ; "only"
    ; "make sure"
    ; "should"
    ; "avoid"
    ; "without"
    ]
  ;;

  (* The sentences of the user's messages that look like instructions, the
     newest few. *)
  let constraints users =
    List.concat_map users ~f:(function
      | Message.User u ->
        String.split_lines (Message.User.body u)
        |> List.concat_map ~f:(String.split ~on:'.')
        |> List.map ~f:String.strip
        |> List.filter ~f:(fun s ->
          let s = String.lowercase s in
          String.length s >= 12
          && List.exists constraint_markers ~f:(fun substring ->
            String.is_substring s ~substring))
      | Assistant _ | Tool_result _ -> [])
    |> List.rev
    |> Fn.flip List.take 20
    |> List.rev
  ;;

  let of_messages messages =
    let users = List.filter messages ~f:Compaction.is_users_own in
    { users
    ; files = Compaction_files.add_calls Compaction_files.empty messages
    ; errors =
        List.filter_map messages ~f:(function
          | Message.Tool_result r when r.is_error -> first_line r.text
          | User _ | Assistant _ | Tool_result _ -> None)
    ; constraints = constraints users
    }
  ;;
end

let text_of (m : Message.t) =
  match m with
  | User u -> [ u.text ]
  | Tool_result r -> [ r.text ]
  | Assistant a ->
    List.concat_map a.content ~f:(function
      | Text s -> [ s ]
      | Thinking th -> [ th.text ]
      | Tool_call c -> [ c.name; c.arguments ]
      | Server_tool s -> s.input :: Option.to_list s.output)
;;

(* Lowercase words of four letters or more. *)
let words s =
  String.split_on_chars
    (String.lowercase s)
    ~on:(String.to_list " \t\n\r.,;:!?()[]{}<>\"'`=/\\|*#-_@$%^&+~")
  |> List.filter ~f:(fun w -> String.length w >= 4)
  |> String.Set.of_list
;;

(* Most of [probe]'s words are among [answer]'s. *)
let recalls ~answer probe =
  let probe = words probe in
  (not (Set.is_empty probe))
  && 2 * Set.length (Set.inter probe answer) >= Set.length probe
;;

module Live_run = struct
  type t =
    { setup : Live.Setup.t
    ; system : string option
    ; mutable summaries : int
    ; mutable failures : int
    ; mutable usage : Usage.t
    }

  let questions =
    "[prigh: compaction-eval] Answer from the conversation so far alone, \
     without calling tools:\n\
     1. Which files were modified (created, edited, patched)? List their paths.\n\
     2. What constraints, requirements and preferences did the user state? \
     Quote them.\n\
     3. What was the last error, exactly, and what was tried about it?"
  ;;

  let probe t ~(truth : Truth.t) context =
    let request =
      { Provider.Request.model = t.setup.model
      ; system = t.system
      ; messages = context @ [ Message.user questions ]
      ; tools = []
      ; thinking = Off
      ; max_tokens = Some (Int.min t.setup.model.max_output 4096)
      ; cache_key = None
      }
    in
    let fits =
      (Option.value_map t.system ~default:0 ~f:String.length / 4)
      + Compaction.estimate_tokens request.messages
      + 4096
      <= t.setup.model.context_window
    in
    let answer =
      if fits
      then (
        let reply =
          t.setup.provider.stream
            (Provider.Request.prepare request)
            ~cancel:t.setup.cancel
            ~on_event:ignore
        in
        t.usage <- Usage.add t.usage reply.usage;
        match reply.stop_reason with
        | Error e -> Some ("error: " ^ e)
        | Aborted -> Some "aborted"
        | End_turn | Tool_use | Length -> Some (Message.Assistant.text reply))
      else None
    in
    let text = Option.value answer ~default:"" in
    let answer_words = words text in
    let lower = String.lowercase text in
    let grade = Option.is_some answer in
    let fraction items ~f =
      if grade then Fraction.count items ~f else Fraction.zero
    in
    { Live.summaries = t.summaries
    ; failures = t.failures
    ; usage = t.usage
    ; cost_usd = Model.cost_usd t.setup.model t.usage
    ; probed = grade
    ; files_modified =
        fraction truth.files.modified ~f:(fun path ->
          String.is_substring
            lower
            ~substring:(String.lowercase (Filename.basename path)))
    ; constraints = fraction truth.constraints ~f:(recalls ~answer:answer_words)
    ; last_error =
        fraction
          (Option.to_list (List.last truth.errors))
          ~f:(recalls ~answer:answer_words)
    ; answer = text
    }
  ;;
end

module Sim = struct
  type t =
    { config : Config.t
    ; model : Model.t (** with the evaluated window *)
    ; threshold : int
    ; overhead : int
    ; strategy : Strategy.t
    ; live : Live_run.t option
    ; mutable context : Item.t list
    ; mutable last_request : Item.t list option
    ; mutable requests : int
    ; mutable peak_tokens : int
    ; mutable over_window : int
    ; mutable input_tokens : int
    ; mutable uncached_tokens : int
    ; mutable cache_invalidations : int
    ; mutable mask_batches : int
    ; mutable compactions : int
    }

  let tokens t items = t.overhead + Item.tokens items

  (* The items [items] starts with that the last request also sent, in the
     same place, and whether the last request was all of them. *)
  let shared_prefix ~last items =
    let rec go acc last items =
      match last, items with
      | [], _ -> List.rev acc, true
      | (l : Item.t) :: last, (i : Item.t) :: items
        when phys_equal l.sent i.sent -> go (i :: acc) last items
      | _ :: _, _ -> List.rev acc, false
    in
    go [] last items
  ;;

  let send t items =
    let total = tokens t items in
    let cached =
      match t.last_request with
      | None -> 0
      | Some last ->
        let shared, extends = shared_prefix ~last items in
        if not extends then t.cache_invalidations <- t.cache_invalidations + 1;
        tokens t shared
    in
    t.requests <- t.requests + 1;
    t.peak_tokens <- Int.max t.peak_tokens total;
    if total > t.model.context_window then t.over_window <- t.over_window + 1;
    t.input_tokens <- t.input_tokens + total;
    t.uncached_tokens <- t.uncached_tokens + total - cached;
    t.last_request <- Some items
  ;;

  let sent t = List.map t.context ~f:(fun (i : Item.t) -> i.sent)

  let mask t =
    let tokens = tokens t t.context in
    match
      Observation_mask.plan
        ~keep_turns:t.config.keep_turns
        ~min_bytes:t.config.mask_min_bytes
        (sent t)
    with
    | Some plan
      when Observation_mask.worth_masking
             ~at:t.config.mask_at
             ~min_saving:t.config.mask_min_saving
             t.model
             ~input_tokens:tokens
             ~urgent:(tokens > t.threshold)
             plan ->
      t.mask_batches <- t.mask_batches + 1;
      t.context
      <- List.mapi t.context ~f:(fun i (item : Item.t) ->
           if i > plan.through
           then item
           else (
             let sent =
               Observation_mask.mask
                 ~min_bytes:t.config.mask_min_bytes
                 item.sent
             in
             if phys_equal sent item.sent
             then item
             else
               { item with sent; tokens = Compaction.estimate_tokens [ sent ] }))
    | Some _ | None -> ()
  ;;

  let ask = Item.of_message (Message.user "[compaction request]")

  let stub_summary t older =
    let files =
      List.find_map (List.rev older) ~f:(fun (i : Item.t) ->
        Session.summary_of_message i.sent)
      |> Option.value_map ~default:Compaction_files.empty ~f:(fun s ->
        snd (Compaction_files.of_summary s))
    in
    let files =
      Compaction_files.add_calls
        files
        (List.map older ~f:(fun (i : Item.t) -> i.original))
    in
    let placeholder = "[stub summary] " in
    Compaction_files.append
      (placeholder
       ^ String.make
           (Int.max
              0
              ((t.config.summary_tokens * 4) - String.length placeholder))
           '.')
      files
  ;;

  let summarise t =
    if tokens t t.context > t.threshold
    then (
      let messages = sent t in
      match
        Compaction.plan ~keep_recent_tokens:t.config.keep_recent_tokens messages
      with
      | None -> ()
      | Some { kept_from; pinned } ->
        let older = List.take t.context kept_from in
        let kept = List.drop t.context kept_from in
        send t (older @ [ ask ]);
        let replaced =
          match t.live with
          | None ->
            let items = Array.of_list t.context in
            let pinned =
              List.map pinned ~f:(fun { index; shortened } ->
                let item = items.(index) in
                if shortened
                then (
                  let sent = Pinned_messages.shorten item.sent in
                  if phys_equal sent item.sent
                  then item
                  else
                    { item with
                      sent
                    ; tokens = Compaction.estimate_tokens [ sent ]
                    })
                else item)
            in
            Some
              (pinned
               @ [ Item.of_message
                     (Session.summary_message (stub_summary t older))
                 ])
          | Some live ->
            (match
               Compaction.compact_messages
                 ~provider:live.setup.provider
                 ~model:live.setup.model
                 ~prompt:{ Compaction.Prompt.none with system = live.system }
                 ~keep_recent_tokens:t.config.keep_recent_tokens
                 ~cancel:live.setup.cancel
                 ~unmasked:
                   (List.map t.context ~f:(fun (i : Item.t) -> i.original))
                 messages
             with
             | Error _ ->
               live.failures <- live.failures + 1;
               None
             | Ok (messages, outcome) ->
               live.summaries <- live.summaries + 1;
               live.usage <- Usage.add live.usage outcome.usage;
               (* Pinned messages keep their place in the cache when unchanged. *)
               let items = Array.of_list t.context in
               let head =
                 List.take messages (List.length messages - List.length kept)
               in
               Some
                 (List.map head ~f:(fun m ->
                    match
                      Array.find items ~f:(fun (i : Item.t) ->
                        Message.equal i.sent m)
                    with
                    | Some i -> i
                    | None -> Item.of_message m)))
        in
        Option.iter replaced ~f:(fun head ->
          t.compactions <- t.compactions + 1;
          t.context <- head @ kept))
  ;;

  let before_request t =
    if Option.is_some t.last_request
    then (
      if Strategy.masks t.strategy then mask t;
      if Strategy.summarises t.strategy then summarise t)
  ;;

  let run t messages =
    List.iter messages ~f:(fun (m : Message.t) ->
      (match m with
       | Assistant _ ->
         before_request t;
         send t t.context
       | User _ | Tool_result _ -> ());
      t.context <- t.context @ [ Item.of_message m ])
  ;;

  let result t ~(truth : Truth.t) =
    let sent = sent t in
    let blob = String.concat ~sep:"\n" (List.concat_map sent ~f:text_of) in
    let mentioned s = String.is_substring blob ~substring:s in
    let user_texts =
      List.filter_map sent ~f:(function
        | Message.User u -> Some u.text
        | Assistant _ | Tool_result _ -> None)
      |> String.Set.of_list
    in
    let text_of_user = function
      | Message.User u -> u.text
      | Assistant _ | Tool_result _ -> ""
    in
    let verbatim, rest =
      List.partition_tf truth.users ~f:(fun m ->
        Set.mem user_texts (text_of_user m))
    in
    let shortened =
      List.count rest ~f:(fun m ->
        Set.mem user_texts (text_of_user (Pinned_messages.shorten m)))
    in
    { Result.strategy = t.strategy
    ; requests = t.requests
    ; peak_tokens = t.peak_tokens
    ; over_window = t.over_window
    ; input_tokens = t.input_tokens
    ; uncached_tokens = t.uncached_tokens
    ; cached_cost_tokens =
        Float.iround_nearest_exn
          ((1.25 *. Float.of_int t.uncached_tokens)
           +. (0.1 *. Float.of_int (t.input_tokens - t.uncached_tokens)))
    ; cache_invalidations = t.cache_invalidations
    ; mask_batches = t.mask_batches
    ; compactions = t.compactions
    ; final_tokens = tokens t t.context
    ; users_verbatim = List.length verbatim
    ; users_shortened = shortened
    ; users_total = List.length truth.users
    ; files_read = Fraction.count truth.files.read ~f:mentioned
    ; files_modified = Fraction.count truth.files.modified ~f:mentioned
    ; errors = Fraction.count truth.errors ~f:mentioned
    ; live = Option.map t.live ~f:(fun live -> Live_run.probe live ~truth sent)
    }
  ;;
end

let recorded_messages session =
  List.filter_map (Session.active_path session) ~f:(fun (e : Session.Entry.t) ->
    match e.payload with
    | Message m -> Some m
    | _ -> None)
;;

let session ?live ?model:given (config : Config.t) session =
  let model_key = Option.map (Session.model session) ~f:fst in
  let model =
    match given with
    | Some m -> m
    | None ->
      Option.bind model_key ~f:Model.find |> Option.value ~default:Model.default
  in
  let window = Option.value config.window ~default:model.context_window in
  let model = { model with context_window = window } in
  let threshold =
    Option.value
      config.threshold_tokens
      ~default:(Float.iround_down_exn (Float.of_int window *. 0.8))
  in
  let messages = recorded_messages session in
  let system = Session.system_prompt session in
  let overhead =
    config.overhead_tokens
    + (Option.value_map system ~default:0 ~f:String.length / 4)
  in
  let truth = Truth.of_messages messages in
  let results =
    List.map config.strategies ~f:(fun strategy ->
      let sim =
        { Sim.config
        ; model
        ; threshold
        ; overhead
        ; strategy
        ; live =
            Option.map live ~f:(fun setup ->
              { Live_run.setup
              ; system
              ; summaries = 0
              ; failures = 0
              ; usage = Usage.zero
              })
        ; context = []
        ; last_request = None
        ; requests = 0
        ; peak_tokens = 0
        ; over_window = 0
        ; input_tokens = 0
        ; uncached_tokens = 0
        ; cache_invalidations = 0
        ; mask_batches = 0
        ; compactions = 0
        }
      in
      Sim.run sim messages;
      Sim.result sim ~truth)
  in
  let title =
    List.find_map truth.users ~f:(function
      | Message.User u ->
        Some
          (String.prefix
             (String.tr
                ~target:'\n'
                ~replacement:' '
                (String.strip (Message.User.body u)))
             60)
      | Assistant _ | Tool_result _ -> None)
    |> Option.value ~default:""
  in
  { Session_report.id = Session.id session
  ; title
  ; model =
      (match given, model_key with
       | Some m, _ -> Model.key m
       | None, Some key -> key
       | None, None -> Model.key model ^ " (default)")
  ; window
  ; threshold_tokens = threshold
  ; messages = List.length messages
  ; recorded_input_tokens =
      List.sum
        (module Int)
        messages
        ~f:(function
          | Message.Assistant a -> a.usage.input + a.usage.cache_read
          | User _ | Tool_result _ -> 0)
  ; results
  }
;;

module Report = struct
  type t =
    { sessions : Session_report.t list
    ; aggregate : Result.t list
    }
  [@@deriving sexp_of, jsonaf_of]

  let tokens n =
    if n >= 1_000_000
    then sprintf "%.2fM" (Float.of_int n /. 1e6)
    else if n >= 10_000
    then sprintf "%.1fk" (Float.of_int n /. 1e3)
    else Int.to_string n
  ;;

  let header =
    sprintf
      "  %-15s %8s %4s %8s %8s %8s %5s %5s %5s %6s %11s %7s %7s %7s"
      "strategy"
      "peak"
      "over"
      "input"
      "uncached"
      "cached$"
      "inval"
      "masks"
      "summ"
      "final"
      "user v/s/n"
      "read"
      "written"
      "errors"
  ;;

  let row (r : Result.t) =
    let main =
      sprintf
        "  %-15s %8s %4d %8s %8s %8s %5d %5d %5d %6s %11s %7s %7s %7s"
        (Strategy.to_string r.strategy)
        (tokens r.peak_tokens)
        r.over_window
        (tokens r.input_tokens)
        (tokens r.uncached_tokens)
        (tokens r.cached_cost_tokens)
        r.cache_invalidations
        r.mask_batches
        r.compactions
        (tokens r.final_tokens)
        (sprintf "%d/%d/%d" r.users_verbatim r.users_shortened r.users_total)
        (Fraction.to_string r.files_read)
        (Fraction.to_string r.files_modified)
        (Fraction.to_string r.errors)
    in
    match r.live with
    | None -> [ main ]
    | Some l ->
      [ main
      ; sprintf
          "  %-15s live: %d summaries (%d failed), $%.4f; %s written files, %s \
           constraints, %s last error"
          ""
          l.summaries
          l.failures
          l.cost_usd
          (if l.probed then Fraction.to_string l.files_modified else "n/a")
          (if l.probed then Fraction.to_string l.constraints else "n/a")
          (if l.probed then Fraction.to_string l.last_error else "n/a")
      ]
  ;;

  let table results = header :: List.concat_map results ~f:row

  let to_table t =
    let sessions =
      List.concat_map t.sessions ~f:(fun (s : Session_report.t) ->
        (sprintf
           "session %s %S\n\
           \  %s, window %s, summarise over %s: %d messages, recorded input %s"
           s.id
           s.title
           s.model
           (tokens s.window)
           (tokens s.threshold_tokens)
           s.messages
           (tokens s.recorded_input_tokens)
         :: table s.results)
        @ [ "" ])
    in
    String.concat
      ~sep:"\n"
      (sessions
       @ (sprintf "aggregate over %d sessions" (List.length t.sessions)
          :: table t.aggregate))
    ^ "\n"
  ;;
end

let report sessions =
  let aggregate =
    match sessions with
    | [] -> []
    | (first : Session_report.t) :: rest ->
      List.fold rest ~init:first.results ~f:(fun acc (s : Session_report.t) ->
        List.map acc ~f:(fun (a : Result.t) ->
          match
            List.find s.results ~f:(fun (r : Result.t) ->
              Strategy.equal r.strategy a.strategy)
          with
          | Some r -> Result.add a r
          | None -> a))
  in
  { Report.sessions; aggregate }
;;
