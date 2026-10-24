open! Core

(* The wordings pi and opencode recognise. *)
let patterns =
  List.map
    ~f:(fun re -> Re.compile (Re.no_case (Re.Perl.re re)))
    [ {|prompt (is )?too long|}
    ; {|prompt exceeds max length|}
    ; {|request_too_large|}
    ; {|input is too long for requested model|}
    ; {|exceeds the context window|}
    ; {|exceeds (the )?(model'?s )?maximum context length|}
    ; {|input token count.*exceeds the maximum|}
    ; {|maximum prompt length is \d+|}
    ; {|reduce the length of the messages|}
    ; {|maximum context length is \d+ tokens|}
    ; {|exceeds (the )?maximum allowed input length|}
    ; {|is longer than the model'?s context length|}
    ; {|exceeds the limit of \d+|}
    ; {|exceeds the available context size|}
    ; {|greater than the context length|}
    ; {|context window exceeds limit|}
    ; {|exceeded model token limit|}
    ; {|too large for model with \d+ maximum context length|}
    ; {|but the configured context size is|}
    ; {|model_context_window_exceeded|}
    ; {|range of input length should be|}
    ; {|context[_ ]length[_ ]exceeded|}
    ; {|too many tokens|}
    ; {|token limit exceeded|}
    ]
;;

let exclusions =
  List.map
    ~f:(fun re -> Re.compile (Re.no_case (Re.Perl.re re)))
    [ {|^(throttling error|service unavailable):|}
    ; {|rate limit|}
    ; {|too many requests|}
    ]
;;

let matches message =
  (not (List.exists exclusions ~f:(fun re -> Re.execp re message)))
  && (not (Usage_limit.unavailable message))
  && List.exists patterns ~f:(fun re -> Re.execp re message)
;;
