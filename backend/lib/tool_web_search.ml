open! Core
open! Import

let name = "web_search"

let spec =
  { Tool_spec.name
  ; parallel_safe = true
  ; on_host = false
  ; freeform = None
  ; destructive = false
  ; description =
      "Search the web for current information (documentation, error messages, \
       releases). Returns result titles, URLs and excerpts; use web_fetch to \
       read a page in full."
  ; parameters =
      Tool_args.schema ~required:[ "query" ] [ "query", `String, "The query" ]
  }
;;

(* Providers run it; a call can only reach us from a model that was not
   offered the native tool. *)
let run _context _args =
  Tool.Result.error
    "web_search runs on the model provider's servers and is not available with \
     this model; use web_fetch with a search engine's URL instead"
;;

let tool = { Tool.spec; run }

let supported (model : Model.t) =
  match model.provider with
  | Anthropic | Openai | Openai_codex -> true
  | Deepseek | Custom _ -> false
;;

let is_spec (spec : Tool_spec.t) = String.equal spec.name name

let results_text results =
  match results with
  | [] -> "no results"
  | results ->
    List.mapi results ~f:(fun i (title, url, age) ->
      sprintf
        "%d. %s — %s%s"
        (i + 1)
        (if String.is_empty (String.strip title)
         then url
         else String.strip title)
        url
        (match age with
         | Some age when not (String.is_empty age) -> sprintf " (%s)" age
         | Some _ | None -> ""))
    |> String.concat ~sep:"\n"
;;
