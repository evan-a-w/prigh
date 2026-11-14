open! Core

type t =
  | Appearance
  | Sessions
  | New_sessions
  | Models
  | Memory
  | About_you
  | Custom_instructions
  | Mcp
  | Git_hosting
  | Nix_store
  | Account
[@@deriving sexp_of, equal, enumerate]

let available ~superuser =
  List.filter all ~f:(function
    | Nix_store -> superuser
    | Appearance
    | Sessions
    | New_sessions
    | Models
    | Memory
    | About_you
    | Custom_instructions
    | Mcp
    | Git_hosting
    | Account -> true)
;;

let visible t ~superuser =
  if List.mem (available ~superuser) t ~equal then t else Appearance
;;

let name = function
  | Appearance -> "Appearance"
  | Sessions -> "Sessions"
  | New_sessions -> "New sessions"
  | Models -> "Models"
  | Memory -> "Memory"
  | About_you -> "About you"
  | Custom_instructions -> "Custom instructions"
  | Mcp -> "MCP servers"
  | Git_hosting -> "Git hosting"
  | Nix_store -> "Nix store"
  | Account -> "Account"
;;

let about = function
  | Appearance ->
    "How this page looks. Saved for your account, so it follows you to every \
     browser you sign in on."
  | Sessions ->
    "How the session list shows sessions, and how this browser tells you that \
     something happened. Kept on the backend."
  | New_sessions ->
    "What a new session starts with. A change applies from the next new \
     session: open ones keep theirs. Kept on the backend."
  | Models ->
    "The providers whose models you use, the models prigh uses for its own \
     work, and when long conversations are summarised. Kept on the backend."
  | Memory ->
    "What you asked the model to remember, for later sessions. Kept on the \
     backend, for your account."
  | About_you ->
    "What the model should know about you, given to it in every session. Kept \
     on the backend, for your account."
  | Custom_instructions ->
    "Standing instructions for the model, given to it in every session; it may \
     also add or edit them when you state a lasting preference. Kept on the \
     backend, for your account."
  | Mcp ->
    "Tools the model can use from MCP servers. The servers here are kept on \
     the backend for your account and run on every host your sessions use; a \
     host's ~/.prigh/mcp.json and a project's .mcp.json add their own."
  | Git_hosting ->
    "The hosts agents push branches to and open pull requests on."
  | Nix_store ->
    "The packages agents fetch with Nix, on the shared /nix volume."
  | Account -> "Who you are on this backend."
;;

let arg = function
  | Appearance -> "appearance"
  | Sessions -> "sessions"
  | New_sessions -> "new-sessions"
  | Models -> "models"
  | Memory -> "memory"
  | About_you -> "user-info"
  | Custom_instructions -> "instructions"
  | Mcp -> "mcp"
  | Git_hosting -> "git"
  | Nix_store -> "nix"
  | Account -> "account"
;;

let of_arg s =
  let s = String.lowercase (String.strip s) in
  let matches t =
    List.exists
      [ arg t; String.lowercase (name t) ]
      ~f:(fun n -> String.is_prefix n ~prefix:s)
  in
  if String.is_empty s
  then None
  else (
    match List.find all ~f:(fun t -> String.equal (arg t) s) with
    | Some t -> Some t
    | None when String.is_prefix "behaviour" ~prefix:s -> Some Sessions
    | None when String.is_prefix "defaults" ~prefix:s -> Some New_sessions
    | None -> List.find all ~f:matches)
;;

let tab_id t = "settings-tab-" ^ arg t

module Move = struct
  type t =
    | Prev
    | Next
    | First
    | Last
  [@@deriving sexp_of, equal]
end

let move t (move : Move.t) ~superuser =
  let sections = available ~superuser in
  let n = List.length sections in
  let i =
    Option.value_map
      (List.findi sections ~f:(fun _ s -> equal s t))
      ~default:0
      ~f:fst
  in
  let j =
    match move with
    | Prev -> (i - 1 + n) % n
    | Next -> (i + 1) % n
    | First -> 0
    | Last -> n - 1
  in
  List.nth_exn sections j
;;
