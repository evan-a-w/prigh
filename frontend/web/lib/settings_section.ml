open! Core

type t =
  | Appearance
  | Behaviour
  | Models
  | Git_hosting
  | Nix_store
  | Account
[@@deriving sexp_of, equal, enumerate]

let available ~superuser =
  List.filter all ~f:(function
    | Nix_store -> superuser
    | Appearance | Behaviour | Models | Git_hosting | Account -> true)
;;

let visible t ~superuser =
  if List.mem (available ~superuser) t ~equal then t else Appearance
;;

let name = function
  | Appearance -> "Appearance"
  | Behaviour -> "Behaviour"
  | Models -> "Models"
  | Git_hosting -> "Git hosting"
  | Nix_store -> "Nix store"
  | Account -> "Account"
;;

let about = function
  | Appearance ->
    "How this page looks. The theme, accent and tabs follow you to every \
     browser you sign in on; the transcript's detail is this browser's."
  | Behaviour ->
    "What the agent asks before it acts, and how this browser tells you that \
     something happened."
  | Models ->
    "The providers whose models you use, and how long conversations are \
     summarised. Kept on the backend."
  | Git_hosting ->
    "The hosts agents push branches to and open pull requests on."
  | Nix_store ->
    "The packages agents fetch with Nix, on the shared /nix volume."
  | Account -> "Who you are on this backend."
;;

let arg = function
  | Appearance -> "appearance"
  | Behaviour -> "behaviour"
  | Models -> "models"
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
