open! Core
open! Import

module Program = struct
  type t =
    | Tui
    | Tool_host
    | Web
    | Desktop
    | Other
  [@@deriving sexp_of, equal]

  let of_string = function
    | "tui" -> Tui
    | "tool-host" -> Tool_host
    | "web" -> Web
    | "desktop" -> Desktop
    | _ -> Other
  ;;

  let to_string = function
    | Tui -> "tui"
    | Tool_host -> "tool-host"
    | Web -> "web"
    | Desktop -> "desktop"
    | Other -> "other"
  ;;

  let name = function
    | Tui -> "prigh-tui"
    | Tool_host -> "prigh tool-host"
    | Web -> "prigh web"
    | Desktop -> "prigh desktop"
    | Other -> "A program"
  ;;
end

module Request = struct
  type t =
    { port : int
    ; state : string
    ; program : Program.t
    }
  [@@deriving sexp_of, equal]

  let is_hex c = Char.is_digit c || Char.between (Char.lowercase c) ~low:'a' ~high:'f'

  let parse value ~program =
    match String.lsplit2 value ~on:'.' with
    | Some (port, state)
      when String.length port <= 5
           && String.for_all port ~f:Char.is_digit
           && (not (String.is_empty port))
           && String.length state >= 16
           && String.length state <= 128
           && String.for_all state ~f:is_hex ->
      let port = Int.of_string port in
      Option.some_if
        (port >= 1 && port <= 65535)
        { port; state; program = Program.of_string program }
    | _ -> None
  ;;

  let of_hash hash =
    let fields =
      String.chop_prefix_if_exists hash ~prefix:"#"
      |> String.split ~on:'&'
      |> List.filter_map ~f:(String.lsplit2 ~on:'=')
    in
    let field name = List.Assoc.find fields ~equal:String.equal name in
    Option.bind (field "cli-signin") ~f:(fun value ->
      parse value ~program:(Option.value (field "program") ~default:""))
  ;;

  let to_string t =
    sprintf "cli-signin=%d.%s&program=%s" t.port t.state (Program.to_string t.program)
  ;;

  let of_string = of_hash
end

let pending_key = "prigh.cli_signin"
let grant_path = "cli-signin/grant"

let grant_body ~user ~token =
  let string = Option.value_map ~default:`Null ~f:(fun s -> `String s) in
  Json.to_string (`Object [ "token", string token; "user", string user ])
;;

let cookie_of_grant text =
  match Json.parse text with
  | Ok json ->
    (match Json.field json "cookie" with
     | Some (`String cookie) when not (String.is_empty cookie) -> Some cookie
     | _ -> None)
  | Error _ -> None
;;

let callback_url (t : Request.t) = sprintf "http://127.0.0.1:%d/callback" t.port

module Answer = struct
  type t =
    | Allow of
        { cookie : string
        ; token : string option
        ; user : string option
        }
    | Cancel
end

let callback_fields (t : Request.t) (answer : Answer.t) =
  ("state", t.state)
  ::
  (match answer with
   | Cancel -> [ "cancel", "1" ]
   | Allow { cookie; token; user } ->
     [ "cookie", cookie
     ; "token", Option.value token ~default:""
     ; "user", Option.value user ~default:""
     ])
;;

let heading (t : Request.t) =
  match t.program with
  | Other -> "Allow a program to connect?"
  | program -> sprintf "Allow %s to connect?" (Program.name program)
;;

let explanation (t : Request.t) ~host ~user =
  [ sprintf
      "%s on this computer wants to connect to %s%s."
      (Program.name t.program)
      host
      (match user with
       | Some user -> " as " ^ user
       | None -> " with this page's sign-in")
  ; "It gets this browser's sign-in to the site and your prigh sign-in. \
     Allow it only if you just started it."
  ]
;;

let grant_error ~status ~body =
  let body = String.strip body in
  match status with
  | 401 ->
    ( (if String.is_empty body then "prigh did not accept this page's sign-in" else body)
      ^ ": sign in to prigh, then allow the program again."
    , `Sign_in )
  | 0 -> "The server could not be reached: check the connection and try again.", `Stop
  | 404 ->
    ( "This prigh server cannot hand sign-ins to programs (it is older than \
       the program): update it, or exempt /ws and /terminal from the sign-in \
       proxy."
    , `Stop )
  | _ ->
    ( (if String.is_empty body then sprintf "The server answered %d." status else body)
    , `Stop )
;;
