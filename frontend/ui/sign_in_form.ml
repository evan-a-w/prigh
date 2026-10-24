open! Core

module Field = struct
  type t =
    | User
    | Token
  [@@deriving sexp_of, equal]
end

type t =
  { address : string
  ; user : string
  ; token : (string[@sexp.opaque])
  ; focus : Field.t
  ; error : string option
  ; submitting : bool
  }
[@@deriving sexp_of, equal]

let create ~address ~user ~error =
  { address
  ; user = Option.value user ~default:""
  ; token = ""
  ; focus = (if Option.is_some user then Token else User)
  ; error
  ; submitting = false
  }
;;

let address t = t.address
let user t = t.user
let focus t = t.focus
let error t = t.error
let submitting t = t.submitting
let masked_token t = String.make (List.length (Text_width.uchars t.token)) '*'

let edit t ~f =
  if t.submitting
  then t
  else (
    match t.focus with
    | User -> { t with user = f t.user }
    | Token -> { t with token = f t.token })
;;

(* A pasted token often ends with a newline. *)
let insert t text =
  let text = String.filter text ~f:(fun c -> not (Char.equal c '\n')) in
  edit t ~f:(fun field -> field ^ text)
;;

let backspace t =
  edit t ~f:(fun field ->
    let chars = Text_width.uchars field in
    String.concat
      (List.map (List.drop_last chars |> Option.value ~default:[]) ~f:fst))
;;

let clear t = edit t ~f:(fun _ -> "")

let toggle t =
  { t with
    focus =
      (match t.focus with
       | User -> Token
       | Token -> User)
  }
;;

let submit t =
  if t.submitting
  then `Wait t
  else (
    match t.focus, String.is_empty t.token with
    | User, true -> `Wait { t with focus = Token }
    | Token, true -> `Wait { t with error = Some "type the token or password" }
    | (User | Token), false ->
      let user = String.strip t.user in
      `Sign_in
        ( { t with submitting = true; error = None }
        , Server_link.Change.Sign_in
            { user = Option.some_if (not (String.is_empty user)) user
            ; token = t.token
            } ))
;;

let failed t ~error =
  { t with error = Some error; submitting = false; token = ""; focus = Token }
;;
