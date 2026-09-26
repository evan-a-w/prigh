open! Core

type t = Jsonaf.t

let rec sexp_of_t : t -> Sexp.t = function
  | `Null -> Atom "null"
  | `True -> Atom "true"
  | `False -> Atom "false"
  | `String s -> Atom s
  | `Number n -> Atom n
  | `Array items -> List (List.map items ~f:sexp_of_t)
  | `Object fields ->
    List (List.map fields ~f:(fun (k, v) -> Sexp.List [ Atom k; sexp_of_t v ]))
;;

let equal a b = String.equal (Jsonaf.to_string a) (Jsonaf.to_string b)
let str s = `String s
let int i = `Number (Int.to_string i)
let float f = `Number (sprintf "%.15g" f)
let bool b = if b then `True else `False
let obj fields = `Object fields
let to_string = Jsonaf.to_string
let parse = Jsonaf.parse

let field t name =
  match t with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal name with
     | Some `Null | None -> None
     | Some v -> Some v)
  | _ -> None
;;

let missing name = Or_error.errorf "missing field %S" name

let to_string_or_error = function
  | `String s -> Ok s
  | other -> Or_error.errorf "expected string, got %s" (Jsonaf.to_string other)
;;

let string_field t name =
  match field t name with
  | Some (`String s) -> Ok s
  | Some other ->
    Or_error.errorf "field %S: expected string, got %s" name (to_string other)
  | None -> missing name
;;

let string_opt_field t name =
  match field t name with
  | None -> Ok None
  | Some (`String s) -> Ok (Some s)
  | Some other ->
    Or_error.errorf "field %S: expected string, got %s" name (to_string other)
;;

let number_field t name =
  match field t name with
  | Some (`Number n) -> Ok n
  | Some other ->
    Or_error.errorf "field %S: expected number, got %s" name (to_string other)
  | None -> missing name
;;

let int_field t name =
  Or_error.bind (number_field t name) ~f:(fun n ->
    match Int.of_string n with
    | i -> Ok i
    | exception _ ->
      (match Float.of_string n with
       | exception _ -> Or_error.errorf "field %S: bad integer %S" name n
       | f ->
         (match Float.to_int f with
          | i -> Ok i
          | exception _ ->
            Or_error.errorf "field %S: integer %S out of range" name n)))
;;

let int64_field t name =
  Or_error.bind (number_field t name) ~f:(fun n ->
    match Int64.of_string n with
    | i -> Ok i
    | exception _ -> Or_error.errorf "field %S: bad integer %S" name n)
;;

let float_field t name =
  Or_error.bind (number_field t name) ~f:(fun n ->
    match Float.of_string n with
    | f -> Ok f
    | exception _ -> Or_error.errorf "field %S: bad number %S" name n)
;;

let bool_field t name =
  match field t name with
  | Some `True -> Ok true
  | Some `False -> Ok false
  | Some other ->
    Or_error.errorf "field %S: expected bool, got %s" name (to_string other)
  | None -> missing name
;;

let list_field t name ~f =
  match field t name with
  | Some (`Array items) ->
    List.mapi items ~f:(fun i item ->
      Or_error.tag_arg (f item) "in" (sprintf "%s[%d]" name i) String.sexp_of_t)
    |> Or_error.all
  | Some other ->
    Or_error.errorf "field %S: expected array, got %s" name (to_string other)
  | None -> missing name
;;

let object_field t name =
  match field t name with
  | Some (`Object _ as o) -> Ok o
  | Some other ->
    Or_error.errorf "field %S: expected object, got %s" name (to_string other)
  | None -> missing name
;;
