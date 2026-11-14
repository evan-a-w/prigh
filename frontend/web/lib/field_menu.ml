open! Core
open! Import

module Kind = struct
  type t =
    | Text_field
    | Terminal
  [@@deriving sexp_of, equal]
end

module Field = struct
  type t =
    { kind : Kind.t
    ; selection : bool
    ; editable : bool
    ; secret : bool
    }
  [@@deriving sexp_of, equal]
end

module Item = struct
  type t =
    | Cut
    | Copy
    | Paste
    | Select_all
  [@@deriving sexp_of, equal, enumerate]

  let label = function
    | Cut -> "Cut"
    | Copy -> "Copy"
    | Paste -> "Paste"
    | Select_all -> "Select all"
  ;;

  let key (kind : Kind.t) t =
    match kind, t with
    | Text_field, Cut -> Some "Ctrl+X"
    | Text_field, Copy -> Some "Ctrl+C"
    | Text_field, Paste -> Some "Ctrl+V"
    | Text_field, Select_all -> Some "Ctrl+A"
    | Terminal, Copy -> Some "Ctrl+Shift+C"
    | Terminal, Paste -> Some "Ctrl+Shift+V"
    | Terminal, (Cut | Select_all) -> None
  ;;
end

type t =
  { field : Field.t
  ; anchor : Context_menu.Anchor.t
  ; selected : Item.t
  }
[@@deriving sexp_of, equal]

let field t = t.field
let anchor t = t.anchor
let selected t = t.selected

let sections_of (field : Field.t) : Item.t list list =
  match field.kind with
  | Text_field -> [ [ Cut; Copy; Paste ]; [ Select_all ] ]
  | Terminal -> [ [ Copy; Paste ]; [ Select_all ] ]
;;

let sections t = sections_of t.field

let enabled_in (field : Field.t) (item : Item.t) =
  List.exists (sections_of field) ~f:(fun items ->
    List.mem items item ~equal:Item.equal)
  &&
  match item with
  | Cut -> field.selection && field.editable && not field.secret
  | Copy -> field.selection && not field.secret
  | Paste -> field.editable
  | Select_all -> true
;;

let enabled t item = enabled_in t.field item

let create field ~anchor =
  { field
  ; anchor
  ; selected =
      List.concat (sections_of field)
      |> List.find ~f:(enabled_in field)
      |> Option.value ~default:Item.Select_all
  }
;;

let move t delta =
  let items = List.filter (List.concat (sections t)) ~f:(enabled t) in
  match List.findi items ~f:(fun _ item -> Item.equal item t.selected) with
  | None -> t
  | Some (i, _) ->
    let n = List.length items in
    { t with selected = List.nth_exn items ((((i + delta) % n) + n) % n) }
;;

let select t item = Option.some_if (enabled t item) { t with selected = item }

module Decision = struct
  type t =
    | Native
    | Suppress
    | Open of Field.t
  [@@deriving sexp_of, equal]
end

let decide ~browser_shell field : Decision.t =
  match browser_shell, field with
  | false, _ -> Native
  | true, None -> Suppress
  | true, Some field -> Open field
;;
