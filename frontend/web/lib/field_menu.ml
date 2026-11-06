open! Core
open! Import

module Field = struct
  type t =
    { selection : bool
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

  let key = function
    | Cut -> "Ctrl+X"
    | Copy -> "Ctrl+C"
    | Paste -> "Ctrl+V"
    | Select_all -> "Ctrl+A"
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

let enabled_in (field : Field.t) : Item.t -> bool = function
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
      List.find Item.all ~f:(enabled_in field)
      |> Option.value ~default:Item.Select_all
  }
;;

let move t delta =
  let items = List.filter Item.all ~f:(enabled t) in
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
