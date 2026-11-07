open! Core
open! Import

let el name attrs children =
  Node.create_svg
    name
    ~attrs:(List.map attrs ~f:(fun (k, v) -> Attr.create k v))
    children
;;

let num f =
  let s = sprintf "%.2f" f in
  let s =
    String.rstrip s ~drop:(Char.equal '0')
    |> String.rstrip ~drop:(Char.equal '.')
  in
  if String.equal s "-0" then "0" else s
;;

let svg ?(cls = "") ?(keep_ratio = false) ~width ~height children =
  el
    "svg"
    ([ "class", cls
     ; "viewBox", sprintf "0 0 %s %s" (num width) (num height)
     ; "aria-hidden", "true"
     ; "focusable", "false"
     ]
     @ if keep_ratio then [] else [ "preserveAspectRatio", "none" ])
    children
;;

let path_of_points points =
  let b = Buffer.create 256 in
  let _ : bool =
    List.fold points ~init:false ~f:(fun drawing p ->
      match p with
      | None -> false
      | Some (x, y) ->
        Buffer.add_string
          b
          (sprintf "%s%s %s " (if drawing then "L" else "M") (num x) (num y));
        true)
  in
  String.rstrip (Buffer.contents b)
;;
