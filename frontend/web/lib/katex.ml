open! Core
open Js_of_ocaml

let assets =
  Page_assets.create
    ~css:[ "katex.min.css" ]
    ~scripts:[ "katex.min.js"; "copy-tex.min.js" ]
;;

let options ~display =
  Js.Unsafe.obj
    [| "displayMode", Js.Unsafe.inject (Js.bool display)
     ; "throwOnError", Js.Unsafe.inject Js._true
     ; "trust", Js.Unsafe.inject Js._false
     ; "maxExpand", Js.Unsafe.inject 1000
     ; "maxSize", Js.Unsafe.inject 20
     ; "strict", Js.Unsafe.inject (Js.string "ignore")
       (* What [trust] refuses shows as its source, in the theme's red. *)
     ; "errorColor", Js.Unsafe.inject (Js.string "var(--brick)")
    |]
;;

let render ~display tex =
  if not (Page_assets.loaded assets)
  then None
  else (
    let katex = Js.Unsafe.global##.katex in
    match
      Js.to_string
        (Js.Unsafe.meth_call
           katex
           "renderToString"
           [| Js.Unsafe.inject (Js.string tex); options ~display |])
    with
    | html -> Some (Ok html)
    | exception Js_error.Exn e ->
      (* KaTeX underlines where it stopped with combining low lines, which
         a title shows as noise. *)
      Some
        (Error
           (String.Search_pattern.replace_all
              (String.Search_pattern.create "\u{0332}")
              ~in_:(Js_error.message e)
              ~with_:"")))
;;

let loaded () = Page_assets.loaded assets

let when_loaded k =
  Page_assets.load assets (function
    | Ok () -> k ()
    | Error _ -> ())
;;
