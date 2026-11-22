open! Import

module Size = struct
  type t =
    | Default
    | Sm
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Sm -> "sm"
  ;;
end

(* The <select>'s value is set as a property before its options exist, so
   the browser would select the first; set it again once they do. *)
module Value_hook = Vdom.Attr.Hooks.Make (struct
    module State = Unit

    module Input = struct
      type t = string [@@deriving sexp_of]

      let combine _ b = b
    end

    let set (el : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t) value =
      let select : Js_of_ocaml.Dom_html.selectElement Js_of_ocaml.Js.t =
        Js_of_ocaml.Js.Unsafe.coerce el
      in
      select##.value := Js_of_ocaml.Js.string value
    ;;

    let init _ _ = ()

    let on_mount =
      `Schedule_immediately_after_this_dom_patch_completes
        (fun value () el -> set el value)
    ;;

    let update ~old_input:_ ~new_input () el = set el new_input
    let destroy _ () _ = ()
  end)

let view
  ?(attrs = [])
  ?class_
  ?(size = Size.Default)
  ?id
  ?name
  ?(disabled = false)
  ?(required = false)
  ?(invalid = false)
  ?value
  ?on_change
  options
  =
  let opt f = Option.value_map ~default:[] ~f:(fun x -> [ f x ]) in
  Node.div
    ~attrs:
      [ Cn.with_class
          ?class_
          [ "group/native-select relative w-fit has-[select:disabled]:opacity-50"
          ]
      ; data "slot" "native-select-wrapper"
      ; data "size" (Size.name size)
      ]
    [ Node.select
        ~attrs:
          ([ data "slot" "native-select"
           ; data "size" (Size.name size)
           ; Attr.class_
               "border-input placeholder:text-muted-foreground selection:bg-primary \
                selection:text-primary-foreground dark:bg-input/30 \
                dark:hover:bg-input/50 focus-visible:border-ring \
                focus-visible:ring-ring/50 aria-invalid:ring-destructive/20 \
                dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive \
                dark:aria-invalid:border-destructive/50 h-8 w-full min-w-0 \
                appearance-none rounded-lg border bg-transparent py-1 pr-8 pl-2.5 \
                text-sm transition-colors select-none focus-visible:ring-3 \
                aria-invalid:ring-3 data-[size=sm]:h-7 \
                data-[size=sm]:rounded-[min(var(--radius-md),10px)] \
                data-[size=sm]:py-0.5 outline-none disabled:pointer-events-none \
                disabled:cursor-not-allowed"
           ]
           @ opt Attr.id id
           @ opt Attr.name name
           @ (if disabled then [ Attr.disabled ] else [])
           @ (if required then [ Attr.create "required" "" ] else [])
           @ (if invalid then [ aria "invalid" "true" ] else [])
           @ opt Attr.value value
           @ opt
               (fun v -> Vdom.Attr.create_hook "native-select-value" (Value_hook.create v))
               value
           @ opt (fun f -> Attr.on_change (fun _ v -> f v)) on_change
           @ attrs)
        options
    ; Icon.view
        ~attrs:[ data "slot" "native-select-icon" ]
        ~class_:"text-muted-foreground top-1/2 right-2.5 size-4 -translate-y-1/2 \
                 pointer-events-none absolute select-none"
        Chevron_down
    ]
;;

let component
  ?attrs
  ?class_
  ?size
  ?id
  ?name
  ?disabled
  ?required
  ?invalid
  ?(default_value = "")
  options
  (local_ graph)
  =
  Control.make ~equal:String.equal default_value graph ~view:(fun value set ->
    view
      ?attrs
      ?class_
      ?size
      ?id
      ?name
      ?disabled
      ?required
      ?invalid
      ~value
      ~on_change:set
      options)
;;

let option ?(attrs = []) ?class_ ?(disabled = false) ~value children =
  Node.option
    ~attrs:
      ([ data "slot" "native-select-option"
       ; Attr.value value
       ; Cn.with_class ?class_ [ "bg-[Canvas] text-[CanvasText]" ]
       ]
       @ (if disabled then [ Attr.disabled ] else [])
       @ attrs)
    children
;;

let opt_group ?(attrs = []) ?class_ ?(disabled = false) ~label children =
  Node.create
    "optgroup"
    ~attrs:
      ([ data "slot" "native-select-optgroup"
       ; Attr.create "label" label
       ; Cn.with_class ?class_ [ "bg-[Canvas] text-[CanvasText]" ]
       ]
       @ (if disabled then [ Attr.disabled ] else [])
       @ attrs)
    children
;;
