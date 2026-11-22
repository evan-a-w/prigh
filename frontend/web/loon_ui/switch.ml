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

let view
  ?(attrs = [])
  ?class_
  ?(size = Size.Default)
  ?id
  ?(disabled = false)
  ?(required = false)
  ?(invalid = false)
  ~checked
  ~on_change
  ()
  =
  let state = data "state" (if checked then "checked" else "unchecked") in
  let disabled_attrs = if disabled then [ data "disabled" "" ] else [] in
  Node.button
    ~attrs:
      ([ Attr.type_ "button"
       ; Attr.role "switch"
       ; aria_bool "checked" checked
       ]
       (* Radix leaves aria-required out unless [required] is given. *)
       @ (if required then [ aria "required" "true" ] else [])
       @ [ state
         ; Attr.value "on"
         ; data "slot" "switch"
         ; data "size" (Size.name size)
         ; Cn.with_class
             ?class_
             [ "data-checked:bg-primary data-unchecked:bg-input \
                focus-visible:border-ring focus-visible:ring-ring/50 \
                aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 \
                aria-invalid:border-destructive dark:aria-invalid:border-destructive/50 \
                dark:data-unchecked:bg-input/80 shrink-0 rounded-full border \
                border-transparent focus-visible:ring-3 aria-invalid:ring-3 \
                group-has-[:focus-visible]/field-label:ring-0 \
                group-has-[:focus-visible]/field-label:border-transparent \
                data-[size=default]:h-[18.4px] data-[size=default]:w-[32px] \
                data-[size=sm]:h-[14px] data-[size=sm]:w-[24px] peer group/switch \
                relative inline-flex items-center transition-all outline-none \
                after:absolute after:-inset-x-3 after:-inset-y-2 \
                data-disabled:cursor-not-allowed data-disabled:opacity-50"
             ]
         ]
       @ Option.value_map id ~default:[] ~f:(fun id -> [ Attr.id id ])
       @ (if invalid then [ aria "invalid" "true" ] else [])
       @ disabled_attrs
       @ (if disabled
          then [ Attr.disabled ]
          else [ Attr.on_click (fun _ -> on_change (not checked)) ])
       @ attrs)
    [ Node.span
        ~attrs:
          ([ state
           ; data "slot" "switch-thumb"
           ; Attr.class_
               "bg-background dark:data-unchecked:bg-foreground \
                dark:data-checked:bg-primary-foreground rounded-full \
                group-data-[size=default]/switch:size-4 \
                group-data-[size=sm]/switch:size-3 \
                group-data-[size=default]/switch:data-checked:translate-x-[calc(100%-2px)] \
                group-data-[size=sm]/switch:data-checked:translate-x-[calc(100%-2px)] \
                group-data-[size=default]/switch:data-unchecked:translate-x-0 \
                group-data-[size=sm]/switch:data-unchecked:translate-x-0 \
                pointer-events-none block ring-0 transition-transform"
           ]
           @ disabled_attrs)
        []
    ]
;;

let component
  ?attrs
  ?class_
  ?size
  ?id
  ?disabled
  ?required
  ?invalid
  ?(default_checked = false)
  (local_ graph)
  =
  Control.make ~equal:Bool.equal default_checked graph ~view:(fun checked set ->
    view ?attrs ?class_ ?size ?id ?disabled ?required ?invalid ~checked ~on_change:set ())
;;
