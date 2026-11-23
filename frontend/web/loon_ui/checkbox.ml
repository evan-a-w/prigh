open! Import

module State = struct
  type t =
    | Checked
    | Unchecked
    | Indeterminate
  [@@deriving enumerate, sexp_of, compare, equal]

  let of_bool b = if b then Checked else Unchecked

  let aria = function
    | Checked -> "true"
    | Unchecked -> "false"
    | Indeterminate -> "mixed"
  ;;

  let name = function
    | Checked -> "checked"
    | Unchecked -> "unchecked"
    | Indeterminate -> "indeterminate"
  ;;
end

let view
  ?(attrs = [])
  ?class_
  ?id
  ?(disabled = false)
  ?(required = false)
  ?(invalid = false)
  ~(checked : State.t)
  ~on_change
  ()
  =
  let disabled_attrs = if disabled then [ data "disabled" "" ] else [] in
  Node.button
    ~attrs:
      ([ Attr.type_ "button"
       ; Attr.role "checkbox"
       ; aria "checked" (State.aria checked)
       ]
       (* Radix leaves aria-required out unless [required] is given. *)
       @ (if required then [ aria "required" "true" ] else [])
       @ [ data "state" (State.name checked)
         ; Attr.value "on"
         ; data "slot" "checkbox"
         ; Cn.with_class
             ?class_
             [ "border-input dark:bg-input/30 data-checked:bg-primary \
                data-checked:text-primary-foreground dark:data-checked:bg-primary \
                data-checked:border-primary aria-invalid:aria-checked:border-primary \
                aria-invalid:border-destructive dark:aria-invalid:border-destructive/50 \
                focus-visible:border-ring focus-visible:ring-ring/50 \
                aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 \
                flex size-4 items-center justify-center rounded-[4px] border \
                transition-colors group-has-disabled/field:opacity-50 \
                focus-visible:ring-3 aria-invalid:ring-3 \
                group-has-[:focus-visible]/field-label:ring-0 \
                group-has-[:focus-visible]/field-label:not-data-checked:border-input \
                group-has-[:focus-visible]/field-label:data-checked:border-primary peer \
                relative shrink-0 outline-none after:absolute after:-inset-x-3 \
                after:-inset-y-2 disabled:cursor-not-allowed disabled:opacity-50"
             ]
         ; Attr.on_keydown (fun ev ->
             match Dom_util.key ev with
             | Enter -> Dom_util.prevent_default ev
             | _ -> Effect.Ignore)
         ]
       @ Option.value_map id ~default:[] ~f:(fun id -> [ Attr.id id ])
       @ (if invalid then [ aria "invalid" "true" ] else [])
       @ disabled_attrs
       @ (if disabled
          then [ Attr.disabled ]
          else
            [ Attr.on_click (fun _ ->
                on_change
                  (match checked with
                   | Checked -> false
                   | Unchecked | Indeterminate -> true))
            ])
       @ attrs)
    (match checked with
     | Unchecked -> []
     | Checked | Indeterminate ->
       [ Node.span
           ~attrs:
             ([ data "state" (State.name checked)
              ; data "slot" "checkbox-indicator"
              ; Attr.class_
                  "[&>svg]:size-3.5 grid place-content-center text-current \
                   transition-none"
              ; Attr.style (Css_gen.create ~field:"pointer-events" ~value:"none")
              ]
              @ disabled_attrs)
           [ Icon.view Check ]
       ])
;;

let component
  ?attrs
  ?class_
  ?id
  ?disabled
  ?required
  ?invalid
  ?(default_checked = false)
  (local_ graph)
  =
  Control.make ~equal:Bool.equal default_checked graph ~view:(fun checked set ->
    view
      ?attrs
      ?class_
      ?id
      ?disabled
      ?required
      ?invalid
      ~checked:(State.of_bool checked)
      ~on_change:set
      ())
;;
