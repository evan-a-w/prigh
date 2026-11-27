open! Import

module Type = struct
  type t =
    | Text
    | Email
    | Password
    | Search
    | Number
    | Url
    | Tel
    | File
    | Date
    | Time
  [@@deriving enumerate, sexp_of, compare, equal]

  let name t = String.lowercase (Sexp.to_string (sexp_of_t t))
end

let class_list ?class_ () =
  [ "dark:bg-input/30 border-input focus-visible:border-ring focus-visible:ring-ring/50 \
     aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 \
     aria-invalid:border-destructive dark:aria-invalid:border-destructive/50 \
     disabled:bg-input/50 dark:disabled:bg-input/80 h-8 rounded-lg border \
     bg-transparent px-2.5 py-1 text-base transition-colors file:h-6 file:text-sm \
     file:font-medium focus-visible:ring-3 aria-invalid:ring-3 md:text-sm narrow:text-base w-full \
     min-w-0 outline-none file:inline-flex file:border-0 file:bg-transparent \
     file:text-foreground placeholder:text-muted-foreground \
     disabled:pointer-events-none disabled:cursor-not-allowed disabled:opacity-50"
  ]
  @ Option.to_list class_
;;

let classes ?class_ () = Cn.cn (class_list ?class_ ())

let view
  ?(attrs = [])
  ?class_
  ?(type_ = Type.Text)
  ?id
  ?name
  ?placeholder
  ?(disabled = false)
  ?(required = false)
  ?(invalid = false)
  ?value
  ?on_input
  ()
  =
  let opt f = Option.value_map ~default:[] ~f:(fun x -> [ f x ]) in
  Node.input
    ~attrs:
      ([ Attr.type_ (Type.name type_)
       ; data "slot" "input"
       ; Cn.attr (class_list ?class_ ())
       ]
       @ opt Attr.id id
       @ opt Attr.name name
       @ opt Attr.placeholder placeholder
       @ (if disabled then [ Attr.disabled ] else [])
       @ (if required then [ Attr.create "required" "" ] else [])
       @ (if invalid then [ aria "invalid" "true" ] else [])
       @ opt Attr.value value
       @ opt (fun f -> Attr.on_input (fun _ s -> f s)) on_input
       @ attrs)
    ()
;;

let component
  ?attrs
  ?class_
  ?type_
  ?id
  ?name
  ?placeholder
  ?disabled
  ?required
  ?invalid
  ?(default_value = "")
  (local_ graph)
  =
  Control.make ~equal:String.equal default_value graph ~view:(fun value set ->
    view
      ?attrs
      ?class_
      ?type_
      ?id
      ?name
      ?placeholder
      ?disabled
      ?required
      ?invalid
      ~value
      ~on_input:set
      ())
;;
