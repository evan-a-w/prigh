open! Import

let number f =
  if Float.is_integer f
  then Int.to_string (Float.to_int f)
  else Float.to_string f
;;

let view ?(attrs = []) ?class_ ?(max = 100.) ?value () =
  let max = if Float.( > ) max 0. then max else 100. in
  let value =
    Option.filter value ~f:(fun v -> Float.( >= ) v 0. && Float.( <= ) v max)
  in
  let state =
    match value with
    | None -> "indeterminate"
    | Some v when Float.equal v max -> "complete"
    | Some _ -> "loading"
  in
  let data_attrs =
    [ data "state" state ]
    @ Option.value_map value ~default:[] ~f:(fun v ->
      [ data "value" (number v) ])
    @ [ data "max" (number max) ]
  in
  Node.div
    ~attrs:
      ([ aria "valuemax" (number max); aria "valuemin" "0" ]
       @ Option.value_map value ~default:[] ~f:(fun v ->
         [ aria "valuenow" (number v)
         ; aria
             "valuetext"
             (sprintf "%d%%" (Float.iround_nearest_exn (v /. max *. 100.)))
         ])
       @ [ Attr.role "progressbar" ]
       @ data_attrs
       @ [ data "slot" "progress"
         ; Cn.with_class
             ?class_
             [ "bg-muted h-1 rounded-full relative flex w-full items-center \
                overflow-x-hidden"
             ]
         ]
       @ attrs)
    [ Node.div
        ~attrs:
          (data_attrs
           @ [ data "slot" "progress-indicator"
             ; Attr.class_ "bg-primary size-full flex-1 transition-all"
             ; Attr.style
                 (Css_gen.create
                    ~field:"transform"
                    ~value:
                      (sprintf
                         "translateX(-%s%%)"
                         (number (100. -. Option.value value ~default:0.))))
             ])
        []
    ]
;;
