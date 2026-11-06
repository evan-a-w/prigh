open! Core
open! Import

let types =
  [ "card"
  ; "grid"
  ; "tabs"
  ; "accordion"
  ; "divider"
  ; "markdown"
  ; "callout"
  ; "stat"
  ; "kv"
  ; "progress"
  ; "badges"
  ; "table"
  ; "chart"
  ; "timeline"
  ; "steps"
  ; "code"
  ; "diff"
  ; "tree"
  ; "image"
  ; "gallery"
  ; "calculator"
  ; "flashcards"
  ; "quiz"
  ; "form"
  ; "choices"
  ; "checklist"
  ]
;;

module Checked = struct
  type t =
    { summaries : string list
    ; images : (string * string) list
    ; interactive : string list
    }
  [@@deriving sexp_of]
end

let max_top_level = 20
let max_components = 100
let max_depth = 4
let max_children = 20
let tones = [ "neutral"; "info"; "success"; "warning"; "danger" ]

let one_of = function
  | [] -> ""
  | [ x ] -> x
  | xs ->
    let rev = List.rev xs in
    String.concat ~sep:", " (List.rev (List.tl_exn rev))
    ^ " or "
    ^ List.hd_exn rev
;;

let suggest candidates name =
  match Edit_distance.closest ~n:1 candidates name with
  | [ best ] when Edit_distance.caseless best name <= 2 ->
    Some (sprintf "did you mean %S?" best)
  | _ -> None
;;

let describe_json : Json.t -> string = function
  | `Null -> "null"
  | `True | `False -> "a boolean"
  | `String _ -> "a string"
  | `Number _ -> "a number"
  | `Object _ -> "an object"
  | `Array _ -> "an array"
;;

let plural n word = if n = 1 then sprintf "1 %s" word else sprintf "%d %ss" n word

type ctx =
  { errors : string Queue.t
  ; ids : string String.Table.t (** id to the path of its component *)
  ; images : (string * string) Queue.t
  ; interactive : string Queue.t
  ; resolve : string -> string
  ; mutable count : int
  }

let error ctx path fmt =
  ksprintf (fun s -> Queue.enqueue ctx.errors (path ^ ": " ^ s)) fmt
;;

let at path name = path ^ "." ^ name
let nth path i = sprintf "%s[%d]" path i

(* An object being checked: the accessors record which fields it takes, so
   that [finish] can report the others. *)
module Obj = struct
  type t =
    { ctx : ctx
    ; path : string
    ; what : string
    ; fields : (string * Json.t) list
    ; mutable takes : string list
    }

  let create ctx path ~what (json : Json.t) =
    match json with
    | `Object fields -> Some { ctx; path; what; fields; takes = [] }
    | other ->
      error ctx path "must be an object, not %s" (describe_json other);
      None
  ;;

  let take t name =
    t.takes <- name :: t.takes;
    match List.Assoc.find t.fields name ~equal:String.equal with
    | None | Some `Null -> None
    | Some json -> Some json
  ;;

  let missing t name = error t.ctx (at t.path name) "missing; %s needs it" t.what

  let field t name ~required ~f =
    match take t name with
    | None ->
      if required then missing t name;
      None
    | Some json -> f (at t.path name) json
  ;;

  let string ?(required = false) t name =
    field t name ~required ~f:(fun path -> function
      | `String s when required && String.is_empty (String.strip s) ->
        error t.ctx path "is empty; give some text";
        None
      | `String s -> Some s
      | `Number n ->
        error t.ctx path "must be a string, not a number: write \"%s\"" n;
        None
      | other ->
        error t.ctx path "must be a string, not %s" (describe_json other);
        None)
  ;;

  let number_of ctx path : Json.t -> float option = function
    | `Number n ->
      (match Float.of_string_opt n with
       | Some f when Float.is_finite f -> Some f
       | _ ->
         error ctx path "%s is not a finite number" n;
         None)
    | other ->
      error ctx path "must be a number, not %s" (describe_json other);
      None
  ;;

  let number ?(required = false) t name =
    field t name ~required ~f:(number_of t.ctx)
  ;;

  let int ?(required = false) ?min ?max t name =
    field t name ~required ~f:(fun path json ->
      Option.bind (number_of t.ctx path json) ~f:(fun f ->
        if not (Float.is_integer f)
        then (
          error t.ctx path "must be a whole number, not %s" (Float.to_string f);
          None)
        else (
          let i = Float.to_int f in
          match min, max with
          | Some lo, Some hi when i < lo || i > hi ->
            error t.ctx path "is %d; use %d to %d" i lo hi;
            None
          | Some lo, None when i < lo ->
            error t.ctx path "is %d; use %d or more" i lo;
            None
          | _ -> Some i)))
  ;;

  let bool t name =
    field t name ~required:false ~f:(fun path -> function
      | `True -> Some true
      | `False -> Some false
      | other ->
        error t.ctx path "must be true or false, not %s" (describe_json other);
        None)
  ;;

  let enum ?(required = false) t name values =
    field t name ~required ~f:(fun path -> function
      | `String s when List.mem values s ~equal:String.equal -> Some s
      | `String s ->
        error t.ctx path "is %S; use %s" s (one_of values);
        None
      | other ->
        error
          t.ctx
          path
          "must be one of %s, not %s"
          (one_of values)
          (describe_json other);
        None)
  ;;

  let tone t = ignore (enum t "tone" tones : string option)

  (* A string or a number, as written. *)
  let text_or_number ?(required = false) t name =
    field t name ~required ~f:(fun path -> function
      | `String s -> Some s
      | `Number n -> Some n
      | other ->
        error
          t.ctx
          path
          "must be a string or a number, not %s"
          (describe_json other);
        None)
  ;;

  (* The items with their paths, when there are [min] to [max] of them. *)
  let array ?(required = false) ?(min = 1) t name ~max ~noun =
    field t name ~required ~f:(fun path -> function
      | `Array items ->
        let n = List.length items in
        if n < min
        then (
          error
            t.ctx
            path
            "has %s; give at least %d"
            (if n = 0 then "none" else plural n noun)
            min;
          None)
        else if n > max
        then (
          error t.ctx path "has %s; give at most %d" (plural n noun) max;
          None)
        else Some (List.mapi items ~f:(fun i item -> nth path i, item))
      | other ->
        error t.ctx path "must be an array of %ss, not %s" noun (describe_json other);
        None)
  ;;

  let finish ?(also = []) t =
    let takes = also @ t.takes in
    List.iter t.fields ~f:(fun (name, _) ->
      if not (List.mem takes name ~equal:String.equal)
      then (
        let known = List.rev (List.filter takes ~f:(fun n -> not (String.equal n "type"))) in
        error
          t.ctx
          (at t.path name)
          "%s does not take %S; %s"
          t.what
          name
          (match suggest known name with
           | Some hint -> hint
           | None ->
             (match known with
              | [] -> "remove it"
              | known -> "it takes " ^ one_of known))))
  ;;
end

(* Each item of an array of objects. *)
let objects ctx items ~what ~f =
  List.filter_map items ~f:(fun (path, json) ->
    Option.map (Obj.create ctx path ~what json) ~f:(fun o ->
      let r = f o in
      Obj.finish o;
      r))
;;

let strings ctx items =
  List.filter_map items ~f:(fun (path, json) ->
    match (json : Json.t) with
    | `String s -> Some s
    | `Number n ->
      error ctx path "must be a string, not a number: write \"%s\"" n;
      None
    | other ->
      error ctx path "must be a string, not %s" (describe_json other);
      None)
;;

let quoted = function
  | None -> ""
  | Some s ->
    let s = String.strip s in
    let s =
      if String.length s > 60 then String.prefix s 57 ^ "..." else s
    in
    sprintf " %S" s
;;

let is_identifier s =
  (not (String.is_empty s))
  && (Char.is_alpha s.[0] || Char.equal s.[0] '_')
  && String.for_all s ~f:(fun c -> Char.is_alphanum c || Char.equal c '_')
;;

let check_image ctx (o : Obj.t) =
  match Obj.string ~required:true o "path" with
  | None -> ()
  | Some path ->
    let field = at o.path "path" in
    if String.mem path '\000'
    then error ctx field "contains NUL"
    else if Filename.is_absolute path
    then Queue.enqueue ctx.images (field, path)
    else
      error
        ctx
        field
        "%S is relative; give the absolute path: use %s"
        path
        (ctx.resolve path)
;;

let check_date ctx path s =
  let ok =
    match String.split s ~on:'-' with
    | [ y; m; d ] ->
      String.length y = 4
      && String.length m = 2
      && String.length d = 2
      && String.for_all (y ^ m ^ d) ~f:Char.is_digit
      && Option.is_some
           (Option.try_with (fun () ->
              Date.create_exn
                ~y:(Int.of_string y)
                ~m:(Month.of_int_exn (Int.of_string m))
                ~d:(Int.of_string d)))
    | _ -> false
  in
  if not ok then error ctx path "%S is not a date; write YYYY-MM-DD" s
;;

let rec component ctx path ~depth (json : Json.t) =
  ctx.count <- ctx.count + 1;
  match json with
  | `Object fields ->
    (match List.Assoc.find fields "type" ~equal:String.equal with
     | None ->
       error ctx (at path "type") "missing; give one of %s" (one_of types);
       None
     | Some (`String type_) when List.mem types type_ ~equal:String.equal ->
       let o = { Obj.ctx; path; what = type_; fields; takes = [ "type" ] } in
       let id =
         Option.map (Obj.string o "id") ~f:(fun id ->
           let field = at path "id" in
           if String.is_empty (String.strip id)
           then error ctx field "is empty; give a short name, e.g. \"deploy\""
           else (
             match Hashtbl.find ctx.ids id with
             | Some other ->
               error
                 ctx
                 field
                 "%S is also %s's id; ids must be unique within the call"
                 id
                 other
             | None -> Hashtbl.set ctx.ids ~key:id ~data:path);
           id)
       in
       let summary = check_type ctx o ~type_ ~id ~depth in
       Obj.finish o;
       Some summary
     | Some (`String type_) ->
       error
         ctx
         (at path "type")
         "unknown type %S; %s"
         type_
         (match suggest types type_ with
          | Some hint -> hint
          | None -> "use one of " ^ one_of types);
       None
     | Some other ->
       error
         ctx
         (at path "type")
         "must be a string, not %s; give one of %s"
         (describe_json other)
         (one_of types);
       None)
  | other ->
    error
      ctx
      path
      "must be a component object ({\"type\": ...}), not %s"
      (describe_json other);
    None

and children ctx (o : Obj.t) ?(name = "children") ~depth () =
  match Obj.array ~required:true o name ~max:max_children ~noun:"component" with
  | None -> 0
  | Some items ->
    if depth + 1 >= max_depth
    then (
      error
        ctx
        (at o.path name)
        "nested too deep (at most %d levels of components); flatten the layout"
        max_depth;
      0)
    else (
      List.iter items ~f:(fun (path, json) ->
        ignore (component ctx path ~depth:(depth + 1) json : string option));
      List.length items)

and check_type ctx (o : Obj.t) ~type_ ~id ~depth =
  let title () = Obj.string o "title" in
  let count n noun = sprintf " (%s)" (plural n noun) in
  let items_count = Option.value_map ~default:0 ~f:List.length in
  let interactive summary = Queue.enqueue ctx.interactive summary in
  let need_id what =
    if Option.is_none id
    then error ctx (at o.path "id") "missing; %s needs an id (unique in the call) to reply" what
  in
  match type_ with
  | "card" ->
    let title = title () in
    ignore (Obj.string o "subtitle" : string option);
    ignore (children ctx o ~depth () : int);
    "card" ^ quoted title
  | "grid" ->
    ignore (Obj.int o "columns" ~min:1 ~max:4 : int option);
    let n = children ctx o ~depth () in
    "grid" ^ count n "component"
  | "tabs" ->
    let tabs =
      Obj.array ~required:true ~min:2 o "tabs" ~max:8 ~noun:"tab"
      |> Option.value ~default:[]
    in
    let labels =
      objects ctx tabs ~what:"a tab" ~f:(fun tab ->
        let label = Obj.string ~required:true tab "label" in
        ignore (children ctx tab ~depth () : int);
        label)
    in
    "tabs" ^ quoted (Some (String.concat ~sep:", " (List.filter_opt labels)))
  | "accordion" ->
    let items =
      Obj.array ~required:true o "items" ~max:max_children ~noun:"item"
      |> Option.value ~default:[]
    in
    ignore
      (objects ctx items ~what:"an accordion item" ~f:(fun item ->
         ignore (Obj.string ~required:true item "title" : string option);
         ignore (Obj.bool item "open" : bool option);
         ignore (children ctx item ~depth () : int))
       : unit list);
    "accordion" ^ count (List.length items) "section"
  | "divider" -> "divider"
  | "markdown" ->
    ignore (Obj.string ~required:true o "text" : string option);
    "markdown"
  | "callout" ->
    Obj.tone o;
    let title = title () in
    ignore (Obj.string ~required:true o "text" : string option);
    "callout" ^ quoted title
  | "stat" ->
    let label = Obj.string ~required:true o "label" in
    ignore (Obj.text_or_number ~required:true o "value" : string option);
    ignore (Obj.string o "unit" : string option);
    ignore (Obj.text_or_number o "delta" : string option);
    ignore (Obj.enum o "trend" [ "up"; "down"; "flat" ] : string option);
    ignore (Obj.enum o "good" [ "up"; "down" ] : string option);
    ignore (Obj.string o "caption" : string option);
    Option.iter
      (Obj.array ~min:2 o "spark" ~max:200 ~noun:"point")
      ~f:(List.iter ~f:(fun (path, json) ->
        ignore (Obj.number_of ctx path json : float option)));
    "stat" ^ quoted label
  | "kv" ->
    let title = title () in
    let items = Obj.array ~required:true o "items" ~max:50 ~noun:"item" in
    ignore
      (objects ctx (Option.value items ~default:[]) ~what:"a kv item" ~f:(fun item ->
         ignore (Obj.string ~required:true item "key" : string option);
         ignore (Obj.string ~required:true item "value" : string option))
       : unit list);
    "kv" ^ quoted title ^ count (items_count items) "item"
  | "progress" ->
    let label = Obj.string o "label" in
    let value = Obj.number ~required:true o "value" in
    let max = Obj.number o "max" in
    Obj.tone o;
    ignore (Obj.string o "caption" : string option);
    (match max with
     | Some m when Float.(m <= 0.) ->
       error ctx (at o.path "max") "must be more than 0"
     | _ ->
       let top = Option.value max ~default:100. in
       Option.iter value ~f:(fun v ->
         if Float.(v < 0. || v > top)
         then
           error
             ctx
             (at o.path "value")
             "is %g, outside 0 to %g; give a value in that range (or set max)"
             v
             top));
    "progress" ^ quoted label
  | "badges" ->
    let items = Obj.array ~required:true o "items" ~max:30 ~noun:"badge" in
    ignore
      (objects ctx (Option.value items ~default:[]) ~what:"a badge" ~f:(fun b ->
         ignore (Obj.string ~required:true b "text" : string option);
         Obj.tone b)
       : unit list);
    "badges" ^ count (items_count items) "badge"
  | "table" -> table ctx o
  | "chart" -> chart ctx o
  | "timeline" ->
    let title = title () in
    let items = Obj.array ~required:true o "items" ~max:50 ~noun:"item" in
    ignore
      (objects ctx (Option.value items ~default:[]) ~what:"a timeline item" ~f:(fun item ->
         ignore (Obj.string ~required:true item "title" : string option);
         ignore (Obj.string item "time" : string option);
         ignore (Obj.string item "text" : string option);
         Obj.tone item)
       : unit list);
    "timeline" ^ quoted title ^ count (items_count items) "item"
  | "steps" ->
    let title = title () in
    let items = Obj.array ~required:true o "items" ~max:50 ~noun:"step" in
    ignore
      (objects ctx (Option.value items ~default:[]) ~what:"a step" ~f:(fun item ->
         ignore (Obj.string ~required:true item "title" : string option);
         ignore (Obj.string item "text" : string option);
         ignore
           (Obj.enum item "status" [ "done"; "current"; "todo"; "failed" ]
            : string option))
       : unit list);
    "steps" ^ quoted title ^ count (items_count items) "step"
  | "code" ->
    ignore (Obj.string o "lang" : string option);
    let title = title () in
    ignore (Obj.string ~required:true o "code" : string option);
    "code" ^ quoted title
  | "diff" ->
    let title = title () in
    ignore (Obj.string ~required:true o "diff" : string option);
    "diff" ^ quoted title
  | "tree" ->
    let title = title () in
    let entries = Obj.array ~required:true o "entries" ~max:500 ~noun:"entry" in
    ignore
      (objects ctx (Option.value entries ~default:[]) ~what:"a tree entry" ~f:(fun e ->
         Option.iter (Obj.string ~required:true e "path") ~f:(fun p ->
           let path = at e.path "path" in
           if String.is_prefix p ~prefix:"/"
           then
             error
               ctx
               path
               "%S starts with \"/\"; give paths relative to the tree's root, \
                e.g. \"src/main.ml\""
               p
           else if
             List.exists
               (String.split (String.chop_suffix_if_exists p ~suffix:"/") ~on:'/')
               ~f:String.is_empty
           then error ctx path "%S has an empty segment; separate names with one \"/\"" p);
         ignore (Obj.string e "note" : string option);
         Obj.tone e)
       : unit list);
    "tree" ^ quoted title ^ count (items_count entries) "entry"
  | "image" ->
    check_image ctx o;
    ignore (Obj.string o "caption" : string option);
    ignore (Obj.string o "alt" : string option);
    ignore (Obj.enum o "width" [ "small"; "medium"; "full" ] : string option);
    (match List.Assoc.find o.fields "path" ~equal:String.equal with
     | Some (`String p) -> "image " ^ p
     | _ -> "image")
  | "gallery" ->
    let title = title () in
    let images = Obj.array ~required:true o "images" ~max:24 ~noun:"image" in
    ignore
      (objects ctx (Option.value images ~default:[]) ~what:"a gallery image" ~f:(fun i ->
         check_image ctx i;
         ignore (Obj.string i "caption" : string option);
         ignore (Obj.string i "alt" : string option))
       : unit list);
    "gallery" ^ quoted title ^ count (items_count images) "image"
  | "calculator" -> calculator ctx o
  | "flashcards" ->
    let title = title () in
    let cards = Obj.array ~required:true o "cards" ~max:50 ~noun:"card" in
    ignore
      (objects ctx (Option.value cards ~default:[]) ~what:"a flashcard" ~f:(fun c ->
         ignore (Obj.string ~required:true c "front" : string option);
         ignore (Obj.string ~required:true c "back" : string option))
       : unit list);
    "flashcards" ^ quoted title ^ count (items_count cards) "card"
  | "quiz" ->
    let title = title () in
    let questions = Obj.array ~required:true o "questions" ~max:50 ~noun:"question" in
    ignore
      (objects ctx (Option.value questions ~default:[]) ~what:"a quiz question" ~f:(fun q ->
         ignore (Obj.string ~required:true q "question" : string option);
         let options =
           Option.map
             (Obj.array ~required:true ~min:2 q "options" ~max:6 ~noun:"option")
             ~f:(strings ctx)
         in
         let answer = Obj.int ~required:true q "answer" in
         ignore (Obj.string q "explanation" : string option);
         match options, answer with
         | Some options, Some answer
           when answer < 0 || answer >= List.length options ->
           error
             ctx
             (at q.path "answer")
             "is %d, but there are %d options; give the right option's index \
              from 0 to %d"
             answer
             (List.length options)
             (List.length options - 1)
         | _ -> ())
       : unit list);
    "quiz" ^ quoted title ^ count (items_count questions) "question"
  | "form" ->
    need_id "a form";
    let title = title () in
    ignore (Obj.string o "description" : string option);
    ignore (Obj.string o "submit" : string option);
    form_fields ctx o;
    let summary = "form" ^ quoted (Option.first_some title id) in
    interactive summary;
    summary
  | "choices" ->
    need_id "choices";
    let prompt = Obj.string o "prompt" in
    ignore (Obj.bool o "multiple" : bool option);
    let options = Obj.array ~required:true o "options" ~max:12 ~noun:"option" in
    let values = String.Table.create () in
    ignore
      (objects ctx (Option.value options ~default:[]) ~what:"a choice" ~f:(fun c ->
         let label = Obj.string ~required:true c "label" in
         let value = Obj.string c "value" in
         ignore (Obj.string c "description" : string option);
         Obj.tone c;
         Option.iter (Option.first_some value label) ~f:(fun v ->
           match Hashtbl.find values v with
           | Some other ->
             error
               ctx
               c.path
               "sends %S like %s; give each option its own value"
               v
               other
           | None -> Hashtbl.set values ~key:v ~data:c.path))
       : unit list);
    let summary = "choices" ^ quoted (Option.first_some prompt id) in
    interactive summary;
    summary
  | "checklist" ->
    let title = title () in
    let submit = Obj.string o "submit" in
    let items = Obj.array ~required:true o "items" ~max:50 ~noun:"item" in
    ignore
      (objects ctx (Option.value items ~default:[]) ~what:"a checklist item" ~f:(fun i ->
         ignore (Obj.string ~required:true i "text" : string option);
         ignore (Obj.bool i "checked" : bool option))
       : unit list);
    let summary =
      "checklist" ^ quoted (Option.first_some title id) ^ count (items_count items) "item"
    in
    if Option.is_some submit
    then (
      need_id "a checklist with submit";
      interactive summary);
    summary
  | other -> raise_s [%message "unhandled component type" other]

and table ctx (o : Obj.t) =
  let title = Obj.string o "title" in
  ignore (Obj.bool o "sortable" : bool option);
  ignore (Obj.string o "caption" : string option);
  let columns = Obj.array ~required:true o "columns" ~max:12 ~noun:"column" in
  let keys = String.Table.create () in
  ignore
    (objects ctx (Option.value columns ~default:[]) ~what:"a table column" ~f:(fun c ->
       Option.iter (Obj.string ~required:true c "key") ~f:(fun key ->
         match Hashtbl.find keys key with
         | Some other ->
           error
             ctx
             (at c.path "key")
             "%S is also %s's key; give each column its own"
             key
             other
         | None -> Hashtbl.set keys ~key ~data:c.path);
       ignore (Obj.string c "label" : string option);
       ignore (Obj.enum c "align" [ "left"; "right"; "center" ] : string option);
       ignore
         (Obj.enum
            c
            "format"
            [ "text"; "number"; "percent"; "currency"; "date"; "md" ]
          : string option);
       Option.iter (Obj.string c "currency") ~f:(fun cur ->
         if not (String.length cur = 3 && String.for_all cur ~f:Char.is_uppercase)
         then
           error
             ctx
             (at c.path "currency")
             "%S is not an ISO currency code; write e.g. \"USD\" or \"EUR\""
             cur);
       ignore (Obj.int c "decimals" ~min:0 ~max:10 : int option))
     : unit list);
  let rows =
    Option.value
      (Obj.array ~required:true ~min:0 o "rows" ~max:500 ~noun:"row")
      ~default:[]
  in
  let column_keys = Hashtbl.keys keys |> List.sort ~compare:String.compare in
  List.iter rows ~f:(fun (path, row) ->
    match row with
    | `Object cells ->
      List.iter cells ~f:(fun (key, value) ->
        let cell = at path key in
        if Option.is_some columns && not (Hashtbl.mem keys key)
        then
          error
            ctx
            cell
            "no column has key %S; %s"
            key
            (match suggest column_keys key with
             | Some hint -> hint
             | None ->
               sprintf
                 "add a column for it or use one of %s"
                 (one_of (List.map column_keys ~f:(sprintf "%S"))))
        else (
          match (value : Json.t) with
          | `String _ | `Number _ | `True | `False | `Null -> ()
          | other ->
            error
              ctx
              cell
              "must be a string, number, boolean or null, not %s"
              (describe_json other)))
    | other ->
      error
        ctx
        path
        "must be an object of cells by column key, not %s"
        (describe_json other));
  "table" ^ quoted title ^ sprintf " (%s)" (plural (List.length rows) "row")

and chart ctx (o : Obj.t) =
  let kinds =
    [ "line"; "area"; "bar"; "stacked_bar"; "horizontal_bar"; "pie"; "donut"; "scatter" ]
  in
  let kind = Obj.enum ~required:true o "kind" kinds in
  let title = Obj.string o "title" in
  List.iter [ "subtitle"; "unit"; "x_label"; "y_label" ] ~f:(fun name ->
    ignore (Obj.string o name : string option));
  ignore (Obj.enum o "height" [ "small"; "medium"; "large" ] : string option);
  let series_count = ref 0 in
  let wrong_for kind names ~takes =
    List.iter names ~f:(fun name ->
      if List.Assoc.mem o.fields name ~equal:String.equal
      then (
        error
          ctx
          (at o.path name)
          "a %s chart does not take %s; it takes %s"
          kind
          name
          takes;
        o.takes <- name :: o.takes))
  in
  (match kind with
   | None -> o.takes <- [ "labels"; "series"; "slices" ] @ o.takes
   | Some (("line" | "area" | "bar" | "stacked_bar" | "horizontal_bar") as kind) ->
     wrong_for kind [ "slices" ] ~takes:"labels and series";
     let labels =
       Option.map (Obj.array ~required:true o "labels" ~max:200 ~noun:"label") ~f:(fun l ->
         List.length (strings ctx l))
     in
     let series = Obj.array ~required:true o "series" ~max:8 ~noun:"series" in
     series_count := Option.value_map series ~default:0 ~f:List.length;
     ignore
       (objects ctx (Option.value series ~default:[]) ~what:"a chart series" ~f:(fun s ->
          ignore (Obj.string ~required:true s "name" : string option);
          wrong_for kind [ "points" ] ~takes:"values (one per label)";
          match Obj.field s "values" ~required:true ~f:(fun p j -> Some (p, j)) with
          | Some (path, `Array values) ->
            List.iteri values ~f:(fun i v ->
              match (v : Json.t) with
              | `Null -> ()
              | v -> ignore (Obj.number_of ctx (nth path i) v : float option));
            Option.iter labels ~f:(fun labels ->
              let n = List.length values in
              if n <> labels
              then
                error
                  ctx
                  path
                  "%s for %s; give one per label (null for a gap)"
                  (plural n "value")
                  (plural labels "label"))
          | Some (path, other) ->
            error ctx path "must be an array of numbers, not %s" (describe_json other)
          | None -> ())
        : unit list)
   | Some (("pie" | "donut") as kind) ->
     wrong_for kind [ "labels"; "series" ] ~takes:"slices";
     let slices = Obj.array ~required:true o "slices" ~max:12 ~noun:"slice" in
     let values =
       objects ctx (Option.value slices ~default:[]) ~what:"a slice" ~f:(fun s ->
         ignore (Obj.string ~required:true s "label" : string option);
         match Obj.number ~required:true s "value" with
         | Some v when Float.(v < 0.) ->
           error ctx (at s.path "value") "is %g; slices cannot be negative" v;
           None
         | v -> v)
     in
     if
       (not (List.is_empty values))
       && List.for_all values ~f:(Option.value_map ~default:false ~f:Float.(fun v -> v = 0.))
     then error ctx (at o.path "slices") "are all 0; give the slices' sizes"
   | Some kind ->
     wrong_for kind [ "labels"; "slices" ] ~takes:"series of points";
     let series = Obj.array ~required:true o "series" ~max:8 ~noun:"series" in
     series_count := Option.value_map series ~default:0 ~f:List.length;
     let total =
       objects ctx (Option.value series ~default:[]) ~what:"a scatter series" ~f:(fun s ->
         ignore (Obj.string ~required:true s "name" : string option);
         wrong_for kind [ "values" ] ~takes:"points ([[x, y], ...])";
         let points =
           Option.value
             (Obj.array ~required:true s "points" ~max:2000 ~noun:"point")
             ~default:[]
         in
         List.iter points ~f:(fun (path, p) ->
           match (p : Json.t) with
           | `Array [ x; y ] ->
             ignore (Obj.number_of ctx (nth path 0) x : float option);
             ignore (Obj.number_of ctx (nth path 1) y : float option)
           | other ->
             error ctx path "must be an [x, y] pair of numbers, not %s"
               (match other with
                | `Array l -> sprintf "an array of %d" (List.length l)
                | other -> describe_json other));
         List.length points)
       |> List.sum (module Int) ~f:Fn.id
     in
     if total > 2000
     then
       error
         ctx
         (at o.path "series")
         "has %d points in all; give at most 2000 (sample them)"
         total);
  "chart"
  ^ quoted title
  ^ if !series_count > 1 then sprintf " (%d series)" !series_count else ""

and calculator ctx (o : Obj.t) =
  let title = Obj.string o "title" in
  let inputs = Obj.array ~required:true o "inputs" ~max:20 ~noun:"input" in
  let names = String.Table.create () in
  ignore
    (objects ctx (Option.value inputs ~default:[]) ~what:"a calculator input" ~f:(fun i ->
       Option.iter (Obj.string ~required:true i "name") ~f:(fun name ->
         let path = at i.path "name" in
         if not (is_identifier name)
         then
           error
             ctx
             path
             "%S is not an identifier; use letters, digits and _, e.g. \
              \"monthly_rate\""
             name
         else if List.Assoc.mem Ui_expr.functions name ~equal:String.equal
         then error ctx path "%S is a function's name; call the input something else" name
         else (
           match Hashtbl.find names name with
           | Some other ->
             error ctx path "%S is also %s's name; give each input its own" name other
           | None -> Hashtbl.set names ~key:name ~data:i.path));
       ignore (Obj.string ~required:true i "label" : string option);
       let default = Obj.number ~required:true i "default" in
       let min = Obj.number i "min" in
       let max = Obj.number i "max" in
       (match Obj.number i "step" with
        | Some s when Float.(s <= 0.) ->
          error ctx (at i.path "step") "must be more than 0"
        | _ -> ());
       ignore (Obj.string i "unit" : string option);
       let slider = Option.value (Obj.bool i "slider") ~default:false in
       if slider && (Option.is_none min || Option.is_none max)
       then error ctx i.path "a slider needs min and max";
       (match min, max with
        | Some lo, Some hi when Float.(lo >= hi) ->
          error ctx (at i.path "max") "is %g, not more than min %g" hi lo
        | _ ->
          Option.iter default ~f:(fun d ->
            let below = Option.exists min ~f:(fun lo -> Float.(d < lo)) in
            let above = Option.exists max ~f:(fun hi -> Float.(d > hi)) in
            if below || above
            then
              error
                ctx
                (at i.path "default")
                "is %g, outside %s to %s"
                d
                (Option.value_map min ~default:"-inf" ~f:(sprintf "%g"))
                (Option.value_map max ~default:"inf" ~f:(sprintf "%g")))))
     : unit list);
  let input_names =
    List.filter_map (Option.value inputs ~default:[]) ~f:(fun (_, j) ->
      match j with
      | `Object fields ->
        (match List.Assoc.find fields "name" ~equal:String.equal with
         | Some (`String n) when is_identifier n -> Some n
         | _ -> None)
      | _ -> None)
  in
  let outputs = Obj.array ~required:true o "outputs" ~max:20 ~noun:"output" in
  ignore
    (objects ctx (Option.value outputs ~default:[]) ~what:"a calculator output" ~f:(fun out ->
       ignore (Obj.string ~required:true out "label" : string option);
       ignore
         (Obj.enum out "format" [ "number"; "percent"; "currency"; "integer" ]
          : string option);
       ignore (Obj.int out "decimals" ~min:0 ~max:10 : int option);
       ignore (Obj.string out "unit" : string option);
       Option.iter (Obj.string ~required:true out "expr") ~f:(fun expr ->
         let path = at out.path "expr" in
         match Ui_expr.parse expr with
         | Error e -> error ctx path "%s in %S" e expr
         | Ok e ->
           List.iter (Ui_expr.check e ~inputs:input_names) ~f:(fun problem ->
             error ctx path "%s" problem)))
     : unit list);
  "calculator" ^ quoted title

and form_fields ctx (o : Obj.t) =
  let kinds =
    [ "text"; "textarea"; "number"; "select"; "radio"; "checkbox"; "toggle"; "slider"; "date" ]
  in
  let fields = Obj.array ~required:true o "fields" ~max:30 ~noun:"field" in
  let names = String.Table.create () in
  ignore
    (objects ctx (Option.value fields ~default:[]) ~what:"a form field" ~f:(fun f ->
       Option.iter (Obj.string ~required:true f "name") ~f:(fun name ->
         match Hashtbl.find names name with
         | Some other ->
           error
             ctx
             (at f.path "name")
             "%S is also %s's name; give each field its own"
             name
             other
         | None -> Hashtbl.set names ~key:name ~data:f.path);
       ignore (Obj.string ~required:true f "label" : string option);
       ignore (Obj.bool f "required" : bool option);
       ignore (Obj.string f "placeholder" : string option);
       ignore (Obj.string f "help" : string option);
       let kind = Obj.enum ~required:true f "kind" kinds in
       let with_options = [ "select"; "radio"; "checkbox" ] in
       let numeric = [ "number"; "slider" ] in
       let options =
         match kind with
         | Some k when List.mem with_options k ~equal:String.equal ->
           Option.map
             (Obj.array ~required:true f "options" ~max:50 ~noun:"option")
             ~f:(strings ctx)
         | Some k ->
           if List.Assoc.mem f.fields "options" ~equal:String.equal
           then (
             f.takes <- "options" :: f.takes;
             error
               ctx
               (at f.path "options")
               "a %s field has no options; use kind select, radio or checkbox"
               k);
           None
         | None ->
           f.takes <- "options" :: f.takes;
           None
       in
       let range =
         List.map [ "min"; "max"; "step" ] ~f:(fun name ->
           match kind with
           | Some k when not (List.mem numeric k ~equal:String.equal) ->
             if List.Assoc.mem f.fields name ~equal:String.equal
             then (
               f.takes <- name :: f.takes;
               error
                 ctx
                 (at f.path name)
                 "only number and slider fields take %s"
                 name);
             None
           | _ -> Obj.number f name)
       in
       (match kind, range with
        | Some "slider", ([ None; _; _ ] | [ _; None; _ ]) ->
          error ctx f.path "a slider needs min and max"
        | _, [ Some lo; Some hi; _ ] when Float.(lo >= hi) ->
          error ctx (at f.path "max") "is %g, not more than min %g" hi lo
        | _, [ _; _; Some s ] when Float.(s <= 0.) ->
          error ctx (at f.path "step") "must be more than 0"
        | _ -> ());
       let default_path = at f.path "default" in
       let wrong expected (json : Json.t) =
         error
           ctx
           default_path
           "a %s field's default must be %s, not %s"
           (Option.value kind ~default:"")
           expected
           (describe_json json)
       in
       let in_options v =
         Option.iter options ~f:(fun options ->
           if not (List.mem options v ~equal:String.equal)
           then
             error
               ctx
               default_path
               "%S is not one of the options (%s)"
               v
               (String.concat ~sep:", " (List.map options ~f:(sprintf "%S"))))
       in
       ignore
         (Obj.field f "default" ~required:false ~f:(fun _ json ->
            (match kind, json with
             | None, _ -> ()
             | Some ("text" | "textarea"), `String _ -> ()
             | Some "date", `String s -> check_date ctx default_path s
             | Some ("select" | "radio"), `String s -> in_options s
             | Some ("text" | "textarea" | "date" | "select" | "radio"), json ->
               wrong "a string" json
             | Some ("number" | "slider"), (`Number _ as json) ->
               Option.iter (Obj.number_of ctx default_path json) ~f:(fun d ->
                 match range with
                 | [ lo; hi; _ ]
                   when Option.exists lo ~f:(fun lo -> Float.(d < lo))
                        || Option.exists hi ~f:(fun hi -> Float.(d > hi)) ->
                   error ctx default_path "is %g, outside min to max" d
                 | _ -> ())
             | Some ("number" | "slider"), json -> wrong "a number" json
             | Some "toggle", (`True | `False) -> ()
             | Some "toggle", json -> wrong "true or false" json
             | Some "checkbox", `Array items ->
               List.iter
                 (strings ctx (List.mapi items ~f:(fun i j -> nth default_path i, j)))
                 ~f:in_options
             | Some "checkbox", json -> wrong "an array of options" json
             | Some _, _ -> ());
            None)
          : unit option))
     : unit list)
;;

let check json ~resolve =
  let ctx =
    { errors = Queue.create ()
    ; ids = String.Table.create ()
    ; images = Queue.create ()
    ; interactive = Queue.create ()
    ; resolve
    ; count = 0
    }
  in
  let summaries =
    match Obj.create ctx "arguments" ~what:"render_ui" json with
    | None -> []
    | Some o ->
      let summaries =
        match
          Obj.array ~required:true o "components" ~max:max_top_level ~noun:"component"
        with
        | None -> []
        | Some items ->
          List.filter_map items ~f:(fun (i, json) ->
            let path = String.chop_prefix_if_exists i ~prefix:"arguments." in
            component ctx path ~depth:0 json)
      in
      Obj.finish o;
      summaries
  in
  if ctx.count > max_components
  then
    Queue.enqueue
      ctx.errors
      (sprintf
         "components: %d components in all; give at most %d (split it into \
          several calls or show less)"
         ctx.count
         max_components);
  if Queue.is_empty ctx.errors
  then
    Ok
      { Checked.summaries
      ; images = Queue.to_list ctx.images
      ; interactive = Queue.to_list ctx.interactive
      }
  else Error (Queue.to_list ctx.errors)
;;

let ids json =
  let rec of_component (json : Json.t) =
    match json with
    | `Object fields ->
      let own =
        match List.Assoc.find fields "id" ~equal:String.equal with
        | Some (`String id) -> [ id ]
        | _ -> []
      in
      let nested name f =
        match List.Assoc.find fields name ~equal:String.equal with
        | Some (`Array items) -> List.concat_map items ~f
        | _ -> []
      in
      let in_items (item : Json.t) =
        match item with
        | `Object item_fields ->
          (match List.Assoc.find item_fields "children" ~equal:String.equal with
           | Some (`Array children) -> List.concat_map children ~f:of_component
           | _ -> [])
        | _ -> []
      in
      own
      @ nested "children" of_component
      @ nested "tabs" in_items
      @ nested "items" in_items
    | _ -> []
  in
  match json with
  | `Object fields ->
    (match List.Assoc.find fields "components" ~equal:String.equal with
     | Some (`Array components) -> List.concat_map components ~f:of_component
     | _ -> [])
  | _ -> []
;;
