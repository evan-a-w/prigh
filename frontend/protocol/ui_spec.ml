open! Core

(* Tolerant readers: a field of the wrong type is as good as absent. *)
module Read = struct
  let field = Json.field

  let number_of_json : Json.t -> float option = function
    | `Number n -> Float.of_string_opt n
    | `String s -> Float.of_string_opt (String.strip s)
    | _ -> None
  ;;

  let text_of_json : Json.t -> string option = function
    | `String s -> Some s
    | `Number n -> Some n
    | `True -> Some "true"
    | `False -> Some "false"
    | _ -> None
  ;;

  let str j k = Option.bind (field j k) ~f:text_of_json

  let nonempty j k =
    Option.bind (str j k) ~f:(fun s ->
      Option.some_if (not (String.is_empty (String.strip s))) s)
  ;;

  let num j k = Option.bind (field j k) ~f:number_of_json

  let int j k =
    Option.bind (num j k) ~f:(fun f ->
      Option.some_if (Float.is_finite f) (Float.iround_nearest_exn f))
  ;;

  let bool j k =
    match field j k with
    | Some `True -> Some true
    | Some `False -> Some false
    | Some (`String "true") -> Some true
    | Some (`String "false") -> Some false
    | _ -> None
  ;;

  let list j k =
    match field j k with
    | Some (`Array items) -> items
    | _ -> []
  ;;

  let objects j k =
    List.filter (list j k) ~f:(function
      | `Object _ -> true
      | _ -> false)
  ;;

  let strings j k = List.filter_map (list j k) ~f:text_of_json
  let numbers j k = List.filter_map (list j k) ~f:number_of_json
end

open Read

module Tone = struct
  type t =
    | Neutral
    | Info
    | Success
    | Warning
    | Danger
  [@@deriving sexp_of, equal]

  let to_string = function
    | Neutral -> "neutral"
    | Info -> "info"
    | Success -> "success"
    | Warning -> "warning"
    | Danger -> "danger"
  ;;

  let of_json j =
    match str j "tone" with
    | Some "info" -> Info
    | Some "success" -> Success
    | Some "warning" -> Warning
    | Some "danger" -> Danger
    | _ -> Neutral
  ;;
end

module Scalar = struct
  type t =
    | Number of float
    | Text of string
  [@@deriving sexp_of, equal]

  let read j k =
    match field j k with
    | Some (`Number n) ->
      Some
        (match Float.of_string_opt n with
         | Some f -> Number f
         | None -> Text n)
    | Some v -> Option.map (text_of_json v) ~f:(fun s -> Text s)
    | None -> None
  ;;
end

module Cell = struct
  type t =
    | Number of float
    | Text of string
    | Bool of bool
    | Null
  [@@deriving sexp_of, equal]

  let of_json : Json.t -> t = function
    | `Number n ->
      (match Float.of_string_opt n with
       | Some f -> Number f
       | None -> Text n)
    | `String s -> Text s
    | `True -> Bool true
    | `False -> Bool false
    | `Null -> Null
    | other -> Text (Json.to_string other)
  ;;
end

module Table = struct
  module Align = struct
    type t =
      | Left
      | Right
      | Center
    [@@deriving sexp_of, equal]
  end

  module Format = struct
    type t =
      | Text
      | Number
      | Percent
      | Currency
      | Date
      | Md
    [@@deriving sexp_of, equal]
  end

  module Column = struct
    type t =
      { key : string
      ; label : string
      ; align : Align.t
      ; format : Format.t
      ; currency : string
      ; decimals : int option
      }
    [@@deriving sexp_of]
  end

  type t =
    { title : string option
    ; columns : Column.t list
    ; rows : (string * Cell.t) list list
    ; sortable : bool
    ; caption : string option
    }
  [@@deriving sexp_of]

  let cell row key =
    Option.value (List.Assoc.find row ~equal:String.equal key) ~default:Cell.Null
  ;;

  let of_json j =
    let rows =
      List.map (objects j "rows") ~f:(function
        | `Object fields -> List.map fields ~f:(fun (k, v) -> k, Cell.of_json v)
        | _ -> [])
    in
    let column c =
      Option.map
        (Option.first_some (nonempty c "key") (nonempty c "label"))
        ~f:(fun key ->
          let format : Format.t =
            match str c "format" with
            | Some "number" -> Number
            | Some "percent" -> Percent
            | Some "currency" -> Currency
            | Some "date" -> Date
            | Some "md" | Some "markdown" -> Md
            | _ -> Text
          in
          let numeric =
            match format with
            | Number | Percent | Currency -> true
            | Text | Date | Md -> false
          in
          { Column.key
          ; label = Option.value (nonempty c "label") ~default:key
          ; align =
              (match str c "align" with
               | Some "right" -> Right
               | Some "center" -> Center
               | Some "left" -> Left
               | _ -> if numeric then Right else Left)
          ; format
          ; currency =
              Option.value_map (nonempty c "currency") ~default:"USD" ~f:String.uppercase
          ; decimals = int c "decimals"
          })
    in
    let columns =
      match List.filter_map (list j "columns") ~f:column with
      | [] ->
        (* No columns: the keys of the rows, in order of appearance. *)
        List.concat_map rows ~f:(List.map ~f:fst)
        |> List.fold ~init:[] ~f:(fun acc k ->
          if List.mem acc k ~equal:String.equal then acc else k :: acc)
        |> List.rev
        |> List.map ~f:(fun key ->
          { Column.key
          ; label = key
          ; align = Left
          ; format = Text
          ; currency = "USD"
          ; decimals = None
          })
      | columns -> columns
    in
    { title = nonempty j "title"
    ; columns
    ; rows
    ; sortable = Option.value (bool j "sortable") ~default:true
    ; caption = nonempty j "caption"
    }
  ;;
end

module Chart = struct
  module Kind = struct
    type t =
      | Line
      | Area
      | Bar
      | Stacked_bar
      | Horizontal_bar
      | Pie
      | Donut
      | Scatter
    [@@deriving sexp_of, equal, enumerate]

    let to_string = function
      | Line -> "line"
      | Area -> "area"
      | Bar -> "bar"
      | Stacked_bar -> "stacked_bar"
      | Horizontal_bar -> "horizontal_bar"
      | Pie -> "pie"
      | Donut -> "donut"
      | Scatter -> "scatter"
    ;;

    let of_string s =
      List.find all ~f:(fun k -> String.equal (to_string k) s)
    ;;
  end

  module Height = struct
    type t =
      | Small
      | Medium
      | Large
    [@@deriving sexp_of, equal]
  end

  module Series = struct
    type t =
      { name : string
      ; values : float option list
      }
    [@@deriving sexp_of]
  end

  module Points = struct
    type t =
      { name : string
      ; points : (float * float) list
      }
    [@@deriving sexp_of]
  end

  module Slice = struct
    type t =
      { label : string
      ; value : float
      }
    [@@deriving sexp_of]
  end

  module Data = struct
    type t =
      | Categories of
          { labels : string list
          ; series : Series.t list
          }
      | Slices of Slice.t list
      | Points of Points.t list
    [@@deriving sexp_of]
  end

  type t =
    { kind : Kind.t
    ; title : string option
    ; subtitle : string option
    ; unit : string option
    ; x_label : string option
    ; y_label : string option
    ; height : Height.t
    ; data : Data.t
    }
  [@@deriving sexp_of]

  let name j i =
    Option.value (nonempty j "name") ~default:(sprintf "Series %d" (i + 1))
  ;;

  let of_json j =
    let kind =
      Option.bind (str j "kind") ~f:Kind.of_string
      |> Option.value ~default:Kind.Line
    in
    let data : Data.t =
      match kind with
      | Pie | Donut ->
        Slices
          (List.filter_map (objects j "slices") ~f:(fun s ->
             Option.map (num s "value") ~f:(fun value ->
               { Slice.label = Option.value (str s "label") ~default:""
               ; value = Float.max 0. value
               })))
      | Scatter ->
        Points
          (List.mapi (objects j "series") ~f:(fun i s ->
             { Points.name = name s i
             ; points =
                 List.filter_map (list s "points") ~f:(function
                   | `Array [ x; y ] ->
                     (match number_of_json x, number_of_json y with
                      | Some x, Some y -> Some (x, y)
                      | _ -> None)
                   | `Object _ as p ->
                     (match num p "x", num p "y" with
                      | Some x, Some y -> Some (x, y)
                      | _ -> None)
                   | _ -> None)
             }))
      | Line | Area | Bar | Stacked_bar | Horizontal_bar ->
        let labels = strings j "labels" in
        let series =
          List.mapi (objects j "series") ~f:(fun i s ->
            let values = List.map (list s "values") ~f:number_of_json in
            (* One value per label: missing ones are gaps. *)
            let n = List.length labels in
            let values =
              if n = 0
              then values
              else
                List.init n ~f:(fun k -> Option.join (List.nth values k))
            in
            { Series.name = name s i; values })
        in
        let labels =
          match labels with
          | [] ->
            let n =
              List.fold series ~init:0 ~f:(fun n s ->
                Int.max n (List.length s.values))
            in
            List.init n ~f:(fun i -> Int.to_string (i + 1))
          | labels -> labels
        in
        Categories { labels; series }
    in
    { kind
    ; title = nonempty j "title"
    ; subtitle = nonempty j "subtitle"
    ; unit = nonempty j "unit"
    ; x_label = nonempty j "x_label"
    ; y_label = nonempty j "y_label"
    ; height =
        (match str j "height" with
         | Some "small" -> Small
         | Some "large" -> Large
         | _ -> Medium)
    ; data
    }
  ;;

  let detail t =
    let n count one = sprintf "%d %s%s" count one (if count = 1 then "" else "s") in
    match t.data with
    | Categories { series = [ _ ]; labels } -> n (List.length labels) "point"
    | Categories { series; _ } -> sprintf "%d series" (List.length series)
    | Slices slices -> n (List.length slices) "slice"
    | Points [ p ] -> n (List.length p.points) "point"
    | Points series -> sprintf "%d series" (List.length series)
  ;;
end

module Calculator = struct
  module Input = struct
    type t =
      { name : string
      ; label : string
      ; default : float
      ; min : float option
      ; max : float option
      ; step : float option
      ; unit : string option
      ; slider : bool
      }
    [@@deriving sexp_of]
  end

  module Format = struct
    type t =
      | Number
      | Percent
      | Currency
      | Integer
    [@@deriving sexp_of, equal]
  end

  module Output = struct
    type t =
      { label : string
      ; expr : string
      ; format : Format.t
      ; decimals : int option
      ; unit : string option
      }
    [@@deriving sexp_of]
  end

  type t =
    { title : string option
    ; inputs : Input.t list
    ; outputs : Output.t list
    }
  [@@deriving sexp_of]

  let of_json j =
    { title = nonempty j "title"
    ; inputs =
        List.filter_map (objects j "inputs") ~f:(fun i ->
          Option.map (nonempty i "name") ~f:(fun name ->
            { Input.name
            ; label = Option.value (nonempty i "label") ~default:name
            ; default = Option.value (num i "default") ~default:0.
            ; min = num i "min"
            ; max = num i "max"
            ; step = num i "step"
            ; unit = nonempty i "unit"
            ; slider = Option.value (bool i "slider") ~default:false
            }))
    ; outputs =
        List.filter_map (objects j "outputs") ~f:(fun o ->
          Option.map (nonempty o "expr") ~f:(fun expr ->
            { Output.label = Option.value (nonempty o "label") ~default:expr
            ; expr
            ; format =
                (match str o "format" with
                 | Some "percent" -> Percent
                 | Some "currency" -> Currency
                 | Some "integer" -> Integer
                 | _ -> Number)
            ; decimals = int o "decimals"
            ; unit = nonempty o "unit"
            }))
    }
  ;;
end

module Field = struct
  module Kind = struct
    type t =
      | Text
      | Textarea
      | Number
      | Select
      | Radio
      | Checkbox
      | Toggle
      | Slider
      | Date
    [@@deriving sexp_of, equal]
  end

  type t =
    { name : string
    ; label : string
    ; kind : Kind.t
    ; options : string list
    ; default : Json.t option
    ; min : float option
    ; max : float option
    ; step : float option
    ; required : bool
    ; placeholder : string option
    ; help : string option
    }
  [@@deriving sexp_of]

  let of_json j =
    Option.map (nonempty j "name") ~f:(fun name ->
      let options = strings j "options" in
      let kind : Kind.t =
        match str j "kind", options with
        | Some "textarea", _ -> Textarea
        | Some "number", _ -> Number
        | Some ("select" | "radio" | "checkbox"), [] -> Text
        | Some "select", _ -> Select
        | Some "radio", _ -> Radio
        | Some "checkbox", _ -> Checkbox
        | Some "toggle", _ -> Toggle
        | Some "slider", _ -> Slider
        | Some "date", _ -> Date
        | _ -> Text
      in
      { name
      ; label = Option.value (nonempty j "label") ~default:name
      ; kind
      ; options
      ; default = field j "default"
      ; min = num j "min"
      ; max = num j "max"
      ; step = num j "step"
      ; required = Option.value (bool j "required") ~default:false
      ; placeholder = nonempty j "placeholder"
      ; help = nonempty j "help"
      })
  ;;
end

module Item = struct
  type t =
    { title : string
    ; time : string option
    ; text : string option
    ; tone : Tone.t
    }
  [@@deriving sexp_of]

  let of_json j =
    { title = Option.value (str j "title") ~default:""
    ; time = nonempty j "time"
    ; text = nonempty j "text"
    ; tone = Tone.of_json j
    }
  ;;
end

module Step_status = struct
  type t =
    | Done
    | Current
    | Todo
    | Failed
  [@@deriving sexp_of, equal]
end

module Image_width = struct
  type t =
    | Small
    | Medium
    | Full
  [@@deriving sexp_of, equal]
end

module Image = struct
  type t =
    { path : string
    ; caption : string option
    ; alt : string option
    }
  [@@deriving sexp_of]

  let of_json j =
    Option.map (nonempty j "path") ~f:(fun path ->
      { path; caption = nonempty j "caption"; alt = nonempty j "alt" })
  ;;
end

module Choice = struct
  type t =
    { label : string
    ; value : string
    ; description : string option
    ; tone : Tone.t
    }
  [@@deriving sexp_of]
end

module Question = struct
  type t =
    { question : string
    ; options : string list
    ; answer : int
    ; explanation : string option
    }
  [@@deriving sexp_of]
end

module Trend = struct
  type t =
    | Up
    | Down
    | Flat
  [@@deriving sexp_of, equal]

  let read j k : t option =
    match str j k with
    | Some "up" -> Some Up
    | Some "down" -> Some Down
    | Some "flat" -> Some Flat
    | _ -> None
  ;;
end

module Component = struct
  type t =
    { id : string option
    ; body : body
    }

  and body =
    | Card of
        { title : string option
        ; subtitle : string option
        ; children : t list
        }
    | Grid of
        { columns : int
        ; children : t list
        }
    | Tabs of (string * t list) list
    | Accordion of { items : accordion_item list }
    | Divider
    | Markdown of string
    | Callout of
        { tone : Tone.t
        ; title : string option
        ; text : string
        }
    | Stat of
        { label : string
        ; value : Scalar.t
        ; unit : string option
        ; delta : Scalar.t option
        ; trend : Trend.t option
        ; good : Trend.t option
        ; caption : string option
        ; spark : float list
        }
    | Kv of
        { title : string option
        ; items : (string * string) list
        }
    | Progress of
        { label : string option
        ; value : float
        ; max : float
        ; tone : Tone.t
        ; caption : string option
        }
    | Badges of (string * Tone.t) list
    | Table of Table.t
    | Chart of Chart.t
    | Timeline of
        { title : string option
        ; items : Item.t list
        }
    | Steps of
        { title : string option
        ; items : (Item.t * Step_status.t) list
        }
    | Code of
        { lang : string option
        ; title : string option
        ; code : string
        }
    | Diff of
        { title : string option
        ; diff : string
        }
    | Tree of
        { title : string option
        ; entries : (string * string option * Tone.t) list
        }
    | Image of
        { image : Image.t
        ; width : Image_width.t
        }
    | Gallery of
        { title : string option
        ; images : Image.t list
        }
    | Calculator of Calculator.t
    | Flashcards of
        { title : string option
        ; cards : (string * string) list
        }
    | Quiz of
        { title : string option
        ; questions : Question.t list
        }
    | Form of
        { title : string option
        ; description : string option
        ; fields : Field.t list
        ; submit : string
        }
    | Choices of
        { prompt : string option
        ; options : Choice.t list
        ; multiple : bool
        }
    | Checklist of
        { title : string option
        ; items : (string * bool) list
        ; submit : string option
        }
    | Unknown of
        { type_ : string
        ; raw : Json.t
        }

  and accordion_item =
    { title : string
    ; children : t list
    ; open_ : bool
    }
  [@@deriving sexp_of]

  let type_name t =
    match t.body with
    | Card _ -> "card"
    | Grid _ -> "grid"
    | Tabs _ -> "tabs"
    | Accordion _ -> "accordion"
    | Divider -> "divider"
    | Markdown _ -> "markdown"
    | Callout _ -> "callout"
    | Stat _ -> "stat"
    | Kv _ -> "kv"
    | Progress _ -> "progress"
    | Badges _ -> "badges"
    | Table _ -> "table"
    | Chart _ -> "chart"
    | Timeline _ -> "timeline"
    | Steps _ -> "steps"
    | Code _ -> "code"
    | Diff _ -> "diff"
    | Tree _ -> "tree"
    | Image _ -> "image"
    | Gallery _ -> "gallery"
    | Calculator _ -> "calculator"
    | Flashcards _ -> "flashcards"
    | Quiz _ -> "quiz"
    | Form _ -> "form"
    | Choices _ -> "choices"
    | Checklist _ -> "checklist"
    | Unknown { type_; _ } -> type_
  ;;

  let title t =
    match t.body with
    | Card { title; _ }
    | Kv { title; _ }
    | Timeline { title; _ }
    | Steps { title; _ }
    | Code { title; _ }
    | Diff { title; _ }
    | Tree { title; _ }
    | Gallery { title; _ }
    | Flashcards { title; _ }
    | Quiz { title; _ }
    | Form { title; _ }
    | Checklist { title; _ } -> Option.first_some title t.id
    | Table { title; _ } | Chart { title; _ } | Calculator { title; _ } ->
      Option.first_some title t.id
    | Callout { title; _ } -> Option.first_some title t.id
    | Progress { label; _ } -> Option.first_some label t.id
    | Stat { label; _ } -> Some label
    | Choices { prompt; _ } -> Option.first_some prompt t.id
    | Grid _ | Tabs _ | Accordion _ | Divider | Markdown _ | Badges _
    | Image _ | Unknown _ -> t.id
  ;;

  let children t =
    match t.body with
    | Card { children; _ } | Grid { children; _ } -> children
    | Tabs tabs -> List.concat_map tabs ~f:snd
    | Accordion { items } -> List.concat_map items ~f:(fun i -> i.children)
    | _ -> []
  ;;

  let rec of_json (j : Json.t) : t =
    let id = nonempty j "id" in
    let children j = List.map (objects j "children") ~f:of_json in
    let type_ = Option.value (str j "type") ~default:"" in
    let body =
      match type_ with
      | "card" ->
        Card
          { title = nonempty j "title"
          ; subtitle = nonempty j "subtitle"
          ; children = children j
          }
      | "grid" ->
        Grid
          { columns =
              Int.clamp_exn (Option.value (int j "columns") ~default:2) ~min:1 ~max:4
          ; children = children j
          }
      | "tabs" ->
        Tabs
          (List.mapi (objects j "tabs") ~f:(fun i tab ->
             ( Option.value (nonempty tab "label") ~default:(sprintf "Tab %d" (i + 1))
             , children tab )))
      | "accordion" ->
        Accordion
          { items =
              List.map (objects j "items") ~f:(fun item ->
                { title = Option.value (str item "title") ~default:""
                ; children = children item
                ; open_ = Option.value (bool item "open") ~default:false
                })
          }
      | "divider" -> Divider
      | "markdown" -> Markdown (Option.value (str j "text") ~default:"")
      | "callout" ->
        Callout
          { tone = Tone.of_json j
          ; title = nonempty j "title"
          ; text = Option.value (str j "text") ~default:""
          }
      | "stat" ->
        Stat
          { label = Option.value (str j "label") ~default:""
          ; value = Option.value (Scalar.read j "value") ~default:(Text "—")
          ; unit = nonempty j "unit"
          ; delta = Scalar.read j "delta"
          ; trend = Trend.read j "trend"
          ; good = Trend.read j "good"
          ; caption = nonempty j "caption"
          ; spark = numbers j "spark"
          }
      | "kv" ->
        Kv
          { title = nonempty j "title"
          ; items =
              List.filter_map (objects j "items") ~f:(fun i ->
                Option.map (str i "key") ~f:(fun key ->
                  key, Option.value (str i "value") ~default:""))
          }
      | "progress" ->
        Progress
          { label = nonempty j "label"
          ; value = Option.value (num j "value") ~default:0.
          ; max =
              (match num j "max" with
               | Some m when Float.(m > 0.) -> m
               | _ -> 100.)
          ; tone = Tone.of_json j
          ; caption = nonempty j "caption"
          }
      | "badges" ->
        Badges
          (List.filter_map (list j "items") ~f:(function
             | `String s -> Some (s, Tone.Neutral)
             | `Object _ as b ->
               Option.map (str b "text") ~f:(fun text -> text, Tone.of_json b)
             | _ -> None))
      | "table" -> Table (Table.of_json j)
      | "chart" -> Chart (Chart.of_json j)
      | "timeline" ->
        Timeline
          { title = nonempty j "title"
          ; items = List.map (objects j "items") ~f:Item.of_json
          }
      | "steps" ->
        Steps
          { title = nonempty j "title"
          ; items =
              List.map (objects j "items") ~f:(fun i ->
                ( Item.of_json i
                , match str i "status" with
                  | Some "done" -> Step_status.Done
                  | Some "current" -> Current
                  | Some "failed" -> Failed
                  | _ -> Todo ))
          }
      | "code" ->
        Code
          { lang = nonempty j "lang"
          ; title = nonempty j "title"
          ; code = Option.value (str j "code") ~default:""
          }
      | "diff" ->
        Diff
          { title = nonempty j "title"
          ; diff = Option.value (str j "diff") ~default:""
          }
      | "tree" ->
        Tree
          { title = nonempty j "title"
          ; entries =
              List.filter_map (list j "entries") ~f:(function
                | `String path -> Some (path, None, Tone.Neutral)
                | `Object _ as e ->
                  Option.map (nonempty e "path") ~f:(fun path ->
                    path, nonempty e "note", Tone.of_json e)
                | _ -> None)
          }
      | "image" ->
        (match Image.of_json j with
         | Some image ->
           Image
             { image
             ; width =
                 (match str j "width" with
                  | Some "small" -> Small
                  | Some "full" -> Full
                  | _ -> Medium)
             }
         | None -> Unknown { type_; raw = j })
      | "gallery" ->
        Gallery
          { title = nonempty j "title"
          ; images = List.filter_map (objects j "images") ~f:Image.of_json
          }
      | "calculator" -> Calculator (Calculator.of_json j)
      | "flashcards" ->
        Flashcards
          { title = nonempty j "title"
          ; cards =
              List.map (objects j "cards") ~f:(fun c ->
                ( Option.value (str c "front") ~default:""
                , Option.value (str c "back") ~default:"" ))
          }
      | "quiz" ->
        Quiz
          { title = nonempty j "title"
          ; questions =
              List.filter_map (objects j "questions") ~f:(fun q ->
                let options = strings q "options" in
                Option.some_if
                  (not (List.is_empty options))
                  { Question.question = Option.value (str q "question") ~default:""
                  ; options
                  ; answer = Option.value (int q "answer") ~default:0
                  ; explanation = nonempty q "explanation"
                  })
          }
      | "form" ->
        Form
          { title = nonempty j "title"
          ; description = nonempty j "description"
          ; fields = List.filter_map (objects j "fields") ~f:Field.of_json
          ; submit = Option.value (nonempty j "submit") ~default:"Send"
          }
      | "choices" ->
        Choices
          { prompt = nonempty j "prompt"
          ; options =
              List.filter_map (list j "options") ~f:(function
                | `String label ->
                  Some
                    { Choice.label; value = label; description = None; tone = Neutral }
                | `Object _ as o ->
                  Option.map (nonempty o "label") ~f:(fun label ->
                    { Choice.label
                    ; value = Option.value (str o "value") ~default:label
                    ; description = nonempty o "description"
                    ; tone = Tone.of_json o
                    })
                | _ -> None)
          ; multiple = Option.value (bool j "multiple") ~default:false
          }
      | "checklist" ->
        Checklist
          { title = nonempty j "title"
          ; items =
              List.filter_map (list j "items") ~f:(function
                | `String text -> Some (text, false)
                | `Object _ as i ->
                  Option.map (str i "text") ~f:(fun text ->
                    text, Option.value (bool i "checked") ~default:false)
                | _ -> None)
          ; submit = nonempty j "submit"
          }
      | _ -> Unknown { type_; raw = j }
    in
    { id; body }
  ;;
end

type t = Component.t list [@@deriving sexp_of]

let of_json j =
  match Json.field j "components" with
  | Some (`Array items) -> Ok (List.map items ~f:Component.of_json)
  | Some _ -> Or_error.error_string "components: expected a list"
  | None -> Or_error.error_string "missing field \"components\""
;;

let of_arguments s =
  match Json.parse s with
  | Ok j -> Result.ok (of_json j)
  | Error _ -> None
;;

let first_line s =
  match String.split_lines (String.strip s) with
  | line :: _ -> line
  | [] -> ""
;;

let shorten s =
  let s = first_line s in
  if String.length s <= 60 then s else String.prefix s 59 ^ "…"
;;

let plural n word = sprintf "%d %s%s" n word (if n = 1 then "" else "s")

let rec walk_list prefix components =
  List.concat_mapi components ~f:(fun i (c : Component.t) ->
    let path = if String.is_empty prefix then Int.to_string i else sprintf "%s.%d" prefix i in
    (path, c)
    ::
    (match c.body with
     | Card { children; _ } | Grid { children; _ } -> walk_list path children
     | Tabs tabs ->
       List.concat_mapi tabs ~f:(fun k (_, children) ->
         walk_list (sprintf "%s.%d" path k) children)
     | Accordion { items } ->
       List.concat_mapi items ~f:(fun k (item : Component.accordion_item) ->
         walk_list (sprintf "%s.%d" path k) item.children)
     | _ -> []))
;;

let walk t = walk_list "" t

let image_paths t =
  List.concat_map (walk t) ~f:(fun (_, (c : Component.t)) ->
    match c.body with
    | Image { image; _ } -> [ image.path ]
    | Gallery { images; _ } -> List.map images ~f:(fun (i : Image.t) -> i.path)
    | _ -> [])
;;

let line (c : Component.t) =
  let name = Component.type_name c in
  let name = if String.is_empty name then "component" else name in
  let what, detail =
    match c.body with
    | Image { image; _ } -> Some image.path, None
    | Markdown text -> Some (shorten text), None
    | Callout { title = None; text; _ } -> Some (shorten text), None
    | Stat { label; value; _ } ->
      ( Some label
      , Some
          (match value with
           | Text s -> s
           | Number f -> Float.to_string_hum ~strip_zero:true ~decimals:4 f) )
    | Table t -> Component.title c, Some (plural (List.length t.rows) "row")
    | Chart chart -> Component.title c, Some (Chart.detail chart)
    | Card { children; _ } -> Component.title c, Some (plural (List.length children) "item")
    | Grid { children; _ } -> None, Some (plural (List.length children) "item")
    | Tabs tabs -> None, Some (String.concat ~sep:", " (List.map tabs ~f:fst))
    | Accordion { items } -> None, Some (plural (List.length items) "section")
    | Timeline { items; _ } -> Component.title c, Some (plural (List.length items) "event")
    | Steps { items; _ } -> Component.title c, Some (plural (List.length items) "step")
    | Tree { entries; _ } -> Component.title c, Some (plural (List.length entries) "entry")
    | Gallery { images; _ } -> Component.title c, Some (plural (List.length images) "image")
    | Flashcards { cards; _ } -> Component.title c, Some (plural (List.length cards) "card")
    | Quiz { questions; _ } -> Component.title c, Some (plural (List.length questions) "question")
    | Choices { options; _ } -> Component.title c, Some (plural (List.length options) "option")
    | Checklist { items; _ } -> Component.title c, Some (plural (List.length items) "item")
    | Code { lang; _ } -> Component.title c, lang
    | _ -> Component.title c, None
  in
  let what = Option.map what ~f:shorten in
  let what =
    Option.filter what ~f:(fun s -> not (String.is_empty (String.strip s)))
  in
  match what, detail with
  | Some w, Some d -> sprintf "%s: %s · %s" name w d
  | Some w, None -> sprintf "%s: %s" name w
  | None, Some d -> sprintf "%s · %s" name d
  | None, None -> name
;;

let summary t = List.map t ~f:line
