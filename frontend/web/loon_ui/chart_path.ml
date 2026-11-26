open! Import

let num = Chart_scale.num
let r = Chart_scale.round4

module Curve = struct
  type t =
    | Linear
    | Natural
    | Monotone
    | Step
    | Step_before
    | Step_after
    | Bump
    | Linear_closed
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Linear -> "linear"
    | Natural -> "natural"
    | Monotone -> "monotone"
    | Step -> "step"
    | Step_before -> "stepBefore"
    | Step_after -> "stepAfter"
    | Bump -> "bump"
    | Linear_closed -> "linearClosed"
  ;;
end

type point = float * float

(* A d3-path context; [flip] swaps x and y (d3's ReflectContext, for
   curveMonotoneY). *)
(* d3-path's [digits(3)], which d3-shape's line and area use. *)
let d3 x = num (Float.round_down ((x *. 1000.) +. 0.5) /. 1000.)

module Ctx = struct
  type t =
    { buf : Buffer.t
    ; flip : bool
    }

  let xy t x y = if t.flip then y, x else x, y

  let move_to t x y =
    let x, y = xy t x y in
    Buffer.add_string t.buf (sprintf "M%s,%s" (d3 x) (d3 y))
  ;;

  let line_to t x y =
    let x, y = xy t x y in
    Buffer.add_string t.buf (sprintf "L%s,%s" (d3 x) (d3 y))
  ;;

  let curve_to t x1 y1 x2 y2 x y =
    let x1, y1 = xy t x1 y1 in
    let x2, y2 = xy t x2 y2 in
    let x, y = xy t x y in
    Buffer.add_string
      t.buf
      (sprintf "C%s,%s,%s,%s,%s,%s" (d3 x1) (d3 y1) (d3 x2) (d3 y2) (d3 x) (d3 y))
  ;;

  let close t = Buffer.add_char t.buf 'Z'
end

(* One run of d3's curve protocol: [line] is d3's [_line] state (0 for an
   area's top, 1 for its base, NaN for a lone line); [t] the step position.
   Returns whether to close. *)
let segment (ctx : Ctx.t) curve ~line ~t (pts : point list) =
  let start x y = if Float.(line = 1.) then Ctx.line_to ctx x y else Ctx.move_to ctx x y in
  let closes n_points = Float.(line = 1.) || (Float.is_nan line && n_points = 1) in
  let pts = Array.of_list pts in
  let n = Array.length pts in
  match (curve : Curve.t) with
  | Linear | Linear_closed ->
    Array.iteri pts ~f:(fun i (x, y) -> if i = 0 then start x y else Ctx.line_to ctx x y);
    let closes =
      match curve with
      | Linear_closed -> n > 0
      | _ -> closes (Int.min n 2)
    in
    if closes then Ctx.close ctx
  | Natural ->
    if n > 0
    then (
      let x0, y0 = pts.(0) in
      start x0 y0;
      if n = 2
      then (
        let x1, y1 = pts.(1) in
        Ctx.line_to ctx x1 y1)
      else if n > 2
      then (
        let control (v : float array) =
          let n = Array.length v - 1 in
          let a = Array.create ~len:n 0. in
          let b = Array.create ~len:n 0. in
          let r = Array.create ~len:n 0. in
          a.(0) <- 0.;
          b.(0) <- 2.;
          r.(0) <- v.(0) +. (2. *. v.(1));
          for i = 1 to n - 2 do
            a.(i) <- 1.;
            b.(i) <- 4.;
            r.(i) <- (4. *. v.(i)) +. (2. *. v.(i + 1))
          done;
          a.(n - 1) <- 2.;
          b.(n - 1) <- 7.;
          r.(n - 1) <- (8. *. v.(n - 1)) +. v.(n);
          for i = 1 to n - 1 do
            let m = a.(i) /. b.(i - 1) in
            b.(i) <- b.(i) -. m;
            r.(i) <- r.(i) -. (m *. r.(i - 1))
          done;
          a.(n - 1) <- r.(n - 1) /. b.(n - 1);
          for i = n - 2 downto 0 do
            a.(i) <- (r.(i) -. a.(i + 1)) /. b.(i)
          done;
          b.(n - 1) <- (v.(n) +. a.(n - 1)) /. 2.;
          for i = 0 to n - 2 do
            b.(i) <- (2. *. v.(i + 1)) -. a.(i + 1)
          done;
          a, b
        in
        let ax, bx = control (Array.map pts ~f:fst) in
        let ay, by = control (Array.map pts ~f:snd) in
        for i = 0 to n - 2 do
          let x, y = pts.(i + 1) in
          Ctx.curve_to ctx ax.(i) ay.(i) bx.(i) by.(i) x y
        done));
    if closes n then Ctx.close ctx
  | Monotone ->
    let sign x = if Float.(x < 0.) then -1. else 1. in
    let x0 = ref Float.nan
    and x1 = ref Float.nan
    and y0 = ref Float.nan
    and y1 = ref Float.nan
    and t0 = ref Float.nan
    and count = ref 0 in
    let div a h other = if Float.(h = 0.) then a /. if Float.(other < 0.) then -0. else 0. else a /. h in
    let slope3 x2 y2 =
      let h0 = !x1 -. !x0
      and h1 = x2 -. !x1 in
      let s0 = div (!y1 -. !y0) h0 h1
      and s1 = div (y2 -. !y1) h1 h0 in
      let p = ((s0 *. h1) +. (s1 *. h0)) /. (h0 +. h1) in
      let v =
        (sign s0 +. sign s1)
        *. Float.min (Float.min (Float.abs s0) (Float.abs s1)) (0.5 *. Float.abs p)
      in
      if Float.is_nan v then 0. else v
    in
    let slope2 t =
      let h = !x1 -. !x0 in
      if Float.(h <> 0.) then ((3. *. (!y1 -. !y0) /. h) -. t) /. 2. else t
    in
    let emit ta tb =
      let dx = (!x1 -. !x0) /. 3. in
      Ctx.curve_to ctx (!x0 +. dx) (!y0 +. (dx *. ta)) (!x1 -. dx) (!y1 -. (dx *. tb)) !x1 !y1
    in
    Array.iter pts ~f:(fun (x, y) ->
      if not (Float.(x = !x1) && Float.(y = !y1))
      then (
        let t1 = ref Float.nan in
        (match !count with
         | 0 ->
           count := 1;
           start x y
         | 1 -> count := 2
         | 2 ->
           count := 3;
           t1 := slope3 x y;
           emit (slope2 !t1) !t1
         | _ ->
           t1 := slope3 x y;
           emit !t0 !t1);
        x0 := !x1;
        x1 := x;
        y0 := !y1;
        y1 := y;
        t0 := !t1));
    (match !count with
     | 2 -> Ctx.line_to ctx !x1 !y1
     | 3 -> emit !t0 (slope2 !t0)
     | _ -> ());
    if closes (Int.min !count 2) then Ctx.close ctx
  | Step | Step_before | Step_after ->
    let px = ref Float.nan
    and py = ref Float.nan
    and count = ref 0 in
    Array.iter pts ~f:(fun (x, y) ->
      (match !count with
       | 0 ->
         count := 1;
         start x y
       | _ ->
         count := 2;
         if Float.(t <= 0.)
         then (
           Ctx.line_to ctx !px y;
           Ctx.line_to ctx x y)
         else (
           let xm = (!px *. (1. -. t)) +. (x *. t) in
           Ctx.line_to ctx xm !py;
           Ctx.line_to ctx xm y));
      px := x;
      py := y);
    if Float.(0. < t && t < 1.) && !count = 2 then Ctx.line_to ctx !px !py;
    if closes !count then Ctx.close ctx
  | Bump ->
    let px = ref Float.nan
    and py = ref Float.nan in
    Array.iteri pts ~f:(fun i (x, y) ->
      if i = 0
      then start x y
      else (
        let xm = (!px +. x) /. 2. in
        Ctx.curve_to ctx xm !py xm y x y);
      px := x;
      py := y);
    if closes (Int.min n 2) then Ctx.close ctx
;;

let step_t : Curve.t -> float = function
  | Step -> 0.5
  | Step_before -> 0.
  | Step_after -> 1.
  | _ -> 0.5
;;

let ctx ~vertical curve =
  { Ctx.buf = Buffer.create 256
  ; flip = vertical && Curve.equal curve Monotone
  }
;;

let flip_points ~vertical curve pts =
  if vertical && Curve.equal curve Monotone then List.map pts ~f:(fun (x, y) -> y, x) else pts
;;

let line ?(vertical = false) curve pts =
  let ctx = ctx ~vertical curve in
  segment ctx curve ~line:Float.nan ~t:(step_t curve) (flip_points ~vertical curve pts);
  Buffer.contents ctx.buf
;;

let area ?(vertical = false) curve ~top ~base =
  let ctx = ctx ~vertical curve in
  let t = step_t curve in
  segment ctx curve ~line:0. ~t (flip_points ~vertical curve top);
  segment ctx curve ~line:1. ~t:(1. -. t) (flip_points ~vertical curve (List.rev base));
  Buffer.contents ctx.buf
;;

let rectangle ?radius ~x ~y ~width ~height () =
  let rw = r width
  and rh = r height in
  let max_radius = Float.min (Float.abs rw /. 2.) (Float.abs rh /. 2.) in
  let ys = if Float.(rh >= 0.) then 1. else -1. in
  let xs = if Float.(rw >= 0.) then 1. else -1. in
  let cw =
    if Float.(rh >= 0. && rw >= 0.) || Float.(rh < 0. && rw < 0.) then "1" else "0"
  in
  let n v = num (r v) in
  let simple () =
    sprintf "M %s,%s h %s v %s h %s Z" (n x) (n y) (n width) (n height) (n (-.width))
  in
  match radius with
  | Some (`Corners (a, b, c, d)) when Float.(max_radius > 0.) ->
    let clamp v = if Float.(v > max_radius) then max_radius else v in
    let a = clamp a
    and b = clamp b
    and c = clamp c
    and d = clamp d in
    let buf = Buffer.create 128 in
    let add = Buffer.add_string buf in
    add (sprintf "M%s,%s" (n x) (n (y +. (ys *. a))));
    if Float.(a > 0.)
    then add (sprintf "A %s,%s,0,0,%s,%s,%s" (n a) (n a) cw (n (x +. (xs *. a))) (n y));
    add (sprintf "L %s,%s" (n (x +. width -. (xs *. b))) (n y));
    if Float.(b > 0.)
    then
      add
        (sprintf "A %s,%s,0,0,%s,\n        %s,%s" (n b) (n b) cw (n (x +. width)) (n (y +. (ys *. b))));
    add (sprintf "L %s,%s" (n (x +. width)) (n (y +. height -. (ys *. c))));
    if Float.(c > 0.)
    then
      add
        (sprintf
           "A %s,%s,0,0,%s,\n        %s,%s"
           (n c)
           (n c)
           cw
           (n (x +. width -. (xs *. c)))
           (n (y +. height)));
    add (sprintf "L %s,%s" (n (x +. (xs *. d))) (n (y +. height)));
    if Float.(d > 0.)
    then
      add
        (sprintf "A %s,%s,0,0,%s,\n        %s,%s" (n d) (n d) cw (n x) (n (y +. height -. (ys *. d))));
    add "Z";
    Buffer.contents buf
  | Some (`All rad) when Float.(max_radius > 0. && rad > 0.) ->
    let k = Float.min max_radius rad in
    sprintf
      "M %s,%s\n\
      \            A %s,%s,0,0,%s,%s,%s\n\
      \            L %s,%s\n\
      \            A %s,%s,0,0,%s,%s,%s\n\
      \            L %s,%s\n\
      \            A %s,%s,0,0,%s,%s,%s\n\
      \            L %s,%s\n\
      \            A %s,%s,0,0,%s,%s,%s Z"
      (n x)
      (n (y +. (ys *. k)))
      (n k)
      (n k)
      cw
      (n (x +. (xs *. k)))
      (n y)
      (n (x +. width -. (xs *. k)))
      (n y)
      (n k)
      (n k)
      cw
      (n (x +. width))
      (n (y +. (ys *. k)))
      (n (x +. width))
      (n (y +. height -. (ys *. k)))
      (n k)
      (n k)
      cw
      (n (x +. width -. (xs *. k)))
      (n (y +. height))
      (n (x +. (xs *. k)))
      (n (y +. height))
      (n k)
      (n k)
      cw
      (n x)
      (n (y +. height -. (ys *. k)))
  | _ -> simple ()
;;

let radian = Float.pi /. 180.

let polar ~cx ~cy ~radius ~angle =
  cx +. (Float.cos (-.radian *. angle) *. radius), cy +. (Float.sin (-.radian *. angle) *. radius)
;;

let sign x = if Float.(x = 0.) then 0. else if Float.(x > 0.) then 1. else -1.

let delta_angle start_angle end_angle =
  sign (end_angle -. start_angle) *. Float.min (Float.abs (end_angle -. start_angle)) 359.999
;;

let n v = num (r v)
let b v = if v then "1" else "0"

let sector_path ~cx ~cy ~inner_radius ~outer_radius ~start_angle ~end_angle =
  let angle = delta_angle start_angle end_angle in
  let temp_end = start_angle +. angle in
  let osx, osy = polar ~cx ~cy ~radius:outer_radius ~angle:start_angle in
  let oex, oey = polar ~cx ~cy ~radius:outer_radius ~angle:temp_end in
  let large = Float.(abs angle > 180.) in
  let path =
    sprintf
      "M %s,%s\n    A %s,%s,0,\n    %s,%s,\n    %s,%s\n  "
      (n osx)
      (n osy)
      (n outer_radius)
      (n outer_radius)
      (b large)
      (b Float.(start_angle > temp_end))
      (n oex)
      (n oey)
  in
  if Float.(inner_radius > 0.)
  then (
    let isx, isy = polar ~cx ~cy ~radius:inner_radius ~angle:start_angle in
    let iex, iey = polar ~cx ~cy ~radius:inner_radius ~angle:temp_end in
    path
    ^ sprintf
        "L %s,%s\n            A %s,%s,0,\n            %s,%s,\n            %s,%s Z"
        (n iex)
        (n iey)
        (n inner_radius)
        (n inner_radius)
        (b large)
        (b Float.(start_angle <= temp_end))
        (n isx)
        (n isy))
  else path ^ sprintf "L %s,%s Z" (n cx) (n cy)
;;

let tangent_circle ~cx ~cy ~radius ~angle ~sign ~is_external ~corner_radius =
  let centre_radius = (corner_radius *. if is_external then 1. else -1.) +. radius in
  let theta = Float.asin (corner_radius /. centre_radius) /. radian in
  let centre_angle = angle +. (sign *. theta) in
  let circle_tangency = polar ~cx ~cy ~radius ~angle:centre_angle in
  let line_tangency =
    polar ~cx ~cy ~radius:(centre_radius *. Float.cos (theta *. radian)) ~angle
  in
  circle_tangency, line_tangency, theta
;;

let sector_with_corner
      ~cx
      ~cy
      ~inner_radius
      ~outer_radius
      ~corner_radius
      ~force_corner_radius
      ~start_angle
      ~end_angle
  =
  let sg = sign (end_angle -. start_angle) in
  let tc = tangent_circle ~cx ~cy ~corner_radius in
  let (socx, socy), (solx, soly), sot =
    tc ~radius:outer_radius ~angle:start_angle ~sign:sg ~is_external:false
  in
  let (eocx, eocy), (eolx, eoly), eot =
    tc ~radius:outer_radius ~angle:end_angle ~sign:(-.sg) ~is_external:false
  in
  let outer_arc = Float.abs (start_angle -. end_angle) -. sot -. eot in
  let neg = b Float.(sg < 0.) in
  if Float.(outer_arc < 0.)
  then
    if force_corner_radius
    then
      sprintf
        "M %s,%s\n        a%s,%s,0,0,1,%s,0\n        a%s,%s,0,0,1,%s,0\n      "
        (n solx)
        (n soly)
        (n corner_radius)
        (n corner_radius)
        (n (corner_radius *. 2.))
        (n corner_radius)
        (n corner_radius)
        (n (-.corner_radius *. 2.))
    else sector_path ~cx ~cy ~inner_radius ~outer_radius ~start_angle ~end_angle
  else (
    let path =
      sprintf
        "M %s,%s\n    A%s,%s,0,0,%s,%s,%s\n    A%s,%s,0,%s,%s,%s,%s\n    A%s,%s,0,0,%s,%s,%s\n  "
        (n solx)
        (n soly)
        (n corner_radius)
        (n corner_radius)
        neg
        (n socx)
        (n socy)
        (n outer_radius)
        (n outer_radius)
        (b Float.(outer_arc > 180.))
        neg
        (n eocx)
        (n eocy)
        (n corner_radius)
        (n corner_radius)
        neg
        (n eolx)
        (n eoly)
    in
    if Float.(inner_radius > 0.)
    then (
      let (sicx, sicy), (silx, sily), sit =
        tc ~radius:inner_radius ~angle:start_angle ~sign:sg ~is_external:true
      in
      let (eicx, eicy), (eilx, eily), eit =
        tc ~radius:inner_radius ~angle:end_angle ~sign:(-.sg) ~is_external:true
      in
      let inner_arc = Float.abs (start_angle -. end_angle) -. sit -. eit in
      if Float.(inner_arc < 0.) && Float.(corner_radius = 0.)
      then path ^ sprintf "L%s,%sZ" (num cx) (num cy)
      else
        path
        ^ sprintf
            "L%s,%s\n      A%s,%s,0,0,%s,%s,%s\n      A%s,%s,0,%s,%s,%s,%s\n      A%s,%s,0,0,%s,%s,%sZ"
            (n eilx)
            (n eily)
            (n corner_radius)
            (n corner_radius)
            neg
            (n eicx)
            (n eicy)
            (n inner_radius)
            (n inner_radius)
            (b Float.(inner_arc > 180.))
            (b Float.(sg > 0.))
            (n sicx)
            (n sicy)
            (n corner_radius)
            (n corner_radius)
            neg
            (n silx)
            (n sily))
    else path ^ sprintf "L%s,%sZ" (n cx) (n cy))
;;

let sector
      ?(corner_radius = 0.)
      ?(force_corner_radius = false)
      ~cx
      ~cy
      ~inner_radius
      ~outer_radius
      ~start_angle
      ~end_angle
      ()
  =
  if Float.(outer_radius < inner_radius || start_angle = end_angle)
  then None
  else (
    let delta = outer_radius -. inner_radius in
    let cr = Float.min corner_radius (delta /. 2.) in
    if Float.(cr > 0. && abs (start_angle -. end_angle) < 360.)
    then
      Some
        (sector_with_corner
           ~cx
           ~cy
           ~inner_radius
           ~outer_radius
           ~corner_radius:cr
           ~force_corner_radius
           ~start_angle
           ~end_angle)
    else Some (sector_path ~cx ~cy ~inner_radius ~outer_radius ~start_angle ~end_angle))
;;

let polygon pts =
  match pts with
  | [] -> ""
  | _ ->
    String.concat
      (List.mapi pts ~f:(fun i (x, y) -> sprintf "%s%s,%s" (if i = 0 then "M" else "L") (n x) (n y)))
    ^ "Z"
;;
