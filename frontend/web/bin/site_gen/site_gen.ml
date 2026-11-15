open! Core

let version path =
  String.prefix (Md5.digest_file_blocking path |> Md5.to_hex) 16
;;

let files paths =
  List.map paths ~f:(fun path -> Filename.basename path, path)
  |> List.sort
       ~compare:(Tuple2.compare ~cmp1:String.compare ~cmp2:String.compare)
;;

let versioned_name ~name ~version = sprintf "%s?v=%s" name version

let sw template paths =
  let versions =
    List.map (files paths) ~f:(fun (name, path) -> name, version path)
  in
  let hash =
    List.concat_map versions ~f:(fun (name, version) -> [ name; version ])
    |> String.concat ~sep:"\n"
    |> Md5.digest_string
    |> Md5.to_hex
    |> fun hex -> String.prefix hex 16
  in
  (* Each file as the page asks for it: by its versioned name where a file
     of the shell (index.html, app.css) names it so. *)
  let contents =
    List.map (files paths) ~f:(fun (_, path) -> In_channel.read_all path)
  in
  let shell =
    List.map versions ~f:(fun (name, version) ->
      let versioned = versioned_name ~name ~version in
      sprintf
        "%S"
        (if
           List.exists contents ~f:(fun text ->
             String.is_substring text ~substring:versioned)
         then versioned
         else name))
    |> String.concat ~sep:", "
    |> sprintf "[%s]"
  in
  In_channel.read_all template
  |> String.substr_replace_all ~pattern:"@@VERSION@@" ~with_:hash
  |> String.substr_replace_all ~pattern:"@@SHELL@@" ~with_:shell
;;

let versioned template paths =
  List.fold
    (files paths)
    ~init:(In_channel.read_all template)
    ~f:(fun text (name, path) ->
      let v = version path in
      List.fold [ "\""; "'" ] ~init:text ~f:(fun text quote ->
        String.substr_replace_all
          text
          ~pattern:(quote ^ name ^ quote)
          ~with_:(quote ^ versioned_name ~name ~version:v ^ quote)))
;;

let () =
  match Sys.get_argv () |> Array.to_list with
  | _ :: "sw" :: template :: paths -> print_string (sw template paths)
  | _ :: "versioned" :: template :: paths ->
    print_string (versioned template paths)
  | _ -> failwith "usage: site_gen (sw | versioned) TEMPLATE FILE..."
;;
