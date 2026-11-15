open! Core

let version path =
  String.prefix (Md5.digest_file_blocking path |> Md5.to_hex) 16
;;

let files paths =
  List.map paths ~f:(fun path -> Filename.basename path, path)
  |> List.sort
       ~compare:(Tuple2.compare ~cmp1:String.compare ~cmp2:String.compare)
;;

let json_object pairs =
  List.map pairs ~f:(fun (k, v) -> sprintf "%S: %S" k v)
  |> String.concat ~sep:", "
  |> sprintf "{%s}"
;;

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
  let shell =
    List.map versions ~f:(fun (name, _) -> sprintf "%S" name)
    |> String.concat ~sep:", "
    |> sprintf "[%s]"
  in
  In_channel.read_all template
  |> String.substr_replace_all ~pattern:"@@VERSION@@" ~with_:hash
  |> String.substr_replace_all ~pattern:"@@SHELL@@" ~with_:shell
  |> String.substr_replace_all
       ~pattern:"@@VERSIONS@@"
       ~with_:(json_object versions)
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
          ~with_:(sprintf "%s%s?v=%s%s" quote name v quote)))
;;

let () =
  match Sys.get_argv () |> Array.to_list with
  | _ :: "sw" :: template :: paths -> print_string (sw template paths)
  | _ :: "versioned" :: template :: paths ->
    print_string (versioned template paths)
  | _ -> failwith "usage: site_gen (sw | versioned) TEMPLATE FILE..."
;;
