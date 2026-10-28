open! Core

let () =
  match Sys.get_argv () |> Array.to_list with
  | [] | [ _ ] -> failwith "usage: sw_gen TEMPLATE FILE..."
  | _ :: template :: files ->
    let files =
      List.map files ~f:(fun path -> Filename.basename path, path)
      |> List.sort
           ~compare:(Tuple2.compare ~cmp1:String.compare ~cmp2:String.compare)
    in
    let version =
      List.concat_map files ~f:(fun (name, path) ->
        [ name; Md5.digest_file_blocking path |> Md5.to_hex ])
      |> String.concat ~sep:"\n"
      |> Md5.digest_string
      |> Md5.to_hex
      |> fun hex -> String.prefix hex 16
    in
    let shell =
      List.map files ~f:(fun (name, _) -> sprintf "%S" name)
      |> String.concat ~sep:", "
      |> sprintf "[%s]"
    in
    In_channel.read_all template
    |> String.substr_replace_all ~pattern:"%%VERSION%%" ~with_:version
    |> String.substr_replace_all ~pattern:"%%SHELL%%" ~with_:shell
    |> print_string
;;
