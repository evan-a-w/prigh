open! Core
open! Prigh

(* ACP content blocks as prigh prompts. *)

let show blocks =
  match Acp_prompt.of_blocks (List.map blocks ~f:Jsonaf.of_string) with
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
  | Ok p ->
    print_endline p.text;
    print_s
      [%sexp
        { attachments = (p.attachments : string list)
        ; images = (List.length p.images : int)
        }];
    print_endline "--"
;;

let%expect_test "selections, encoded paths, other URIs, blobs" =
  show
    [ {|{"type": "text", "text": "see"}|}
    ; {|{"type": "resource_link", "name": "x", "uri": "file:///w/my%20dir/x.ml#L7"}|}
    ; {|{"type": "resource_link", "name": "x", "uri": "file:///w/my%20dir/x.ml#L7-L9"}|}
    ; {|{"type": "resource_link", "name": "y", "uri": "/w/y.ml"}|}
    ];
  show
    [ {|{"type": "text", "text": "and "}|}
    ; {|{"type": "resource", "resource": {"uri": "zed://notes/1", "text": "a note"}}|}
    ; {|{"type": "resource", "resource": {"uri": "file:///w/logo.svg", "blob": "PHN2Zy8+", "mimeType": "application/octet-stream"}}|}
    ; {|{"type": "resource", "resource": {"uri": "file:///w/p.png", "blob": "iVBO", "mimeType": "image/png"}}|}
    ];
  show [ {|{"type": "video"}|} ];
  show [ {|{"text": "no type"}|} ];
  show [ {|{"type": "image", "data": "x"}|} ];
  show [ {|{"type": "resource", "resource": {"uri": "file:///w/a"}}|} ];
  [%expect
    {|
    see @/w/my dir/x.ml (line 7) @/w/my dir/x.ml (lines 7-9) @/w/y.ml

    (The line ranges are the user's selection.)
    ((attachments ("/w/my dir/x.ml" /w/y.ml)) (images 0))
    --
    and [zed://notes/1](zed://notes/1) @/w/logo.svg (a binary resource of type application/octet-stream, not included) @/w/p.png

    <file path="zed://notes/1">
    a note
    </file>
    ((attachments ()) (images 1))
    --
    error: prompt block 0: unknown content type "video" (prigh takes text, image, resource_link and resource)
    error: prompt block 0: missing type
    error: prompt block 0: an image needs data and mimeType
    error: prompt block 0: a resource needs text or blob
    |}]
;;
