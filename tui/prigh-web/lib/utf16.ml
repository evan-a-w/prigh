open! Core

let byte_offset text ~utf16 =
  let length = String.length text in
  let rec go byte units =
    if units >= utf16 || byte >= length
    then byte
    else (
      let decode = Stdlib.String.get_utf_8_uchar text byte in
      let width = Stdlib.Uchar.utf_decode_length decode in
      let uchar = Stdlib.Uchar.utf_decode_uchar decode in
      go
        (byte + width)
        (units + if Stdlib.Uchar.to_int uchar > 0xFFFF then 2 else 1))
  in
  go 0 0
;;
