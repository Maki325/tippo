let pad_string_to_len str len padder =
  let str_len = String.length str in
  if str_len >= len then str else String.make (len - str_len) padder ^ str

let to_string_padded int padding =
  pad_string_to_len (Int.to_string int) padding '0'
