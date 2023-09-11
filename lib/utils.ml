let rec pad_string_to_len str len padder =
  if String.length str >= len then str
  else pad_string_to_len (padder ^ str) len padder

let to_string_padded int padding =
  pad_string_to_len (Int.to_string int) padding "0"
