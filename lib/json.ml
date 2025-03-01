open Parser

type json_value =
  | JsonNull
  | JsonBool of bool
  | JsonNumber of float
  | JsonString of string
  | JsonArray of json_value list
  | JsonObject of (string * json_value) list

let ws_parser = many (satisfy (fun c -> c = ' ' || c = '\n' || c = '\t' || c = '\r')) >|= fun _ -> ()
let json_null = string "null" >|= fun _ -> JsonNull
let json_bool = string "true" >|= (fun _ -> JsonBool true) <|> (string "false" >|= fun _ -> JsonBool false)

let json_number =
  let is_digit c = c >= '0' && c <= '9' in
  let digits = many1 (satisfy is_digit) >|= fun ds -> String.of_seq (List.to_seq ds) in
  let* sign = char '-' >|= (fun _ -> -1.) <|> return 1. in
  let* int_part = digits >|= float_of_string in
  let* frac_part =
    (let* _ = char '.' in
     let* frac = digits in
     return (float_of_string ("0." ^ frac)))
    <|> return 0.
  in
  let* exp_part =
    (let* _ = char 'e' <|> char 'E' in
     let* sign = char '+' >|= (fun _ -> 1) <|> (char '-' >|= fun _ -> -1) <|> return 1 in
     let* exp = digits >|= int_of_string in
     return (10. ** float_of_int (sign * exp)))
    <|> return 1.
  in
  return (JsonNumber (sign *. (int_part +. frac_part) *. exp_part))
;;

let sep_by sep elem =
  fun input ->
  ((let* x = elem in
    let* xs = many (sep *> elem) in
    return (x :: xs))
   <|> return [])
    input
;;

let escape_char =
  let* _ = char '\\' in
  char '"'
  >|= (fun _ -> '"')
  <|> (char '\\' >|= fun _ -> '\\')
  <|> (char '/' >|= fun _ -> '/')
  <|> (char 'b' >|= fun _ -> '\b')
  <|> (char 'f' >|= fun _ -> '\012')
  <|> (char 'n' >|= fun _ -> '\n')
  <|> (char 'r' >|= fun _ -> '\r')
  <|> (char 't' >|= fun _ -> '\t')
;;

let normal_char = satisfy (fun c -> c <> '"' && c <> '\\')

let string_literal =
  let* _ = char '"' in
  let* chars = many (escape_char <|> normal_char) in
  let* _ = char '"' in
  return (String.of_seq (List.to_seq chars))
;;

let json_string = string_literal >|= fun s -> JsonString s

let rec json_value input =
  (ws_parser *> (json_null <|> json_bool <|> json_number <|> json_string <|> json_array <|> json_object) <* ws_parser)
    input

and json_array input =
  (let* _ = char '[' *> ws_parser in
   let* elements = sep_by (ws_parser *> char ',' <* ws_parser) json_value in
   let* _ = ws_parser *> char ']' in
   return (JsonArray elements))
    input

and json_object input =
  (let* _ = char '{' *> ws_parser in
   let pair =
     let* key = string_literal <* ws_parser <* char ':' <* ws_parser in
     let* value = json_value in
     return (key, value)
   in
   let* pairs = sep_by (ws_parser *> char ',' <* ws_parser) pair in
   let* _ = ws_parser *> char '}' in
   return (JsonObject pairs))
    input
;;

let parse_json s =
  match json_value { text = s; pos = 0 } with
  | Ok (_, value) -> Ok value
  | Error e -> Error e
;;

let rec show_json_value = function
  | JsonNull -> "null"
  | JsonBool b -> string_of_bool b
  | JsonNumber n -> string_of_float n
  | JsonString s ->
    let escaped =
      String.concat
        ""
        (List.map
           (function
             | '\n' -> "\\n"
             | c -> String.make 1 c)
           (List.init (String.length s) (String.get s)))
    in
    Printf.sprintf "\"%s\"" escaped
  | JsonArray vs -> "[" ^ String.concat ", " (List.map show_json_value vs) ^ "]"
  | JsonObject pairs ->
    let show_pair (k, v) = Printf.sprintf "\"%s\": %s" k (show_json_value v) in
    "{" ^ String.concat ", " (List.map show_pair pairs) ^ "}"
;;
