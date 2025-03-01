type input =
  { text : string
  ; pos : int
  }

type parser_error = ParserError of int * string
type 'a parser = input -> (input * 'a, parser_error) result

let get_input_hd input =
  if input.pos >= String.length input.text
  then None
  else (
    let c = String.get input.text input.pos in
    Some (c, { input with pos = input.pos + 1 }))
;;

let return x input = Ok (input, x)

let ( >>= ) p f =
  fun input ->
  match p input with
  | Ok (i, x) -> f x i
  | Error e -> Error e
;;

let ( let* ) = ( >>= )
let ( *> ) p1 p2 = p1 >>= fun _ -> p2
let ( <* ) p1 p2 = p1 >>= fun x -> p2 >>= fun _ -> return x

let ( <|> ) p1 p2 =
  fun input ->
  match p1 input with
  | Ok result -> Ok result
  | Error _ -> p2 input
;;

let map f p =
  fun input ->
  match p input with
  | Ok (i, x) -> Ok (i, f x)
  | Error e -> Error e
;;

let ( >|= ) p f = map f p
let ( <*> ) p1 p2 = p1 >>= fun f -> p2 >>= fun x -> return (f x)

let many p =
  let rec loop acc input =
    match p input with
    | Ok (new_input, x) -> loop (x :: acc) new_input
    | Error _ -> Ok (input, List.rev acc)
  in
  loop []
;;

let many1 p =
  let rec loop acc input =
    match p input with
    | Ok (new_input, x) -> loop (x :: acc) new_input
    | Error e when acc = [] -> Error e
    | Error _ -> Ok (input, List.rev acc)
  in
  fun input ->
    match loop [] input with
    | Ok (_, []) -> Error (ParserError (input.pos, "many1: empty result"))
    | res -> res
;;

let satisfy predicate =
  fun input ->
  match get_input_hd input with
  | Some (c, new_input) when predicate c -> Ok (new_input, c)
  | Some (c, _) -> Error (ParserError (input.pos, Printf.sprintf "Unexpected '%c'" c))
  | None -> Error (ParserError (input.pos, "Unexpected EOF"))
;;

let char c = satisfy (fun x -> x = c)

let string s =
  let len = String.length s in
  fun input ->
    if input.pos + len > String.length input.text
    then Error (ParserError (input.pos, Printf.sprintf "Expected '%s'" s))
    else (
      let substr = String.sub input.text input.pos len in
      if substr = s
      then Ok ({ input with pos = input.pos + len }, s)
      else Error (ParserError (input.pos, Printf.sprintf "Expected '%s' got '%s'" s substr)))
;;

let digit = satisfy (fun c -> c >= '0' && c <= '9')
let number = many1 digit >|= fun ds -> ds |> List.to_seq |> String.of_seq |> int_of_string
