open Parser

type expr =
  | Num of int
  | Add of expr * expr
  | Mul of expr * expr

let spaces = many (satisfy (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r'))
let token p = p <* spaces
let lparen = token (char '(')
let rparen = token (char ')')
let plus = token (char '+') *> return (fun x y -> Add (x, y))
let mul = token (char '*') *> return (fun x y -> Mul (x, y))
let number = token (many1 digit >|= fun ds -> List.to_seq ds |> String.of_seq |> int_of_string)
let pair p1 p2 = p1 >>= fun a -> p2 >>= fun b -> return (a, b)

let rec expr input =
  (let* t = term in
   let* fs = many (pair plus term) in
   let result = List.fold_left (fun acc (f, term) -> f acc term) t fs in
   return result)
    input

and term input =
  (let* f = factor in
   let* fs = many (pair mul factor) in
   let result = List.fold_left (fun acc (f, factor) -> f acc factor) f fs in
   return result)
    input

and factor input = (number >|= (fun n -> Num n) <|> (lparen *> expr <* rparen)) input

let parse_expr str =
  let input = { text = str; pos = 0 } in
  match expr input with
  | Ok (remaining, res) ->
    if remaining.pos = String.length remaining.text
    then Ok res
    else Error (ParserError (remaining.pos, "Unexpected trailing characters"))
  | Error e -> Error e
;;
