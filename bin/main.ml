open Lab4_parsec.Basic

let () =
  let test = "1 + 2 * (3 + 4)" in
  match parse_expr test with
  | Ok expr ->
    let rec to_string = function
      | Num n -> string_of_int n
      | Add (a, b) -> "(" ^ to_string a ^ " + " ^ to_string b ^ ")"
      | Mul (a, b) -> "(" ^ to_string a ^ " * " ^ to_string b ^ ")"
    in
    print_endline (to_string expr)
  | Error (ParserError (pos, msg)) -> Printf.printf "Error at position %d: %s\n" pos msg
;;
