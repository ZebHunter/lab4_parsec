open OUnit2
open Lab4_parsec.Basic
open Lab4_parsec.Parser

let test_char_parser _ =
  let p = char 'a' in
  assert_equal (Ok ({ text = "a"; pos = 1 }, 'a')) (p { text = "a"; pos = 0 });
  assert_equal (Error (ParserError (0, "Unexpected 'b'"))) (p { text = "b"; pos = 0 })
;;

let test_string_parser _ =
  let p = string "test" in
  assert_equal (Ok ({ text = "test"; pos = 4 }, "test")) (p { text = "test"; pos = 0 });
  assert_equal (Error (ParserError (0, "Expected 'test' got 'tesx'"))) (p { text = "tesx"; pos = 0 })
;;

let test_digit_parser _ =
  let p = digit in
  assert_equal (Ok ({ text = "5"; pos = 1 }, '5')) (p { text = "5"; pos = 0 });
  assert_equal (Error (ParserError (0, "Unexpected 'a'"))) (p { text = "a"; pos = 0 })
;;

let test_simple_expr _ =
  let res = parse_expr "1 + 2" in
  match res with
  | Ok (Add (Num 1, Num 2)) -> assert true
  | _ -> assert false
;;

let test_priority _ =
  let res = parse_expr "2 + 3 * 4" in
  match res with
  | Ok (Add (Num 2, Mul (Num 3, Num 4))) -> assert true
  | _ -> assert false
;;

let test_parentheses _ =
  let res = parse_expr "(2 + 3) * 4" in
  match res with
  | Ok (Mul (Add (Num 2, Num 3), Num 4)) -> assert true
  | _ -> assert false
;;

let suite =
  "Parser Tests"
  >::: [ "Char Parser" >:: test_char_parser
       ; "String Parser" >:: test_string_parser
       ; "Digit Parser" >:: test_digit_parser
       ; "Simple Expression" >:: test_simple_expr
       ; "Operator Priority" >:: test_priority
       ; "Parentheses" >:: test_parentheses
       ]
;;

let () = run_test_tt_main suite
