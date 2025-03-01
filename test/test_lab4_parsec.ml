open OUnit2
open Lab4_parsec.Basic
open Lab4_parsec.Parser
open Lab4_parsec.Json

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

let test_json_null _ = assert_equal (Ok JsonNull) (parse_json "null")

let test_json_bool _ =
  assert_equal (Ok (JsonBool true)) (parse_json "true");
  assert_equal (Ok (JsonBool false)) (parse_json "false")
;;

let test_json_number _ =
  assert_equal (Ok (JsonNumber 42.0)) (parse_json "42");
  assert_equal (Ok (JsonNumber (-3.14))) (parse_json "-3.14");
  assert_equal (Ok (JsonNumber 2.5e3)) (parse_json "2.5e3")
;;

let test_json_string _ =
  assert_equal (Ok (JsonString "hello")) (parse_json "\"hello\"");
  assert_equal (Ok (JsonString "line\nbreak")) (parse_json "\"line\\nbreak\"")
;;

let test_json_array _ =
  assert_equal (Ok (JsonArray [ JsonNumber 1.0; JsonNumber 2.0; JsonNumber 3.0 ])) (parse_json "[1, 2, 3]")
;;

let test_json_object _ =
  assert_equal (Ok (JsonObject [ "key", JsonString "value" ])) (parse_json "{\"key\": \"value\"}")
;;

let suite =
  "Parser Tests"
  >::: [ "Char Parser" >:: test_char_parser
       ; "String Parser" >:: test_string_parser
       ; "Digit Parser" >:: test_digit_parser
       ; "Simple Expression" >:: test_simple_expr
       ; "Operator Priority" >:: test_priority
       ; "Parentheses" >:: test_parentheses
       ; "test_json_null" >:: test_json_null
       ; "test_json_bool" >:: test_json_bool
       ; "test_json_number" >:: test_json_number
       ; "test_json_string" >:: test_json_string
       ; "test_json_array" >:: test_json_array
       ; "test_json_object" >:: test_json_object
       ]
;;

let () = run_test_tt_main suite
