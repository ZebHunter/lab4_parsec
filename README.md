# Лабораторная работа 4. OCaml. Библиотека парсер-комбинаторов

- Автор: Рогачев Михаил Сергеевич P34082

- Цель: получить навыки работы со специфичными для выбранной технологии/языка программирования приёмами.

## Условие задания

В рамках лабораторной работы необходимо реализовать библиотеку парсер-комбинаторов на языке OCaml

## Реализация

### Основное API

В рамках бибдиотеки предоставлены следующие фунции:
- ( >>= ) - последовательная передача парсера (p >>= f -- сначала выполняется p, затем результат передается в f)
- ( <|> ) - альтернатива, пробует парсер p1, и если не сработал - p2
- ( *> ) - выполняет p1 и p2, возвращает результат p2 
- ( <* ) - выполняет p1 и p2, возвращает результат p1 
- many - повторяет парсер p ноль или более раз. Возвращает список результатов
- many1 - аналогичен many, но требует минимум одно успешное применение p
- map - Преобразует результат парсера p функцией f
- ( <*> ) - Применяет парсер-функцию к парсер-значению

Базовые парсеры: 
- satisfy - Парсит символ, удовлетворяющий предикату
- char - парсит конкретный символ c
- string - парсит точную строку s
- digit - парсит цифру ('0'-'9')
- number - парсит целое число (последовательность цифр) и конвертирует в int

### Примеры реализации

Для демонстрации работы библиотеки был написан небольшой калькулятор

``` ocaml
type expr =
  | Num of int
  | Add of expr * expr
  | Mul of expr * expr
```


### Тесты
```OCaml
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

```


## Вывод
В ходе работы я написал библиотеку парсер-комбинаторов на языке OCaml.