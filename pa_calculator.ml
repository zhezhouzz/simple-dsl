open Camlp4.PreCast
module MyUsers = Map.Make(String);;

type int_expr =
    | NumInt of int
    | IVariable of string
    | Plus of expr * expr
    | Multi of expr * expr
    | Minus of expr * expr

type bool_expr =
    | NumBool of bool
    | BVariable of string
    | And of expr * expr
    | Or of expr * expr
    | Not of expr

type expr =
    | Parentheses of expr
    | IntExpr of int_expr
    | Bool of bool_expr

let expression = Gram.Entry.mk "expression"

EXTEND Gram
  GLOBAL: expression;

  expression:
  [   "Parentheses" NONA
      [ "("; s = SELF; ")" -> Parentheses s]
    | "IntExpr" NONA
      [ i = int_expr -> IntExpr i]
    | "BoolExpr" NONA
      [ b = bool_expr -> BoolExpr b] ];

  int_expr:
  [   "NumInt" NONA
      [ `INT (i, _) -> Num i]
    | "NumString" NONA
      [ s = LIDENT -> Variable s]
    | "Plus" LEFTA
      [ x = SELF; "+"; y = SELF -> Plus (x, y) ]
    | "Multi" LEFTA
      [ x = SELF; "*"; y = SELF -> Multi (x, y) ]
    | "Minus" LEFTA
      [ x = SELF; "-"; y = SELF -> Minus (x, y) ] ];

  bool_expr:
  [   "NumBool" NONA
      [ b = SYMBOL -> let if_true = equal "true" b in
                      (match if_true with
                       | true -> NumBool true
                       | false -> let if_false = equal "false" b in
                                  (match if_false with
                                   | true -> NumBool false
                                   | false -> ))
                      | "true" ->
                      |  ]
    | "NumString" NONA
      [ s = LIDENT -> Variable s]
    | "Plus" LEFTA
      [ x = SELF; "+"; y = SELF -> Plus (x, y) ]
    | "Multi" LEFTA
      [ x = SELF; "*"; y = SELF -> Multi (x, y) ]
    | "Minus" LEFTA
      [ x = SELF; "-"; y = SELF -> Minus (x, y) ] ];
END

type state = int MyUsers.t

let _loc = Loc.mk "<string>"

exception PaserError of string

let rec generate_code st e =
  match e with
  | Num i -> (Some i, let i_str = string_of_int i in <:expr< $int:i_str$ >>)
  | Variable s -> let i = (try (MyUsers.find s st) with Not_found -> raise (PaserError "PaserError")) in
                  (Some i, let i_str = string_of_int i in <:expr< $int:i_str$ >>)
(*   | Assign (x, expr) -> let ast_v = generate_code st expr
                     in (match ast_v with
                     | (Some i), _ -> let st = (MyUsers.add x i st) in (None, <:expr< NONE >>)
                     | _, code     -> raise (PaserError "PaserError")) *)
  | Plus (v1, v2) -> let ast_v1 = generate_code st v1
                     and ast_v2 = generate_code st v2
                     in (match (ast_v1, ast_v2) with
                     | ((Some i), _), ((Some j), _) -> let _str = string_of_int (i+j) in ((Some (i+j)), <:expr< $int:_str$ >>)
                     | (_, code1), (_, code2)       -> raise (PaserError "PaserError"))
  | Multi (v1, v2) -> let ast_v1 = generate_code st v1
                     and ast_v2 = generate_code st v2
                     in (match (ast_v1, ast_v2) with
                     | ((Some i), _), ((Some j), _) -> let _str = string_of_int (i*j) in ((Some (i*j)), <:expr< $int:_str$ >>)
                     | (_, code1), (_, code2)       -> raise (PaserError "PaserError"))
  | Minus (v1, v2) -> let ast_v1 = generate_code st v1
                     and ast_v2 = generate_code st v2
                     in (match (ast_v1, ast_v2) with
                     | ((Some i), _), ((Some j), _) -> let _str = string_of_int (i-j) in ((Some (i-j)), <:expr< $int:_str$ >>)
                     | (_, code1), (_, code2)       -> raise (PaserError "PaserError"))

let parse_and_generate_code str =
  let e = Gram.parse_string expression _loc str
  and st = MyUsers.empty in
   match (try (generate_code st e) with (PaserError err_code) -> (None, <:expr< $str:err_code$ >>)) with
   | _, code -> code

let _ =
    print_string "# ";
    let str = read_line () in
    let e = parse_and_generate_code str in
        let ast_e = <:expr< $e$ >> in
        Camlp4.PreCast.Printers.OCaml.print_implem <:str_item< let _ = let res = $ast_e$ in print_int res >>
