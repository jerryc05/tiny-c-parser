open TokenTypes

let __DEBUG__ = false

let tokenize input =
  let length = String.length input in
  let rec tok pos =
    if pos >= length then [EOF]

    else  if Str.string_match (Str.regexp ",") input pos then
      let value = Str.matched_string input in
      (Tok_Comma)::(tok (pos + String.length value))


    else  if Str.string_match (Str.regexp "int[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "bool[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "printf[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "main[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "if[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "else[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "for[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "from[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "to[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "true[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "false[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "while[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))
    else  if Str.string_match (Str.regexp "return[a-zA-Z0-9_]+") input pos then
      let value = Str.matched_string input in
      (Tok_ID value)::(tok (pos + String.length value))

    else if Str.string_match (Str.regexp "int") input pos then
      Tok_Int_Type::  (tok (pos+3))
    else if Str.string_match (Str.regexp "bool") input pos then
      Tok_Bool_Type:: (tok (pos+4))
    else if Str.string_match (Str.regexp "printf") input pos then
      Tok_Print::     (tok (pos+6))
    else if Str.string_match (Str.regexp "main") input pos then
      Tok_Main::      (tok (pos+4))
    else if Str.string_match (Str.regexp "if") input pos then
      Tok_If::        (tok (pos+2))
    else if Str.string_match (Str.regexp "else") input pos then
      Tok_Else::      (tok (pos+4))
    else if Str.string_match (Str.regexp "for") input pos then
      Tok_For::       (tok (pos+3))
    else if Str.string_match (Str.regexp "from") input pos then
      Tok_From::      (tok (pos+4))
    else if Str.string_match (Str.regexp "to") input pos then
      Tok_To::        (tok (pos+2))
    else if Str.string_match (Str.regexp "while") input pos then
      Tok_While::     (tok (pos+5))
    else if Str.string_match (Str.regexp "true")  input pos then
      (Tok_Bool  true)::(tok (pos+4))
    else if Str.string_match (Str.regexp "false") input pos then
      (Tok_Bool false)::(tok (pos+5))
    else if Str.string_match (Str.regexp "return") input pos then
      (Tok_Return    )::(tok (pos+6))

    else if Str.string_match (Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*") input pos then
      let value = Str.matched_string input in
      let _ = if __DEBUG__ then Printf.printf "Matching ID \"%s\"\n" value else () in
      Tok_ID (value)::(tok (pos + String.length value))

    else if Str.string_match (Str.regexp "-?[0-9]+") input pos then
      let value = Str.matched_string input in
      let _ = if __DEBUG__ then Printf.printf "Matching \"%s\"\n" value else () in
      Tok_Int (int_of_string value)::(tok (pos + String.length value))

    else if Str.string_match (Str.regexp "(") input pos then Tok_LParen::(tok (pos+1))
    else if Str.string_match (Str.regexp ")") input pos then Tok_RParen::(tok (pos+1))
    else if Str.string_match (Str.regexp "{") input pos then Tok_LBrace::(tok (pos+1))
    else if Str.string_match (Str.regexp "}") input pos then Tok_RBrace::(tok (pos+1))

    else if Str.string_match (Str.regexp "==") input pos then Tok_Equal::(tok (pos+2))
    else if Str.string_match (Str.regexp "!=") input pos then Tok_NotEqual::(tok (pos+2))
    else if Str.string_match (Str.regexp ">=") input pos then Tok_GreaterEqual::(tok (pos+2))
    else if Str.string_match (Str.regexp "<=") input pos then Tok_LessEqual::(tok (pos+2))
    else if Str.string_match (Str.regexp ">")  input pos then Tok_Greater::(tok (pos+1))
    else if Str.string_match (Str.regexp "<")  input pos then Tok_Less::(tok (pos+1))
    else if Str.string_match (Str.regexp "=")  input pos then Tok_Assign::(tok (pos+1))

    else if Str.string_match (Str.regexp "||") input pos then Tok_Or::(tok (pos+2))
    else if Str.string_match (Str.regexp "&&") input pos then Tok_And::(tok (pos+2))
    else if Str.string_match (Str.regexp "!")  input pos then Tok_Not::(tok (pos+1))
    else if Str.string_match (Str.regexp ";")  input pos then Tok_Semi::(tok (pos+1))

    else if Str.string_match (Str.regexp "\\+") input pos then Tok_Add::(tok (pos+1))
    else if Str.string_match (Str.regexp "-")   input pos then Tok_Sub::(tok (pos+1))
    else if Str.string_match (Str.regexp "\\*") input pos then Tok_Mult::(tok (pos+1))
    else if Str.string_match (Str.regexp "\\/") input pos then Tok_Div::(tok (pos+1))
    else if Str.string_match (Str.regexp "\\^") input pos then let _ = if __DEBUG__ then Printf.printf "Matching \"^\"\n" else () in
      Tok_Pow::(tok (pos+1))

    else if Str.string_match (Str.regexp "[ \t\n]") input pos then
      let _ = if __DEBUG__ then Printf.printf "Matching \"WHITE_SPACE\"\n" else () in
      tok (pos+1)
    else
      let _ = if __DEBUG__ then Printf.printf "%s is not recognized!\n0" input else () in
      raise (InvalidInputException "Lexer failed!") in
      (* raise (InvalidInputException (String.sub input pos (String.length input - pos))) in *)
  tok 0
