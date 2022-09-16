open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)



let rec string_of_list conv lst = 
match lst with
| [] -> ""
| h::[] -> conv h
| h::t -> (conv h) ^ " " ^ (string_of_list conv t)


let rec tokenize (input : string) : token list =
  let length = String.length input in

  let rec tok pos =
    if pos = length then 
      []
    else if Str.string_match (Str.regexp "(-?[0-9]+)") input pos then
      let value = Str.matched_string input in
      let value2 = String.sub value (1) (String.length value -2) in 
      Tok_Int(int_of_string value2)::(tok (pos + String.length value))
    else if Str.string_match (Str.regexp "(") input pos then
      Tok_LParen::(tok (pos + 1))
    (*done *)
    else if Str.string_match (Str.regexp ")") input pos then
      Tok_RParen::(tok (pos + 1))
    (*done *)
    else if Str.string_match (Str.regexp "=") input pos then
      Tok_Equal :: (tok (pos + 1))
    (*done *)
    else if Str.string_match (Str.regexp "<>") input pos then
      Tok_NotEqual::(tok (pos + 2))
    (*done *)
    else if Str.string_match (Str.regexp ">") input pos then
      Tok_Greater::(tok (pos + 1))
    (*done *)
    else if Str.string_match (Str.regexp "<") input pos then
      Tok_Less::(tok (pos + 1))
    (*done *)
    else if Str.string_match (Str.regexp ">=") input pos then
      Tok_GreaterEqual::(tok (pos + 2))
    (*done *)
    else if Str.string_match (Str.regexp "<=") input pos then
      Tok_LessEqual::(tok (pos + 2))
    (*done *)
    else if Str.string_match (Str.regexp "||") input pos then
      Tok_Or::(tok (pos + 2))
    (*done *)
    else if Str.string_match (Str.regexp "&&") input pos then
      Tok_And::(tok (pos + 2))
    (*done *)
    else if Str.string_match (Str.regexp "not") input pos then
      Tok_Not::(tok (pos + 3))
    (*done *)
    else if Str.string_match (Str.regexp "if") input pos then
      Tok_If::(tok (pos + 2))
    (*done *)
    else if Str.string_match (Str.regexp "->") input pos then
      Tok_Arrow::(tok (pos + 2))

    else if Str.string_match (Str.regexp "then") input pos then
      Tok_Then::(tok (pos + 4))
    (*done *)
    else if Str.string_match (Str.regexp "else") input pos then
      Tok_Else::(tok (pos + 4))
    (*done *)
    else if Str.string_match (Str.regexp "\\+") input pos then
      Tok_Add::(tok (pos + 1))
    (*done *)
    else if Str.string_match (Str.regexp "-") input pos then
      Tok_Sub::(tok (pos + 1))
    (*done *)
    else if Str.string_match (Str.regexp "\\*") input pos then
      Tok_Mult::(tok (pos + 1))
    (*done *)
    else if Str.string_match (Str.regexp "/") input pos then
      Tok_Div::(tok (pos + 1))
    (*done *)
    else if Str.string_match (Str.regexp "\\^") input pos then
      Tok_Concat::(tok (pos + 1))
    (*doen *)
    else if Str.string_match (Str.regexp "let") input pos then
      Tok_Let::(tok (pos + 3))
    (*done *)
    else if Str.string_match (Str.regexp "rec") input pos then
      Tok_Rec::(tok (pos + 3))
    (*done *)
    else if Str.string_match (Str.regexp "in") input pos then
      Tok_In::(tok (pos + 2))
    (*done *)
    else if Str.string_match (Str.regexp "def") input pos then
      Tok_Def::(tok (pos + 3))
    (*done *)
    else if Str.string_match (Str.regexp "fun") input pos then
      Tok_Fun::(tok (pos + 3))
    (*done *)
    else if Str.string_match (Str.regexp "-?[0-9]+") input pos then
      let value = Str.matched_string input in
      Tok_Int(int_of_string value)::(tok (pos + String.length value))

    else if Str.string_match (Str.regexp "true") input pos then
      Tok_Bool true::(tok (pos + 4))
    
    else if Str.string_match (Str.regexp "false") input pos then
      Tok_Bool false::(tok (pos + 5))
    
    else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then
      let added = Str.matched_string  input in
      let added2 = String.sub (added) (1) ((String.length added)-2) in
      Tok_String added2::(tok (pos + String.length added))

    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
      let added = Str.matched_string input  in
      Tok_ID added::(tok (pos + String.length added))

    else if Str.string_match (Str.regexp ";;") input pos then
      Tok_DoubleSemi::(tok (pos + 1))

    else
      tok (pos + 1)

  in tok 0;;