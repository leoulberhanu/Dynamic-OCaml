open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  match lookahead toks with 
    | Some Tok_Let -> parse_let toks
    | Some Tok_If -> parse_if toks
    | Some Tok_Fun -> parse_fun toks
    | _ -> parse_or toks
  and parse_let toks =
    let temp = match_token toks Tok_Let in
    match lookahead temp with 
      | Some Tok_Rec -> 
        let rectok = match_token temp Tok_Rec in 
       ( match lookahead rectok with 
          | Some Tok_ID a -> 
            let idtok = match_token rectok (Tok_ID(a)) in
            let equaltok = match_token idtok Tok_Equal in
            let (tlist1,exp1) = parse_expr equaltok in
            let intok = match_token tlist1 Tok_In in
            let (tlist2,exp2 )= parse_expr intok in
            (tlist2, Let(a, true, exp1, exp2))
          | _ -> raise (InvalidInputException("")))
        
      | Some Tok_ID a-> 
        let idtok = match_token temp (Tok_ID(a)) in
        let equaltok = match_token idtok Tok_Equal in
        let (tlist1,exp1) = parse_expr equaltok in
        let intok = match_token tlist1 Tok_In in
        let (tlist2,exp2) = parse_expr intok in
        (tlist2, Let(a, false, exp1, exp2))
      | _ -> raise (InvalidInputException(""))
  and parse_if toks =
    let iftok = match_token toks Tok_If in
    let (tlist1,exp1) = parse_expr iftok in 
    let thentok = match_token tlist1 Tok_Then in
    let (tlist2,exp2) = parse_expr thentok in 
    let elsetok = match_token tlist2 Tok_Else in
    let (tlist3,exp3) = parse_expr elsetok in
    (tlist3, If(exp1,exp2,exp3))
  and parse_fun toks =
    let funtok = match_token toks Tok_Fun in
    match lookahead funtok with
      | Some Tok_ID a -> let idtok = match_token funtok (Tok_ID(a)) in
                        let arrowtok = match_token idtok Tok_Arrow in
                        let (tlist1,exp1) = parse_expr arrowtok in
                        (tlist1,Fun(a,exp1))
      | _ -> raise (InvalidInputException("")) (* extract id val and then that is ur vrar value *)
  and parse_or toks =
    let (tlist1, exp1) = parse_and toks in 
    match lookahead tlist1 with 
      | Some Tok_Or -> 
        let tlist2 = match_token tlist1 Tok_Or in  
        let (tlist3, exp2)  = parse_or tlist2 in  
        (tlist3, Binop(Or, exp1, exp2))
      | _ -> (tlist1, exp1)
  and parse_and toks =
    let (tlist1, exp1) = parse_equality toks in
    match lookahead tlist1 with
      | Some Tok_And -> 
        let tlist2 = match_token tlist1 Tok_And in
        let (tlist3, exp2) = parse_and tlist2 in
        (tlist3, Binop(And, exp1, exp2))
      | _ ->  (tlist1, exp1)
  and parse_equality toks = (* did i implement this correctly? *)
    let (tlist1,exp1) = parse_relational toks in 
    match lookahead tlist1 with
      | Some Tok_Equal -> 
        let tlist2 = match_token tlist1 Tok_Equal in
        let (tlist3, exp2) = parse_equality tlist2 in
        (tlist3, Binop(Equal,exp1,exp2))
      | Some Tok_NotEqual -> 
        let tlist2 = match_token tlist1 Tok_NotEqual in
        let (tlist3, exp2) = parse_equality tlist2 in
        (tlist3, Binop(NotEqual,exp1,exp2))
      | _ -> (tlist1, exp1 )
  and parse_relational toks =
    let (tlist1,exp1) = parse_additive toks in
    match lookahead tlist1 with
      | Some Tok_Greater ->
        let tlist2 = match_token tlist1 Tok_Greater in
        let (tlist3,exp2) = parse_relational tlist2 in
        (tlist3,Binop(Greater,exp1,exp2))
      | Some Tok_Less ->
        let tlist2 = match_token tlist1 Tok_Less in
        let (tlist3,exp2) = parse_relational tlist2 in
        (tlist3,Binop(Less,exp1,exp2))
      | Some Tok_LessEqual ->
        let tlist2 = match_token tlist1 Tok_LessEqual in
        let (tlist3,exp2) = parse_relational tlist2 in
        (tlist3,Binop(LessEqual,exp1,exp2))
      | Some Tok_GreaterEqual ->
        let tlist2 = match_token tlist1 Tok_GreaterEqual in
        let (tlist3,exp2) = parse_relational tlist2 in
        (tlist3,Binop(GreaterEqual,exp1,exp2))
      | _ -> (tlist1,exp1)
  and parse_additive toks = 
    let (tlist1,exp1) = parse_multiplicative toks in
    match lookahead tlist1 with
      | Some Tok_Add -> 
        let tlist2 = match_token tlist1 Tok_Add in 
        let (tlist3, exp2) = parse_additive tlist2 in
        (tlist3, Binop(Add,exp1,exp2))
      | Some Tok_Sub ->
        let tlist2 = match_token tlist1 Tok_Sub in 
        let (tlist3, exp2) = parse_additive tlist2 in
        (tlist3, Binop(Sub,exp1,exp2))
      | _ -> (tlist1,exp1)

  and parse_multiplicative toks =
    let (tlist1, exp1) = parse_concat toks in
    match lookahead tlist1 with
      | Some Tok_Mult ->
        let tlist2 = match_token tlist1 Tok_Mult in
        let (tlist3, exp2) = parse_multiplicative tlist2 in
        (tlist3, Binop(Mult, exp1, exp2))
      | Some Tok_Div -> 
        let tlist2 = match_token tlist1 Tok_Div in
        let (tlist3, exp2) = parse_multiplicative tlist2 in
        (tlist3, Binop(Div, exp1, exp2))
      | _ -> (tlist1, exp1)
  and parse_concat toks = 
    let (tlist1, exp1) = parse_unary toks in
    match lookahead tlist1 with
      | Some Tok_Concat ->
        let tlist2 = match_token tlist1 Tok_Concat in 
        let (tlist3, exp2) = parse_concat tlist2 in
        (tlist3, Binop(Concat, exp1, exp2))
      | _ -> (tlist1, exp1)
  and parse_unary toks = 
    match lookahead toks with 
      | Some Tok_Not -> 
        let tlist1 = match_token toks Tok_Not in 
        let (tlist2, exp1) = parse_unary tlist1 in
        (tlist2, Not(exp1))
      | _ -> 
        let (tlist1, exp1) = parse_functional toks in (tlist1, exp1) 
  and parse_functional toks =
    let (tlist1, exp1) = parse_primary toks in
    match lookahead tlist1 with 
      | Some Tok_Int a-> 
        let (tlist2, exp2) = parse_primary tlist1 in
        (tlist2, FunctionCall(exp1,exp2))
      | Some Tok_Bool a->
        let (tlist2, exp2) = parse_primary tlist1 in
        (tlist2, FunctionCall(exp1,exp2))
      | Some Tok_String a->
        let (tlist2, exp2) = parse_primary tlist1 in
        (tlist2, FunctionCall(exp1,exp2))
      | Some Tok_ID a-> 
        let (tlist2, exp2) = parse_primary tlist1 in
        (tlist2, FunctionCall(exp1,exp2))
      | Some Tok_LParen ->
        let (tlist2, exp2) = parse_primary tlist1 in
        (tlist2, FunctionCall(exp1,exp2))
      | _ -> (tlist1, exp1)
  and parse_primary toks =
    match lookahead toks with
      | Some Tok_Int (a) -> 
        let tlist1 = match_token toks (Tok_Int(a)) in
        (tlist1, Value(Int(a)))
      | Some Tok_Bool (a) ->
        let tlist1 = match_token toks (Tok_Bool(a)) in
        (tlist1, Value(Bool(a)))
      | Some Tok_String (a) ->
        let tlist1 = match_token toks (Tok_String(a)) in
        (tlist1, Value(String(a)))
      | Some Tok_ID (a) -> 
        let tlist1 = match_token toks (Tok_ID(a)) in
        (tlist1, ID(a))
      | Some Tok_LParen -> (* this case is confusing *)
        let tlist1 = match_token toks Tok_LParen in
        let (tlist2,exp1) = parse_expr tlist1 in
        let tlist3 = match_token tlist2 Tok_RParen in
        (tlist3, exp1)
      | _ -> raise (InvalidInputException(""))


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  
  match lookahead toks with 
    | Some Tok_Def -> 
      let deftok = match_token toks Tok_Def in
      (match lookahead deftok with
        | Some Tok_ID a -> let idtok = match_token deftok (Tok_ID(a)) in
                        let equaltok = match_token idtok Tok_Equal in
                        let (tlist1,exp1) = parse_expr equaltok in
                        let tlist2 = match_token tlist1 Tok_DoubleSemi in
                        (tlist2,Def(a,exp1))
        | _ -> raise (InvalidInputException(""))) (* extract id val and then that is ur vrar value *) 
    | Some Tok_DoubleSemi -> let tlist1 = match_token toks Tok_DoubleSemi in
                        (tlist1, NoOp)
    | _ -> let (tlist1, exp1) = parse_expr toks in
            let tlist2 = match_token tlist1 Tok_DoubleSemi in
            (tlist2, Expr(exp1))
    