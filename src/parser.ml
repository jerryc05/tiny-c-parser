open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers *)

let tok_list = ref []

(* Returns next token in the list. *)
let lookahead () : token =
  match !tok_list with
  | [] -> raise (Failure "no tokens")
  | h :: t -> h

(* Matches the top token in the list. *)
let consume (a : token) : unit =
  match !tok_list with
  | h :: t when a = h ->
    Printf.printf "consumed [%s]!\n" (string_of_token h);
    tok_list := t
  | h :: t -> raise (Failure (Printf.sprintf "consume failed! Expected [%s] but got [%s]!" (string_of_token a) (string_of_token h)))
  | _ -> raise (Failure (Printf.sprintf "consume failed! Expected [%s] but got unknown!" (string_of_token a)))


























































(* Parsing *)

let rec parse_expr_wrapper () = parse_expr ()

and parse_expr () = parse_OrExpr()

and parse_OrExpr() =
  let aa = parse_AndExpr() in
  match lookahead() with
  | Tok_Or ->
    let _ = consume Tok_Or in
    let oo = parse_OrExpr() in
    Or(aa, oo)
  | _ -> aa

and parse_AndExpr() =
  let ee = parse_EqualityExpr() in
  match lookahead() with
  | Tok_And ->
    let _ = consume Tok_And in
    let aa = parse_AndExpr() in
    And(ee, aa)
  | _ -> ee

and parse_EqualityExpr() =
  let rr = parse_RelationalExpr() in
  match lookahead() with
  | Tok_Equal ->
    let _ = consume Tok_Equal in
    let ee = parse_EqualityExpr() in
    Equal(rr, ee)

  | Tok_NotEqual ->
    let _ = consume Tok_NotEqual in
    let ee = parse_EqualityExpr() in
    NotEqual(rr, ee)

  | _ -> rr

and parse_RelationalExpr() =
  let x = parse_AddictiveExpr() in
  match lookahead() with
  | Tok_LessEqual ->
    let _ = consume Tok_LessEqual in
    let y = parse_RelationalExpr() in
    LessEqual(x, y)

  | Tok_GreaterEqual ->
    let _ = consume Tok_GreaterEqual in
    let y = parse_RelationalExpr() in
    GreaterEqual(x, y)

  | Tok_Less ->
    let _ = consume Tok_Less in
    let y = parse_RelationalExpr() in
    Less(x, y)

  | Tok_Greater ->
    let _ = consume Tok_Greater in
    let y = parse_RelationalExpr() in
    Greater(x, y)

  | _ -> x

and parse_AddictiveExpr() =
  let x = parse_MultiplicativeExpr() in
  match lookahead() with
  | Tok_Add ->
    let _ = consume Tok_Add in
    let y = parse_AddictiveExpr() in
    Add(x, y)

  | Tok_Sub ->
    let _ = consume Tok_Sub in
    let y = parse_AddictiveExpr() in
    Sub(x, y)

  | _ -> x

and parse_MultiplicativeExpr() =
  let x = parse_PowerExpr() in
  match lookahead() with
  | Tok_Mult ->
    let _ = consume Tok_Mult in
    let y = parse_MultiplicativeExpr() in
    Mult(x, y)

  | Tok_Div ->
    let _ = consume Tok_Div in
    let y = parse_MultiplicativeExpr() in
    Div(x, y)

  | _ -> x

and parse_PowerExpr() =
  let x = parse_UnaryExpr() in
  match lookahead() with
  | Tok_Pow ->
    let _ = consume Tok_Pow in
    let y = parse_PowerExpr() in
    Pow(x, y)

  | _ -> x

and parse_UnaryExpr() =
  match lookahead() with
  | Tok_Not ->
    let _ = consume Tok_Not in
    Not(parse_UnaryExpr())

  | _ -> parse_PrimaryExpr()

and parse_PrimaryExpr() =
  match lookahead() with
  | Tok_Int x ->
    let _ = consume (Tok_Int x) in
    Int x

  | Tok_Bool x ->
    let _ = consume (Tok_Bool x) in
    Bool x

  | Tok_ID x ->
    consume (Tok_ID x);
    (match lookahead() with
    | Tok_LParen ->
      consume Tok_LParen;
      if lookahead() = Tok_RParen then
        (consume Tok_RParen;
        FunctionCall (x,[]))
      else
        (let lst = parse_Expr() in
        let _ = consume Tok_RParen in
        FunctionCall (x, lst))
    | _ -> ID x)

  | Tok_LParen ->
    let _ = consume Tok_LParen in
    let x = parse_expr() in
    let _ = consume Tok_RParen in
    x

  | _ ->  raise (InvalidInputException ("parse_PrimaryExpr() failed!"))

and parse_Expr() = 
  let e = parse_expr() in
  match lookahead() with
  | Tok_RParen -> [e]
  | _ -> e::parse_ExprList()

and parse_ExprList() = 
  match lookahead() with
  | Tok_RParen -> []
  | _ -> 
    consume Tok_Comma;
    parse_Expr()


























































let rec parse_stmt_wrapper () = parse_stmt()

and parse_stmt () =
  let x = parse_StmtOptions () in
  match lookahead () with
  | Tok_Int_Type  ->
    Seq(x, parse_stmt ())
  | Tok_Bool_Type ->
    Seq(x, parse_stmt ())
  | Tok_ID y      ->
    Seq(x, parse_stmt ())
  | Tok_Print     ->
    Seq(x, parse_stmt ())
  | Tok_If        ->
    Seq(x, parse_stmt ())
  | Tok_For       ->
    Seq(x, parse_stmt ())
  | Tok_While     ->
    Seq(x, parse_stmt ())
  | Tok_Return    ->
    Seq(x, parse_stmt ())
  | _ -> if x == NoOp then NoOp else Seq(x, NoOp)


and parse_StmtOptions() =
  match lookahead() with
  | Tok_Int_Type ->
    let _ = consume Tok_Int_Type in
    (match lookahead() with
    | Tok_ID x ->
      let _ = consume (Tok_ID x) in
      let _ = consume Tok_Semi in
      Declare(Int_Type, x)
    | _ -> raise (InvalidInputException ("Invalid Tok_Int_Type")))

  | Tok_Bool_Type ->
    let _ = consume Tok_Bool_Type in
    (match lookahead() with
    | Tok_ID x ->
      let _ = consume (Tok_ID x) in
      let _ = consume Tok_Semi in
      Declare(Bool_Type, x)
    | _ -> raise (InvalidInputException ("Invalid Tok_Bool_Type")))

  | Tok_ID x ->
    let _ = consume (Tok_ID x) in
    let _ = consume Tok_Assign in
    let y = parse_expr() in
    let _ = consume Tok_Semi in
    Assign(x, y)

  | Tok_If ->
    let _ = consume Tok_If in
    let _ = consume Tok_LParen in
    let x = parse_expr() in
    let _ = consume Tok_RParen in
    let _ = consume Tok_LBrace in
    let y = parse_stmt() in
    let _ = consume Tok_RBrace in
    (match lookahead() with
    | Tok_Else ->
      let _ = consume Tok_Else in
      let _ = consume Tok_LBrace in
      let z = parse_stmt() in
      let _ = consume Tok_RBrace in
      If(x, y, z)
    | _ ->
      If(x, y, NoOp))

  | Tok_Print ->
    let _ = consume Tok_Print in
    let _ = consume Tok_LParen in
    let x = parse_expr() in
    let _ = consume Tok_RParen in
    let _ = consume Tok_Semi in
    Print(x)

  | Tok_While ->
    let _ = consume Tok_While in
    let _ = consume Tok_LParen in
    let x = parse_expr() in
    let _ = consume Tok_RParen in
    let _ = consume Tok_LBrace in
    let y = parse_stmt() in
    let _ = consume Tok_RBrace in
    While(x, y)

  | Tok_Return ->
    consume Tok_Return;
    let x = parse_expr() in
    let _ = consume Tok_Semi in
    Return(x)

  | Tok_For ->
    let _ = consume Tok_For in
    let _ = consume Tok_LParen in
    (match lookahead() with
    | Tok_ID x ->
      let _ = consume (Tok_ID x) in
      let _ = consume Tok_From in
      let y = parse_expr() in
      let _ = consume Tok_To in
      let z = parse_expr() in
      let _ = consume Tok_RParen in
      let _ = consume Tok_LBrace in
      let w = parse_stmt() in
      let _ = consume Tok_RBrace in
      For(x, y, z, w)
    | _ -> raise (InvalidInputException ("Invalid Tok_For")))

  | _ ->
    NoOp


























































let rec parse_top_level toks =
  tok_list := toks;
  Printf.printf "\nParsing top level\n";
  let x = (match lookahead() with
  | Tok_Int_Type ->
    consume Tok_Int_Type;
    (match lookahead() with
    | Tok_Main -> parse_IntMain()
    | _ -> parse_BasicTypeID(true))

  | Tok_Bool_Type -> 
    consume Tok_Bool_Type;
    parse_BasicTypeID(false)

  | _ -> raise (InvalidInputException ("parse_top_level BasicType not matched!"))) in
  if !tok_list = [] then 
    (Printf.printf "\n\nanswer = [%s]\n\n" (string_of_stmt x);
    x)
  else
    (let y = Seq(x, parse_top_level !tok_list) in
    let _ = Printf.printf "\n\nanswer = [%s]\n\n" (string_of_stmt y) in
    y)

and parse_IntMain () =
  Printf.printf "Parsing int main\n";
  consume Tok_Main;
  consume Tok_LParen;
  consume Tok_RParen;
  consume Tok_LBrace;
  let stmt = parse_stmt() in
  let _ = consume Tok_RBrace in
  let _ = consume EOF in
  FunctionDecl ("main", Int_Type, [], stmt)

and parse_BasicTypeID (is_int) =
  Printf.printf "Parsing basic type ID with [%s]\n" (if is_int then "int" else "bool");
  match lookahead() with
  | Tok_ID id -> (
    consume (Tok_ID id);
    consume Tok_LParen;
    let param_list = parse_BasicTypeParam([]) in
    let _ = consume Tok_RParen in
    let _ = consume Tok_LBrace in
    let stmt = parse_stmt() in
    let _ = consume Tok_RBrace in
    FunctionDecl (id, (if is_int then Int_Type else Bool_Type), param_list, stmt))

  | _ -> raise (InvalidInputException (
    Printf.sprintf "parse_BasicTypeID ID not matched! Got [%s]!" (string_of_token (lookahead()))))

and parse_BasicTypeParam(lst) =
  Printf.printf "Parsing basic type param\n";
  (match lookahead() with
  | Tok_RParen -> lst

  | Tok_Int_Type ->
    consume Tok_Int_Type;
    (match lookahead() with
    | Tok_ID x ->
      consume (Tok_ID x);
      parse_BasicTypeParamList(lst@[(x, Int_Type)])
    | _ -> raise (InvalidInputException ("parse_BasicTypeParam ID not matched!")))

  | Tok_Bool_Type ->
    consume Tok_Bool_Type;
    (match lookahead() with
    | Tok_ID x ->
      consume (Tok_ID x);
      parse_BasicTypeParamList(lst@[(x, Bool_Type)])
    | _ -> raise (InvalidInputException ("parse_BasicTypeParam ID not matched!")))

  | _ -> raise (InvalidInputException ("parse_BasicTypeParam BasicType not matched!")))

and parse_BasicTypeParamList(lst) =
  Printf.printf "Parsing basic type param list\n";
  (match lookahead() with
  | Tok_RParen -> lst

  | _ ->
    consume Tok_Comma;
    parse_BasicTypeParam(lst))




















































(* PLEASE UNCOMMENT BASED ON YOUR IMPLEMENTATION *)
(* ONLY ONE LINE SHOULD BE UNCOMMENTED IN EACH FUNCTION *)
let parse_expr_wrapper toks =
    (* UNCOMMENT the following line if you did the parser FUNCTIONALLY *)
    (* let (_, e) = parse_expr toks in e *)
    (* UNCOMMENT the following line if you did the parser IMPERATIVELY *)
    tok_list := toks; parse_expr ()

let parse_stmt_wrapper toks =
    (* UNCOMMENT the following line if you did the parser FUNCTIONALLY *)
    (* let (_, e) = parse_stmt toks in e *)
    (* UNCOMMENT the following line if you did the parser IMPERATIVELY *)
    tok_list := toks; parse_stmt ()
