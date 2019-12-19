open SmallCTypes
open EvalUtils
open Builtins

exception TypeError of string
exception DeclareError of string
exception DivByZeroError
exception Not_Found

let assoc_opt k t =
  (if List.mem_assoc k t then
    Some (List.assoc k t)
  else
    None)

(* Insert value into environment and raise Declare error with passed message *)
let insert_val (k: string) (v: value) (env: environment) (msg: string): environment = failwith "Unfinished"

(* Helper function useful for inserting primitive values into the environment *)
let insert_primitive k v typ env =
  failwith "Unfinished"








































(* Get value from environment and raise DeclareEror if it is not found. *)
let rec get_val env k = match env with
| [] -> raise (DeclareError "get_val reached end of list!")
| (x, y)::env' -> if k = x then y else get_val env' k

let rec member env x = match env with
  | [] -> false
  | (y, _)::env' -> 
    if x = y then true else member env' x

(* Function references *)
let funcs = ref []

(* Used to access functions outside module. *)
let get_funcs () = !funcs

let reset_funcs () = funcs := []

let add_func name typ params body =
  if (List.mem_assoc name !funcs) then
    raise (DeclareError ("Function " ^ name ^" already exists"))
  else
    funcs := (name, (typ, params, body))::!funcs

let get_func name = List.assoc name !funcs







































let get_type: (value -> data_type) = function
  | _ -> failwith "Unfinished"

let is_thunk (env: environment) (id: string): bool = failwith "Unfinished"

(* checks that value equals expected data_type fails with TypeError otherwise *)
let check_type (expect: data_type) (v: value): value = failwith "Unfinished"

(* Creates thunks for each argument and inserts them into new environment used to evaluate function body.
    Fails if too many or too few arguments and "if" parameters. *)
let rec new_func_scope (params: parameter list) (args: expr list) (curr_env: environment): environment =
  List.map2 (fun a b -> match a with 
  | (s, d) ->
    (s, Thunk_Val(curr_env, b, d))
  ) params args











































(* Functions are mutually recursive *)
(* Replace this with your code from P4 and add to it to complete this project *)
and eval_expr env t = match t with
	| Int x -> Int_Val x

	| Bool x -> Bool_Val x
	
  | ID x -> 
    let v = (get_val env x) in
    (match v with
      | Thunk_Val (env', exp, dtype) -> 
        let val' = (eval_expr env' exp) in 
        (match val' with
          | Int_Val a when dtype = Int_Type -> val' 
          | Bool_Val a when dtype = Bool_Type -> val'
          | _ -> raise (TypeError ("eval_expr ID thunk_val failed!")))
      | _ -> v)

	| Add (exp1, exp2) -> 
		let v1 = eval_expr env exp1 in
		let v2 = eval_expr env exp2 in 
		(match v1 with
		| Int_Val x1 -> 
      (match v2 with 
        | Int_Val x2 -> Int_Val (x1+x2)
        | _ -> raise (TypeError ("eval_expr Add int_val failed!")))
		| _ -> raise (TypeError ("eval_expr Add failed!"))) 

	| Sub (exp1, exp2) -> 
		let v1 = eval_expr env exp1 in
		let v2 = eval_expr env exp2 in 
		(match v1 with
		| Int_Val x1 -> 
      (match v2 with 
        | Int_Val x2 -> Int_Val (x1-x2)
        | _ -> raise (TypeError ("eval_expr Sub int_val failed!")))
		| _ -> raise (TypeError ("eval_expr Sub failed!")))

	| Mult (exp1, exp2) -> 
		let v1 = eval_expr env exp1 in
		let v2 = eval_expr env exp2 in 
		(match v1 with
		| Int_Val x1 -> 
      (match v2 with 
        | Int_Val x2 -> Int_Val (x1*x2)
        | _ -> raise (TypeError ("eval_expr Mult int_val failed!")))
		| _ -> raise (TypeError ("eval_expr Mult failed!")))

	| Div (x, y) ->
    let xx = eval_expr env x in
    let yy = eval_expr env y in
    (match (xx, yy) with
      | (Int_Val xxx, Int_Val yyy) ->
        if yyy <> 0 then
          Int_Val (xxx/yyy)
        else
          raise DivByZeroError
      | _ -> raise (TypeError "TypeError in Div()!"))

	| Pow (x, y) ->
    let xx = eval_expr env x in
    let yy = eval_expr env y in
    (match (xx, yy) with
      | (Int_Val xxx, Int_Val yyy) ->
        let pow x y =
          if y < 0 then 0 else
          if x = 0 && y <= 0 then 
            raise DivByZeroError 
          else
            (int_of_float ((float_of_int x)**(float_of_int y))) in
        Int_Val (pow xxx yyy)
      | _ -> raise (TypeError "TypeError in Pow()!"))

	
  | Or (x, y) ->
    let xx = eval_expr env x in
    let yy = eval_expr env y in
    (match (xx, yy) with
      | (Bool_Val xxx, Bool_Val yyy) -> Bool_Val (xxx||yyy)
      | _ -> raise (TypeError "TypeError in Or()!"))

  | And (x, y) ->
    let xx = eval_expr env x in
    let yy = eval_expr env y in
    (match (xx, yy) with
      | (Bool_Val xxx, Bool_Val yyy) -> Bool_Val (xxx&&yyy)
      | _ -> raise (TypeError "TypeError in And()!"))

  | Not x ->
    let xx = eval_expr env x in
    (match xx with
      | Bool_Val xxx -> Bool_Val (not xxx)
      | _ -> raise (TypeError "TypeError in Not()!"))

  | Greater (x, y) ->
    let xx = eval_expr env x in
    let yy = eval_expr env y in
    (match (xx, yy) with
      | (Int_Val xxx, Int_Val yyy) -> Bool_Val (xxx>yyy)
      | _ -> raise (TypeError "TypeError in Greater()!"))

  | Less (x, y) ->
    let xx = eval_expr env x in
    let yy = eval_expr env y in
    (match (xx, yy) with
      | (Int_Val xxx, Int_Val yyy) -> Bool_Val (xxx<yyy)
      | _ -> raise (TypeError "TypeError in Less()!"))

  | GreaterEqual (x, y) ->
    let xx = eval_expr env x in
    let yy = eval_expr env y in
    (match (xx, yy) with
      | (Int_Val xxx, Int_Val yyy) -> Bool_Val (xxx>=yyy)
      | _ -> raise (TypeError "TypeError in GreaterEqual()!"))

  | LessEqual (x, y) ->
    let xx = eval_expr env x in
    let yy = eval_expr env y in
    (match (xx, yy) with
      | (Int_Val xxx, Int_Val yyy) -> Bool_Val (xxx<=yyy)
      | _ -> raise (TypeError "TypeError in LessEqual()!"))

	| Equal (x, y) ->
    let xx = eval_expr env x in
    let yy = eval_expr env y in
    (match (xx, yy) with
      | (Int_Val  xxx, Int_Val  yyy) -> Bool_Val (xxx=yyy)
      | (Bool_Val xxx, Bool_Val yyy) -> Bool_Val (xxx=yyy)
      | _ -> raise (TypeError "TypeError in Equal()!"))

  | NotEqual (x, y) ->
    let xx = eval_expr env x in
    let yy = eval_expr env y in
    (match (xx, yy) with
      | (Int_Val  xxx, Int_Val  yyy) -> Bool_Val (xxx<>yyy)
      | (Bool_Val xxx, Bool_Val yyy) -> Bool_Val (xxx<>yyy)
      | _ -> raise (TypeError "TypeError in NotEqual()!"))
  
	| FunctionCall (fun_name, explist) -> 
    if (member (!funcs) fun_name) = false then
      raise Not_Found
    else
      let (_type, _paramlist, _stmt) = get_func fun_name in
      if List.length _paramlist <> List.length explist then
        raise (DeclareError ("explist length != paranlist length"))	
      else
        let newenv = (eval_stmt (new_func_scope _paramlist explist env) _stmt) in
        let rvalue = (get_val newenv "~ret") in
        (match rvalue with
        | Int_Val  a when _type = Int_Type  -> rvalue
        | Bool_Val a when _type = Bool_Type -> rvalue 
        | _ -> raise (TypeError ("type does not match ~ret")))
      
	







































(* Replace this with your code from P4 and add to it to complete this project *)
and eval_stmt env s =  match s with
  | NoOp -> env

  | Seq (x, y) ->
    let xx = eval_stmt env x in
    eval_stmt xx y

  | Declare (x, y) ->
    if (member env y) then
      raise (DeclareError y)
    else
      (match x with
        | Int_Type  -> (y, Int_Val 0)::env
        | Bool_Type -> (y, Bool_Val false)::env)

  | Assign (x, y) ->
  if (List.mem_assoc x env) then
    let v1 = (List.assoc x env) in
    (match eval_expr env y  with
      | Int_Val  v -> (match v1 with
        | Int_Val  _  -> (x, Int_Val(v)) ::(List.remove_assoc x env)
        | _           -> raise (TypeError "TypeError in Assign int"))

      | Bool_Val v -> (match v1 with
        | Bool_Val _  -> (x, Bool_Val(v))::(List.remove_assoc x env)
        | _           -> raise (TypeError "TypeError in Assign bool"))

      | _ -> raise (TypeError "TypeError in Assign neither int nor bool")) 
  else 
    raise (DeclareError x)
  
	| If (x, y, z) -> (match eval_expr env x with
    | Bool_Val xx -> eval_stmt env (if xx then y else z)
    | _ -> raise (TypeError "TypeError in If"))

	| While (x, y) ->
      let rec f env = (match eval_expr env x with
        | Bool_Val xx ->
          if xx then f (eval_stmt env y) else env
        | _ -> raise (TypeError "TypeError in While!")) in
      f env

	| For (str, exp1, exp2, stm1) ->
		let x1 = eval_expr env exp1 in
		let x2 = eval_expr env exp2 in
		(match (x1, x2) with
		| (Int_Val v1, Int_Val v2) -> 
			if (member env str) then 
        eval_stmt env (Seq(Assign(str, exp1), While(LessEqual(ID(str), exp2), Seq(stm1, Assign(str, Add(ID(str), Int(1)))))))
			else 
        eval_stmt env (Seq(Declare(Int_Type, str), Seq(Assign(str, exp1), While(LessEqual(ID(str), exp2), Seq(stm1, Assign(str, Add(ID(str), Int(1))))))))
		| _ -> raise (TypeError ("TypeError in For!")))

	| Print x -> (match eval_expr env x with
    | Int_Val y ->
      print_output_int y;
      print_output_newline ();
    | Bool_Val y ->
      print_output_bool y;
      print_output_newline ();
    | _ -> (););
      env
  
	| Return (x) -> 
		("~ret", (eval_expr env x))::env

	| FunctionDecl (name, dt, paramlist, stmt) -> 
    if name <> "main" then 
      (add_func name dt paramlist stmt;
      env)
    else 
      eval_stmt env stmt