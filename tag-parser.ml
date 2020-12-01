#use "reader.ml";;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;
exception X_whyyyyyyyyy;;

module type TAG_PARSER = sig
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;  

(* work on the tag parser starts here *)
let is_reserved_symbol sym = List.exists (fun v-> sym = v) reserved_word_list
let rec tag_parse = function 
  (* |Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil))))->If(tag_parse test, tag_parse dit, tag_parse dif) *)
  |Number(x) -> Const(Sexpr(Number(x)))
  |String(x) -> Const(Sexpr(String(x)))
  |Char(x) -> Const(Sexpr(Char(x)))
  |Bool(x) -> Const(Sexpr(Bool(x)))
  |Nil -> Const(Sexpr(Nil))
  |Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil)))) -> If(tag_parse test, tag_parse dit, tag_parse dif)  (* if test dit dif *)
  |Pair(Symbol("if"), Pair(test, Pair(dit, Nil))) -> If(tag_parse test, tag_parse dit, Const(Void))              (* if test then *)
  |Pair(Symbol ("lambda"), Pair(arglist, Pair( exp, Nil))) -> parsing_lambda (Pair (arglist,exp))
  |Pair(Symbol ("or"), expr_list) -> Or (tag_parse_list_from_pair expr_list)
  |Pair(Symbol "define", Pair(name, Pair(expr, Nil)))-> Def (tag_parse name, tag_parse expr)
  |Pair(Symbol "set!", Pair(x, Pair(exp, Nil)))-> Set(tag_parse x, tag_parse exp)
  |Pair(Symbol("quote"), Pair(x, Nil)) -> Const(Sexpr(x))
  |Pair(Symbol "begin",explist)-> Seq(sequnce_complete (convert_to_list explist))
  |Pair(Symbol ("quasiquote"), Pair (x , Nil)) -> tag_parse (evallll (x))
                (* VVVV should be last I think or change the error to compute it if it is a reserved word - Raviv*)
  |Pair(function_applic,arg_list) ->
                                let parsed_function= tag_parse function_applic in
                                let args= tag_parse_list_from_pair arg_list in
                                Applic (parsed_function,args)

  |Symbol(x) -> 
                                (if (is_reserved_symbol x) then raise X_syntax_error
                                 else  Var x)

and evallll x =
match x with 
  |Nil -> Nil 
  |Pair(Symbol ("unquote"), Pair(a, Nil))-> a 
  |Pair(Symbol ("unquote-splicing"), Pair(a, Nil))-> a
  |Pair(Pair(Symbol ("unquote"), Pair(l, Nil)), b)-> Pair(Symbol ("cons"),Pair (l, Pair( (evallll b),Nil)))  
  |Pair(Pair(Symbol ("unquote"), a), b)-> Pair(Symbol "cons",Pair (a, Pair( (evallll b),Nil)))
  |Pair(Pair(Symbol ("unquote-splicing") , Pair(s, Nil)), b) -> Pair(Symbol ("append"),Pair( s,Pair( (evallll b), Nil)))  
  |Pair(Pair(Symbol ("unquote-splicing") ,s), b)-> Pair(Symbol "append",Pair( s,Pair( (evallll b), Nil)))
  |Pair(Pair(a, b),c) ->  Pair(Symbol "cons", Pair(Pair (Symbol "cons", Pair(evallll a, Pair(evallll b , Nil))), Pair((evallll c), Nil)))
  |Pair(not_unquote, b) ->  Pair(Symbol "cons", Pair(Pair (Symbol "quote", Pair( not_unquote, Nil)), Pair((evallll b),Nil))) 
  |quot-> (Pair (Symbol "quote", Pair( quot, Nil)))

  and tag_parse_list_from_pair x= 
                let lst =convert_to_list x in
                List.map (fun a-> (tag_parse a)) lst 
                        
  and proper_list = function                    (*check the list for the lambda*)
    |Nil -> true 
    |Pair (l,r) -> proper_list(r)
    |_ ->false
    
  and convert_to_list =function
  | Pair(l,r)-> l :: (convert_to_list r)
  | Nil -> []
  | l -> [l]

  and parsing_lambda  = function 
  |(Pair (arglist,exp))->
                if (proper_list arglist) then (make_simple_lambda (Pair (arglist,exp))) 
                else (make_opt_lambda(Pair (arglist,exp)))
  |_ -> raise X_syntax_error
        (* parsing the sequence *)
  and sequence tmp_body= 
      match (List.length tmp_body) with 
      |0-> Const(Void)
      |1 ->  tag_parse (List.hd tmp_body)
      |_ ->  Seq (List.map (fun a-> (tag_parse a)) tmp_body)

   and  tmp_sequence tmp_body= 
   match (List.length tmp_body) with 
   |0-> [Const(Void)]
   |1 ->  [tag_parse (List.hd tmp_body)]
   |_ ->   (List.map (fun a-> (tag_parse a)) tmp_body)  
  and sequence_flat =function 
    | []-> []
    | Seq(x) :: tail -> List.append x (sequence_flat tail)
    | hd::tl -> List.append [hd] (sequence_flat tl) 
    
  and sequnce_complete body= sequence_flat (tmp_sequence body)
      (* check for duplicate in the are list  *)
  and check_dup args =
      match args with 
      |[] -> false
      | head::tail -> List.exists (fun(element)->element = head) tail || check_dup tail
        (* geting the value of symbol *)
  and remSym sym= 
        match sym with 
                 | Symbol(a)-> a
                 | _->raise X_syntax_error
    (* simple lambda *)
  and make_simple_lambda = function
  |(Pair (l,r))->
                  let tmp_args = (convert_to_list l) in 
                  let args = (List.map remSym tmp_args) in 
                  let tmp_body = (convert_to_list r) in 
                  let body =  (sequence tmp_body) in 
                  if body= Const(Void) || (check_dup args) then raise X_syntax_error
                  else LambdaSimple (args, body)
  |_ -> raise X_syntax_error
    (* opt lambda *)
  and make_opt_lambda = function
  |(Pair (l,r))->
                  let tmp_args = (convert_to_list l) in 
                  let args = (List.map remSym tmp_args) in 
                  let last_arg =  (List.nth args ((List.length args) - 1)) in    (*we know its an improper list *)
                  let tmp_body = (convert_to_list r) in 
                  let body =  (sequence tmp_body) in
                  if body= Const(Void) || (check_dup args) then raise X_syntax_error
                  else LambdaOpt (args, last_arg, body)
  |_ -> raise X_syntax_error
  (* quasiquote macro exception *)
  
;;


let tag_parse_expression sexpr = tag_parse sexpr;;
let tag_parse_expressions sexpr = List.map tag_parse sexpr;;
end;; (* struct Tag_Parser *)
