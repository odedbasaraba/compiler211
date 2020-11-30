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

let rec tag_parse = function 
  (* |Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil))))->If(tag_parse test, tag_parse dit, tag_parse dif) *)
  |Pair(Symbol("quote"), Pair(x, Nil)) -> Const(Sexpr(x))
  |Number(x) -> Const(Sexpr(Number(x)))
  |String(x) -> Const(Sexpr(String(x)))
  |Char(x) -> Const(Sexpr(Char(x)))
  |Bool(x) -> Const(Sexpr(Bool(x)))
  |Symbol(x) -> 
                (if (List.exists (fun v-> x = v) reserved_word_list) then raise X_syntax_error
                 else  Var x)
  |Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil)))) -> If(tag_parse test, tag_parse dit, tag_parse dif)  (* if test dit dif *)
  |Pair(Symbol("if"), Pair(test, Pair(dit, Nil))) -> If(tag_parse test, tag_parse dit, Const(Void))              (* if test then *)
  |Pair(Symbol ("lambda"), Pair(arglist, Pair( exp, Nil))) -> parsing_lambda (Pair (arglist,exp))

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
;;


let tag_parse_expression sexpr = tag_parse sexpr;;
let tag_parse_expressions sexpr = List.map tag_parse sexpr;;
end;; (* struct Tag_Parser *)

