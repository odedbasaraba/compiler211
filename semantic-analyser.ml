#use "tag-parser.ml";;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of var * expr'
  | Def' of var * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | Box'(VarFree v1), Box'(VarFree v2) -> String.equal v1 v2
  | Box'(VarParam (v1,mn1)), Box'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Box'(VarBound (v1,mj1,mn1)), Box'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | BoxGet'(VarFree v1), BoxGet'(VarFree v2) -> String.equal v1 v2
  | BoxGet'(VarParam (v1,mn1)), BoxGet'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | BoxGet'(VarBound (v1,mj1,mn1)), BoxGet'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | BoxSet'(VarFree v1,e1), BoxSet'(VarFree v2, e2) -> String.equal v1 v2 && (expr'_eq e1 e2)
  | BoxSet'(VarParam (v1,mn1), e1), BoxSet'(VarParam (v2,mn2),e2) -> String.equal v1 v2 && mn1 = mn2 && (expr'_eq e1 e2)
  | BoxSet'(VarBound (v1,mj1,mn1),e1), BoxSet'(VarBound (v2,mj2,mn2),e2) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2 && (expr'_eq e1 e2)
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq (Var'(var1)) (Var'(var2))) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;	
                      
exception X_syntax_error;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics : SEMANTICS = struct

let rec contains_symbol lst symbol index =
match lst with 
   |[]->(-1)
   |Var(symbol)::_->index
   |hd::tl->(contains_symbol tl symbol (index+1))
   |_->(-1)


  and index_of_list_contains_symbol lst symbol index =
  match lst with 
      |[]->(-1)
      |hd::tl->(if((contains_symbol hd symbol 0)>(-1)) then index else (index_of_list_contains_symbol tl symbol index+1))
      |_->(-1)
  and index_of_item_in_list_contains_symbol lst symbol =
  match lst with 
  |[]->(-1)
  |hd::tl-> let index_in_lst=(contains_symbol hd symbol 0) in 
            (if (index_in_lst>(-1)) then index_in_lst else(index_of_item_in_list_contains_symbol tl symbol))
  |_->(-1);;

 
let get_var name params_list env_list=
  let index_in_params=(contains_symbol params_list name 0) in
    if(index_in_params>(-1))
    then Var'(VarParam(name ,index_in_params))
    else let index_of_list_in_env=(index_of_list_contains_symbol env_list name 0) in
              (if (index_of_list_in_env>(-1))
              then Var'(VarBound(name,index_of_list_in_env,(index_of_item_in_list_contains_symbol env_list name)))
              else Var'(VarFree(name)));;
let rec annotate_lexical_addresses_recursive expr params_list env_list= match expr with 
| Const(x)->Const'(x)
| Var(name)->(get_var name params_list env_list)
| If (test,dit,dif)-> If' ((annotate_lexical_addresses_recursive test params_list env_list),(annotate_lexical_addresses_recursive dit params_list env_list),(annotate_lexical_addresses_recursive dif params_list env_list))
| Seq (lst) ->Seq'( List.map (fun exp -> (annotate_lexical_addresses_recursive exp params_list env_list )) lst)
| Set (vari,expr) -> Set' ((match (annotate_lexical_addresses_recursive vari params_list env_list) with
                          |Var'(x) -> x
                          |_-> raise X_syntax_error) ,(annotate_lexical_addresses_recursive expr params_list env_list ))
| Def (var1,val1) -> Def' ((match (annotate_lexical_addresses_recursive var1 params_list env_list) with
                          |Var'(x) -> x
                          |_-> raise X_syntax_error),(annotate_lexical_addresses_recursive val1 params_list env_list ))
| Or (lst) -> Or' ( List.map (fun exp -> (annotate_lexical_addresses_recursive exp params_list env_list )) lst)
(*| LambdaSimple of string list * expr
| LambdaOpt of string list * string * expr
| Applic (func,lst) -> Applic' ((annotate_lexical_addresses_recursive func params_list env_list ), ( List.map (fun exp -> (annotate_lexical_addresses_recursive exp params_list env_list )) lst))
|_-> raise X_syntax_error *)
;;
let annotate_lexical_addresses e =
  (annotate_lexical_addresses_recursive e [] [])
;;

let annotate_tail_calls e = raise X_not_yet_implemented;;

let box_set e = raise X_not_yet_implemented;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)


