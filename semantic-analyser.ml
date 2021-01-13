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
   |hd::tl->(if ((compare hd symbol)==0) then index else (contains_symbol tl symbol (index+1)))


  and index_of_list_contains_symbol lst symbol index =
  match lst with 
      |[]->(-1)
      |hd::tl->(if((contains_symbol hd symbol 0)>(-1)) then index else (index_of_list_contains_symbol tl symbol (index+1)))

  and index_of_item_in_list_contains_symbol lst symbol =
  match lst with 
  |[]->(-1)
  |hd::tl-> let index_in_lst=(contains_symbol hd symbol 0) in 
            (if (index_in_lst>(-1)) then index_in_lst else(index_of_item_in_list_contains_symbol tl symbol))
  ;;

 
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
  | LambdaSimple (params,body)-> 
                                let new_env = params_list::env_list in
                                let param_lst = params in
                                let new_body = (annotate_lexical_addresses_recursive body param_lst new_env)in
                                LambdaSimple' (params,new_body)
  |LambdaOpt (params,optinalParam,body)->
                                        let new_env = params_list::env_list in
                                        let param_lst = (List.append params [optinalParam]) in
                                        let new_body = (annotate_lexical_addresses_recursive body param_lst new_env)in
                                        LambdaOpt' (params,optinalParam,new_body)
  | Applic (func,lst) -> Applic' ((annotate_lexical_addresses_recursive func params_list env_list ), ( List.map (fun exp -> (annotate_lexical_addresses_recursive exp params_list env_list )) lst))
  ;;
let annotate_lexical_addresses e =
    (annotate_lexical_addresses_recursive e [] [])
  ;;
(*\\\\\\\\\\\\\\\\\\\\\\\tail-calls/////////////////////////////// *)
let rec tail_call exp =
  match exp with 
  | LambdaSimple'(x,Seq'(body))->LambdaSimple'(x,Seq'(make_TP_last_element body))
  | LambdaSimple'(x,body) -> LambdaSimple'(x,make_TP body)
  | LambdaOpt'(x,y,Seq'(body))-> LambdaOpt' (x,y,Seq'(make_TP_last_element body))
  | LambdaOpt'(x,y,body) -> LambdaOpt' (x,y,make_TP body)
  | If'(test, dit, dif)-> If'((tail_call test),(tail_call dit),(tail_call dif))
  | Or'(lst)-> Or'(List.map (fun x->(tail_call x))lst)
  | Seq'(lst) -> Seq' (make_TP_last_element lst)
  | Set'(vari,exp) -> Set' (vari, (tail_call exp)) (*check if needed*)
  | Def'(vari,exp) -> Def' (vari, (tail_call exp))
  | Applic'(proc,lst) -> Applic'((tail_call proc),(List.map (fun x -> (tail_call x))lst))
  | x -> x

and make_TP = function
  |If'(test,dif,dit) -> If'((tail_call test),(make_TP (tail_call dif)),(make_TP (tail_call dit)))
  |Or' (lst) -> Or'(make_TP_last_element lst)
  |Applic'(proc,lst) -> Applic' ((tail_call proc),(List.map tail_call lst)) (*applicTP after arrow!!!!!*)
  |x -> tail_call x

and make_TP_last_element lst =
  let after_tail_lst = (List.map (fun x -> (tail_call x))) lst in
  let tail = List.hd (List.rev after_tail_lst) in
  List.rev_append (List.tl(List.rev after_tail_lst)) [(make_TP tail)] ;;
(*\\\\\\\\\\\\\\\\\\\\\\\tail-calls/////////////////////////////// *)
let annotate_tail_calls e = (tail_call e);;
(*\\\\\\\\\\\\\\\\\\\\\\\\\\\box/////////////////////////////////////// *)
let get_var_param params var_p= 
                                let minor =(contains_symbol params var_p 0) in  
                                          Set'(VarParam(var_p, minor), Box'(VarParam(var_p,minor)));;
let add_set_at_body body_expr params= 
  match body_expr with 
  |Seq'(lst) -> Seq' (List.append (List.map  (fun a->(get_var_param params a)) params) lst)
  |single_expr-> match params with 
                |hd::tl->Seq' (List.append (List.map  (fun a->(get_var_param params a)) params)  [single_expr])
                |_->single_expr;;




let rec box_recursive e=
  
  match e with 
  | LambdaSimple'(x,body)->LambdaSimple'(x,(add_set_at_body (box_recursive body) x))
  | LambdaOpt'(x,y,body) -> LambdaOpt' (x,y,(add_set_at_body (box_recursive body) (List.append x [y])))
  | If'(test, dit, dif)-> If'((box_recursive test),(box_recursive dit),(box_recursive dif))
  | Or'(lst)-> Or'(List.map (fun x->(box_recursive x))lst)
  | Seq'(lst) -> Seq' (List.map box_recursive lst)
  | Set'(vari,exp) -> BoxSet' (vari, (box_recursive exp)) 
  | Def'(vari,exp) -> Def' (vari, (box_recursive exp))
  | Applic'(proc,lst) -> Applic'((box_recursive proc),(List.map (fun x -> (box_recursive x))lst))
  | Var'(VarParam (x,y)) ->  BoxGet'(VarParam (x,y))
  | Var'(VarBound (x,y,z))-> BoxGet'(VarBound (x,y,z))
  |x->x;;
let box_set e = (box_recursive e);;

let run_semantics expr =
  box_set(
    annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)


