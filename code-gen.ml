#use "semantic-analyser.ml";;

exception X_not_yet_implemented_code_gen;;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string )) list
  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (constant * (int * string )) list -> (string * int) list -> int -> expr'  -> string 
end;;

module Code_Gen : CODE_GEN = struct
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~make const table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
  let get_offset const_tbl_and_ofsset=
    match const_tbl_and_ofsset with 
    |(_,num) -> num
   ;;


  let get_const_tbl const_tbl_and_ofsset=
    match const_tbl_and_ofsset with 
    |(const_tbl,_) ->const_tbl;;

  let add_to_const_table const tbl offset assembly_expression = 
    (List.append tbl ([(const, (offset, assembly_expression))]));;
    
  let rec  find_const_addr var tbl =
    match tbl with 
    | (v, (addr,_))::tl -> if(var = v) then addr else (find_const_addr var tl) 
    | []-> -1;;
    let make_string_char_list str= 
      if str=""
      then "\"\""
      else "{" ^  String.concat "," (List.map (fun single_character-> string_of_int (Char.code single_character)) (string_to_list str)) ^ "}";;
  
  let rec eval_const ast const_tbl_and_ofsset= 

      match ast with
      | If'(test,dit, dif) -> 
        let new_const_tbl_and_ofsset= (eval_const test const_tbl_and_ofsset) in
        let new_const_tbl_and_ofsset= (eval_const dit new_const_tbl_and_ofsset) in
          (eval_const dif new_const_tbl_and_ofsset)
      | LambdaSimple'(args,body) -> (eval_const body const_tbl_and_ofsset)
      | LambdaOpt'(args,arg,body) -> (eval_const body const_tbl_and_ofsset)
      | Set' (y, z)-> let new_const_tbl_and_ofsset = (eval_const (Var' y) const_tbl_and_ofsset) in
                        (eval_const z new_const_tbl_and_ofsset) 
      | Or' (args) -> (List.fold_left (fun acc curr-> (eval_const curr acc))  const_tbl_and_ofsset args)
      | Def' (variable,y) ->
        let new_const_tbl_and_ofsset= (eval_const (Var' variable) const_tbl_and_ofsset) in
        (eval_const y new_const_tbl_and_ofsset) 
      | Applic' (op,args)-> 
        let new_const_tbl_and_ofsset= (List.fold_left (fun acc curr-> (eval_const curr acc))  const_tbl_and_ofsset args) in
        (eval_const op new_const_tbl_and_ofsset)
      | ApplicTP' (op,args) ->
        let new_const_tbl_and_ofsset= (List.fold_left (fun acc curr-> (eval_const curr acc))  const_tbl_and_ofsset args) in
        (eval_const op new_const_tbl_and_ofsset)
      | Seq' (body)-> (List.fold_left (fun acc curr-> (eval_const curr acc))  const_tbl_and_ofsset body)
      | BoxSet'(var,exp)-> (eval_const exp const_tbl_and_ofsset)
      | Const'(const)->
                        let offset=get_offset const_tbl_and_ofsset in
                                      let const_tbl= get_const_tbl const_tbl_and_ofsset in 
                                      const_Handler const const_tbl offset 
      | exp -> const_tbl_and_ofsset



    and const_Handler const const_tbl offset=
    let address_at_const_table= (find_const_addr const const_tbl) in  
    match address_at_const_table with
    | -1-> 
    (
      match const with
      | Sexpr(Char x)-> ((add_to_const_table const const_tbl offset ("MAKE_LITERAL_CHAR("^(string_of_int(Char.code x))^")")),(offset +2))
      | Sexpr(Number(Fraction(x,y))) -> ((add_to_const_table const const_tbl offset ("MAKE_LITERAL_RATIONAL(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")")  ),(offset+17))
      | Sexpr(Number(Float(x))) ->  ((add_to_const_table const const_tbl offset ("MAKE_LITERAL_FLOAT(" ^ (string_of_float x) ^ ")")) ,(offset+9) )
      | Sexpr(Pair(x,y)) -> let newtbl_and_offset = (const_Handler (Sexpr x) const_tbl offset )  in 
                              let add_of_x = offset in (* just change the name in order to have clean code *)
                                (match newtbl_and_offset with 
                                |(tbl,add_of_y)-> let newtbl_and_offset = (const_Handler (Sexpr y) tbl add_of_y ) in
                                                    (match newtbl_and_offset with 
                                                    |(tbl,offset_of_pair) -> ((add_to_const_table const tbl offset_of_pair ("MAKE_LITERAL_PAIR("
                                                      ^ (string_of_int add_of_x) ^ ", " ^ (string_of_int add_of_y)^ ")")),(offset_of_pair+17) )
                                ))
      | Sexpr(Bool x)-> (const_tbl,offset) 
      | Sexpr (Nil) ->   (const_tbl,offset)
      | Void ->   (const_tbl,offset)                      
      | Sexpr(String x) -> ((add_to_const_table const const_tbl offset ("MAKE_LITERAL_STRING "^(make_string_char_list x)^" ")), ((String.length x)+9+offset))
      | Sexpr(Symbol x) -> let newtbl_and_offset = (const_Handler (Sexpr(String x)) const_tbl offset) in 
                              let add_of_x= offset in
                              match newtbl_and_offset with 
                              | (tbl,symbol_offset)->  ((add_to_const_table const tbl symbol_offset ("MAKE_LITERAL_SYMBOL(const_tbl+"
                            ^ (string_of_int add_of_x) ^ ")") ),(symbol_offset+ 9))
                          
    
    
  
    
    )
    | _ -> (const_tbl,offset)
    ;;
  let make_consts_tbl asts = 
   let ze = (List.fold_left (fun acc curr-> (eval_const curr acc))
   ([(Void, (0, "MAKE_VOID"));
    (Sexpr Nil, (1, "MAKE_NIL"));
    (Sexpr(Bool false), (2, "MAKE_LITERAL_BOOL (0)"));
    (Sexpr(Bool true), (4, "MAKE_LITERAL_BOOL (1)"))],6) asts) in
    match ze with 
    |(tbl,mashu)->tbl
    ;; 
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~make const table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~make fvar table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
let primitive_names_to_labels =
  [
    (* Type queries  *)
    "boolean?", "boolean?"; "flonum?", "flonum?"; "rational?", "rational?";
    "pair?", "pair?"; "null?", "null?"; "char?", "char?"; "string?", "string?";
    "procedure?", "procedure?"; "symbol?", "symbol?";
    (* String procedures *)
    "string-length", "string_length"; "string-ref", "string_ref"; "string-set!", "string_set";
    "make-string", "make_string"; "symbol->string", "symbol_to_string";
    (* Type conversions *)
    "char->integer", "char_to_integer"; "integer->char", "integer_to_char"; "exact->inexact", "exact_to_inexact";
    (* Identity test *)
    "eq?", "eq?";
    (* Arithmetic ops *)
    "+", "add"; "*", "mul"; "/", "div"; "=", "eq"; "<", "lt";
    (* Additional rational numebr ops *)
    "numerator", "numerator"; "denominator", "denominator"; "gcd", "gcd";
    (* you can add yours here *)
  ]
  let rec  find_fvar var tbl =
    match tbl with 
    | (fvar, addr)::tl -> if(var = fvar) then addr else (find_fvar var tl) 
    | []-> -1;;
  let rec make_Fvar_exp ast fvar_tbl= 
      match ast with
      | If'(test,dit,dif) -> 
      let fvar_tbl = (make_Fvar_exp test fvar_tbl) in
      let fvar_tbl = (make_Fvar_exp dit fvar_tbl) in
                     (make_Fvar_exp dif fvar_tbl)
      | LambdaSimple'(args,body) -> (make_Fvar_exp body fvar_tbl)
      | LambdaOpt'(args,arg,body) -> (make_Fvar_exp body fvar_tbl)
      | Set' (y, z)-> 
      let var = Var'(y) in 
        let fvar_tbl = (make_Fvar_exp var fvar_tbl) in
                       (make_Fvar_exp z fvar_tbl) 
      | Def' (variable,y) ->
       let var = Var'(variable) in 
       let fvar_tbl = (make_Fvar_exp var fvar_tbl) in
                       (make_Fvar_exp y fvar_tbl) 
      | Or' (args) -> (List.fold_left (fun acc curr-> (make_Fvar_exp curr acc))  fvar_tbl args)
      | Applic' (op,args)-> 
        let fvar_tbl= (List.fold_left (fun acc curr-> (make_Fvar_exp curr acc))  fvar_tbl args) in
                      (make_Fvar_exp op fvar_tbl)
      | ApplicTP' (op,args) ->
        let fvar_tbl= (List.fold_left (fun acc curr-> (make_Fvar_exp curr acc))  fvar_tbl args) in
                      (make_Fvar_exp op fvar_tbl)
      | Seq' (body)-> (List.fold_left (fun acc curr-> (make_Fvar_exp curr acc))  fvar_tbl body)
      | BoxSet'(var,exp)-> (make_Fvar_exp exp fvar_tbl)
      | Var'(VarFree(name))->( 
                      let check_exist =(find_fvar name fvar_tbl) in
                      match check_exist with
                      | -1-> let new_fvar = (name,((List.length fvar_tbl)*8)) in 
                                             List.rev(new_fvar ::(List.rev(fvar_tbl)))
                      |_ -> fvar_tbl)
                      
      | _-> fvar_tbl
  let make_fvars_tbl asts = List.fold_left (fun acc curr-> 
                                           (make_Fvar_exp curr acc))  
                                           [("car",0);("cdr",8);("map",16);("boolean?",24);("flonum?",32); ("rational?",40);
                                           ("pair?",48); ("null?",56);("char?",64);("string?",72);
                                           ("procedure?",80);("symbol?",88);("string-length",96);("string-ref",104);("string-set!",112);
                                           ("make-string",120) ;("symbol->string",128);
                                           (* Type conversions *)
                                           ("char->integer",136);("integer->char",144);("exact->inexact",152);
                                           (* Identity test *)
                                           ("eq?",160);
                                           (* Arithmetic ops *)
                                           ("+",168);("*",176);("/",184) ; ("=",192);("<",200);
                                           (* Additional rational numebr ops *)
                                           ("numerator",208);("denominator",216);("gcd",224);("cons",232);("set-car!",240);("set-cdr!",248)] asts;;(*("apply",232)*)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~make fvar table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(*~~~~~~~~~~~~~~~~~~~~~~~~Generate Strings~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
let simple_lambda env_size id lambda_body =

  "push rbx
  push rcx
  push rdx
  push rsi
  MALLOC rax, WORD_SIZE*" ^env_size^ "; allocate new enviorment \n" ^
  "mov rbx,[rbp +2 *WORD_SIZE]
  mov rcx,0 
  env_copy" ^id^":
  cmp rcx, " ^env_size^ "\n" ^
  "je finish_env_copy" ^ id ^"\n"^
  "mov rdx, [rbx + rcx * WORD_SIZE]
  inc rcx
  mov [rax + rcx * WORD_SIZE], rdx
  jmp env_copy" ^id^"\n" ^ 
  "finish_env_copy" ^ id ^ ":
  mov rbx, [rbp + 3 * WORD_SIZE]
  cmp rbx,0
  jne allocate_args" ^ id ^ "\n"^
  "mov rdx, SOB_NIL_ADDRESS
  jmp finish_copy_args" ^ id ^ "\n"^
  "allocate_args" ^ id ^ ":
  shl rbx,3
  MALLOC rdx, rbx
  shr rbx,3
  mov rcx,0
  copy_args"^id^":
  cmp rcx,rbx
  je finish_env_copy" ^ id ^ "\n" ^"
  mov rsi, PVAR(rcx)
  mov [rdx + rcx *WORD_SIZE ],rsi
  inc rcx
  jmp copy_args" ^ id ^ "\n" ^
  "finish_copy_args" ^ id ^ ":
  mov [rax + 0 * WORD_SIZE] , rdx ;place at envorment 0
  mov rbx,rax
  MAKE_CLOSURE(rax, rbx, Lcode" ^ id  ^ ")\n" ^
  "
  pop rsi
  pop rdx
  pop rcx
  pop rbx
  jmp Lcont"^id ^ "\n" ^ 
  "Lcode"^id^":
    push rbp
    mov rbp,rsp \n" ^
    lambda_body ^"\n" ^
    "leave
    ret
    Lcont" ^id ^ ":\n"




  
  
  

  ;;



(*~~~~~~~~~~~~~~~~~~~~~~~~~~~GENERATE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)






  let make_lazy_list initial next =
    let lazy_list current ()= 
      let result = !current in
      current := (next !current); result 
      in lazy_list (ref initial);;
  let new_id=make_lazy_list 0 (function i -> i+1);;

  let rec generate consts fvars env_size e = 
    match e with 
    | Const' (cns) ->  let addr = find_const_addr cns consts in
                    if (addr == -1) then raise X_syntax_error
                    else "mov rax, const_tbl+" ^ (string_of_int addr) ^" \n"
    | Seq' (exps)-> 
      (String.concat "\n" (List.map (fun eps-> (generate consts fvars env_size eps)) exps)) (*check if need to seperate with \n*) 
    | If'  (test,dit,dif) ->
                           let id=(string_of_int (new_id())) in
                            (generate consts fvars env_size test)^
                            "cmp rax, SOB_FALSE_ADDRESS\n"^
                            "je Lelse" ^ id ^ "\n"^
                            (generate consts fvars env_size dit)^
                            "jmp Lexit" ^ id ^ "\n"^
                            "Lelse" ^ id ^ ":\n"^
                            (generate consts fvars env_size dif)^
                            "Lexit" ^ id ^ ":\n"
    | Var'(VarParam(_, minor))->
                    "mov rax, qword [rbp + WORD_SIZE * (4 + "^(string_of_int minor)^")]"^" \n"
    | Set'((VarParam(_, minor)),eps) ->
                    (generate consts fvars env_size eps)^
                    "mov qword [rbp + WORD_SIZE ∗ (4 +"^(string_of_int minor)^")], rax"^"\n"^
                    "mov rax, SOB_VOID_ADDRESS"^"\n"
    | Var'(VarBound(_, major, minor)) ->
                    "mov rax, qword [rbp + WORD_SIZE * 2]"^"\n"^
                    "mov rax, qword [rax + WORD_SIZE * " ^ (string_of_int major)^"]"^"\n" ^
                    "mov rax, qword [rax + WORD_SIZE * " ^(string_of_int minor)^"]"^"\n" 
    |Set'((VarBound(_,major, minor)),z)->
                    (generate consts fvars env_size z)^
                    "mov rbx, qword [rbp + WORD_SIZE * 2]"^"\n"^
                    "mov rbx, qword [rbx + WORD_SIZE * "^ (string_of_int major)^"]"^"\n"^
                    "mov qword [rbx + WORD_SIZE * "^ (string_of_int minor)^"], rax"^"\n"^
                    "mov rax, SOB_VOID_ADDRESS"^"\n"
    | Var'(VarFree(v)) -> 
                    let addr = find_fvar v fvars in
                    if (addr = -1) then raise X_syntax_error
                    else "mov rax, qword [fvar_tbl+"^(string_of_int addr)^"]\n" 

    | BoxGet'(v)->
                    (generate consts fvars env_size (Var' v))^
                    "mov rax, qword [rax]"^"\n"

    | BoxSet'(v,eps)->
                    (generate consts fvars env_size eps)^
                    "push rax"^"\n"^
                    (generate consts fvars env_size (Var' v))^
                    "pop qword [rax]"^"\n"^
                    "mov rax, SOB_VOID_ADDRESS"^"\n"
    |Or' (lst) ->     let exit_label = "Lexit" ^ (string_of_int (new_id())) in
                      let last_arg =  (List.nth lst ((List.length lst) - 1)) in
                      (String.concat "" (List.map 
                        (fun epsilon -> (generate consts fvars env_size epsilon) ^ 
                                        "cmp rax, SOB_FALSE_ADDRESS\n"^
                                        "jne "^exit_label^"\n" ) (List.rev(List.tl(List.rev lst))) ))^
                                        (generate consts fvars env_size last_arg) ^
                                        exit_label ^":\n"
    | Set'((VarFree(v)),eps) ->   
              let addr = find_fvar v fvars in
                      if (addr = -1)  then raise X_syntax_error 
                      else (generate consts fvars env_size eps)^
                      "mov qword [fvar_tbl + "^(string_of_int addr)^"], rax \n"^
                      "mov rax, SOB_VOID_ADDRESS"^"\n" 
    | Def'((VarFree(v)),eps) ->   
    let addr = find_fvar v fvars in
            if (addr = -1)  then raise X_syntax_error 
            else (generate consts fvars env_size eps)^
            "mov qword [fvar_tbl + "^(string_of_int addr)^"], rax \n"^
            "mov rax, SOB_VOID_ADDRESS"^"\n" 
    | LambdaSimple'(_,body)-> let id=(string_of_int (new_id())) in
                              let lambda_body=(generate consts fvars (env_size+1) body) in
                              (simple_lambda (string_of_int env_size) id lambda_body)
    | Box' (VarParam (x, minor))->
            "MALLOC rbx, WORD_SIZE"^"\n"^
            (generate consts fvars env_size (Var'(VarParam (x,minor))))^
            "mov qword [rbx] , rax
            mov rax, rbx "^"\n" 
    | Applic'(proc ,args) ->raise X_not_yet_implemented_code_gen
    | ApplicTP'(proc ,args) -> raise X_not_yet_implemented_code_gen
    | LambdaOpt'( _,_,body)-> raise X_not_yet_implemented_code_gen

    |_ ->  raise X_not_yet_implemented_code_gen

    |_ ->  raise X_this_should_not_happen;;
end;;

(* type expr' =
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);; *)