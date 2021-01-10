#use "semantic-analyser.ml";;

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
  val generate : (constant * (int * string )) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~make const table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
  let get_offset const_tbl_and_ofsset=
    match const_tbl_and_ofsset with 
    |(_,num) -> num
    |_-> raise X_this_should_not_happen;;


  let get_const_tbl const_tbl_and_ofsset=
    match const_tbl_and_ofsset with 
    |(const_tbl,_) ->const_tbl
    
    |_-> raise X_this_should_not_happen;;

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
        let new_const_tbl_and_ofsset= (eval_const dit const_tbl_and_ofsset) in
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
      | Sexpr(Number(Fraction(x,y))) -> ((add_to_const_table const const_tbl offset ("MAKE_LITERAL_RATIONAL(" ^ (string_of_int x) ^ " " ^ (string_of_int y) ^ ")")  ),(offset+17))
      | Sexpr(Number(Float(x))) ->  ((add_to_const_table const const_tbl offset ("MAKE_LITERAL_FLOAT(" ^ (string_of_float x) ^ ")")) ,(offset+9) )
      | Sexpr(Pair(x,y)) -> let newtbl_and_offset = (const_Handler (Sexpr x) const_tbl offset )  in 
                              let add_of_x = offset in (* just change the name in order to have clean code *)
                                (match newtbl_and_offset with 
                                |(tbl,add_of_y)-> let newtbl_and_offset = (const_Handler (Sexpr y) tbl add_of_y ) in
                                                    (match newtbl_and_offset with 
                                                    |(tbl,offset_of_pair) -> ((add_to_const_table const tbl offset_of_pair ("MAKE_LITERAL_PAIR("
                                                      ^ (string_of_int add_of_x) ^ ", " ^ (string_of_int add_of_y)^ ")")),(offset_of_pair+17) )
                                |_-> raise X_this_should_not_happen))
                              
      | Sexpr(String x) -> ((add_to_const_table const const_tbl offset ("MAKE_LITERAL_STRING "^(make_string_char_list x)^" ")), ((String.length x)+9))
      | Sexpr(Symbol x) -> let newtbl_and_offset = (const_Handler (Sexpr(String x)) const_tbl offset) in 
                              let add_of_x= offset in
                              match newtbl_and_offset with 
                              | (tbl,symbol_offset)->  ((add_to_const_table const tbl symbol_offset ("MAKE_LITERAL_SYMBOL(const_tbl+"
                            ^ (string_of_int add_of_x) ^ ")") ),(symbol_offset+ 9))
                              |_-> raise X_this_should_not_happen
    
    
      
    | _ -> (const_tbl,offset)
    )
    | _ -> (const_tbl,offset)
    ;;
  let make_consts_tbl asts = 
   let ze = (List.fold_left (fun acc curr-> (eval_const curr acc))
   ([(Void, (0, "MAKE_VOID"));
    (Sexpr(Nil), (1, "MAKE_NIL"));
    (Sexpr(Bool false), (2, "MAKE_BOOL(0)"));
    (Sexpr(Bool true), (4, "MAKE_BOOL(1)"))],6) asts) in
    match ze with 
    |(tbl,mashu)->tbl
    |_ -> [];; 
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~make const table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~make fvar table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
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
                                           [("car",0);("cdr",8);("map",16);("boolean?",24);("flonum?",32); ("rational?"40);
                                           ("pair?",48); ("null?",56);("char?",64);("string?",72);
                                           ("procedure?",80);("symbol?",88);("string-length",96);("string-ref",104);("string-set!",112);
                                           ("make-string",120) ;("symbol->string",128);
                                           (* Type conversions *)
                                           ("char->integer",136);("integer->char",144); "exact->inexact", "exact_to_inexact";
                                           (* Identity test *)
                                           "eq?", "eq?";
                                           (* Arithmetic ops *)
                                           "+", "add"; "*", "mul"; "/", "div"; "=", "eq"; "<", "lt";
                                           (* Additional rational numebr ops *)
                                           "numerator", "numerator"; "denominator", "denominator"; "gcd", "gcd";] asts;;
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~make fvar table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~GENERATE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

  let rec generate consts fvars e = 
    match e with 
    | Const' (cns)->  let addr = find_const_addr cns consts in
                    if (addr = -1) raise X_syntax_error
                    else "mov rax, const_tbl+" ^ (string_of_int addr) ^" \n "
    | Var'(VarParam'(_, minor))->
                    "mov rax, qword [rbp + 8 * (4 + "^(string_of_int minor)^")]"^" \n "
    | Set(Var'(VarParam'(_, minor)),eps) ->
                    (generate consts fvars eps)^
                    "mov qword [rbp + 8 ∗ (4 +"^(string_of_int minor)^")], rax"^"\n"^
                    "mov rax, SOB_VOID_ADDRESS"^"\n"
    | Var'(VarBound(_, major, minor)) ->
                    "mov rax, qword [rbp + 8 * 2]"^"\n"^
                    "mov rax, qword [rax + 8 * " ^ (string_of_int major)^"]"^"\n" ^
                    "mov rax, qword [rax + 8 * " ^(string_of_int minor)^"]"^"\n" 
    |Set'(Var'(VarBound(_,major, minor)),z)->
                    (generate consts fvars z)^
                    "mov rbx, qword [rbp + 8 * 2]"^"\n"^
                    "mov rbx, qword [rbx + 8 * "^ (string_of_int major)^"]"^"\n"^
                    "mov qword [rbx + 8 * "^ (string_of_int minor)^"], rax"^"\n"^
                    "mov rax, SOB_VOID_ADDRESS"^"\n"
    | Var'(VarFree'(v)) -> 
                    let addr = find_fvar v fvars in
                    if (addr = -1) raise X_syntax_error
                    else "mov rax, qword ["^(string_of_int addr)^"]\n" 
    | Set(Var'(VarFree'(v)),eps) ->   
                    let addr = find_fvar v fvars in
                    if (addr = -1) raise X_syntax_error 
                    else (generate consts fvars eps)^
                    "mov qword [fvar_tbl + "^(string_of_int addr)^"], rax \n"^
                    "mov rax, SOB_VOID_ADDRESS"^"\n" 
    | Seq' (exps)-> 
                    (String.concat "\n" (List.map (fun eps-> (generate consts fvars eps)) exps)) (*check if need to seperate with \n*) 
    | BoxGet'(Var'(v))->
                    (generate consts fvars Var'(v))^
                    "mov rax, qword [rax]"^"\n"
    | BoxSet'(Var'(v),eps)->
                    (generate consts fvars eps)^
                    "push rax"^"\n"
                    (generate consts fvars Var'(v))^
                    "pop qword [rax]"^"\n"
                    "mov rax, SOB_VOID_ADDRESS"^"\n"
    

                                                          ;;
end;;

