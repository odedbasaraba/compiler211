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
  let get_offset const_tbl_and_ofsset=
    match const_tbl_and_ofsset with 
    |(_,num) -> num
    |_-> raise X_this_should_not_happen;;


  let get_const_tbl const_tbl_and_ofsset=
    match const_tbl_and_ofsset with 
    |(const_tbl,_) ->const_tbl
    
    |_-> raise X_this_should_not_happen;;

  let add_to_const_table const tbl offset assembly_expression size = 
    ((List.append tbl ([(const, (offset, assembly_expression))])),[(offset+size)])
    
  let rec eval_const ast const_tbl_and_ofsset= 
    let offset=get_offset const_tbl_and_ofsset in
    let const_tbl= get_const_tbl const_tbl_and_ofsset in 
      match ast with
      | If'(test,dit,dif) -> 
        let new_const_tbl_and_ofsset= (eval_const test new_const_tbl_and_ofsset) in
        let new_const_tbl_and_ofsset= (eval_const dit new_const_tbl_and_ofsset) in
          (eval_const dif new_const_tbl_and_ofsset)
      | LambdaSimple'(args,body) -> (eval_const body const_tbl_and_ofsset)
      | LambdaOpt'(args,arg,body) -> (eval_const body const_tbl_and_ofsset)
      | Set' (y, z)-> 
        let const_tbl = (eval_const y const_tbl_and_ofsset) in
        (eval_const z const_tbl_and_ofsset) 
      | Or' (args) -> (List.fold_left (fun acc curr-> (eval_const curr acc))  const_tbl_and_ofsset args)
      | Def' (variable,y) ->
        let new_const_tbl_and_ofsset= (eval_const variable const_tbl_and_ofsset) in
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
        const_Handler const const_tbl offset 
      | exp -> const_tbl_and_ofsset

  and find_const_addr var tbl =
  match tbl with 
  |[(v, (addr,_))::tl -> if(sexpr_eq var v) then addr else (find_const_addr var tl) 
  |[]-> -1

  and const_Handler const const_tbl offset=
  let address_at_const_table= (find_const_addr const const_tbl) in  
  match address_at_const_table with
  | -1-> 
  (
    match const with
    | Sexpr(Char x)-> (add_to_const_table const tbl offset ("MAKE_LITERAL_CHAR("^(string_of_int(Char.code x))^")") 2)
    | Sexpr(Number(Fraction(x,y))) -> (add_to_const_table const tbl offset ("MAKE_LITERAL_RATIONAL(" ^ (string_of_int x) ^ (string_of_int y) ")") 17 )
    | Sexpr(Number(Float(x))) ->  (add_to_const_table const tbl offset ("MAKE_LITERAL_FLOAT(" ^ (string_of_float x) ^ ")") 9 )
    | Sexpr(Pair(x,y)) -> let newtbl = (const_Handler (Sexpr x) tbl offset ) in 
                        (match newtbl with 
                        |(tbl,offset)-> let newtbl = (const_Handler (Sexpr y) newtbl offset ) in
                                          let add_of_x = find_const_addr (Sexpr x) newtbl in
                                            let add_of_y = find_const_addr (Sexpr y) newtbl in
                                            (add_to_const_tbl const newtbl ("MAKE_LITERAL_PAIR(const_tbl+"
                                              ^ (string_of_int add_of_x) ^ ", const_tbl+"^ (string_of_int add_of_y)^")") 17) 
                        |_-> raise X_this_should_not_happen)
                            
    | Sexpr(String x) -> (add_to_const_tbl const tbl ("MAKE_LITERAL_STRING "^(make_string_to_fvar x)^" ") ((String.length x)+9))
    | Sexpr(Symbol x) -> let newtbl = (const_Handler (Sexpr(String x)) tbl ) in 
                          let add_of_x = find_const_addr (Sexpr(String x)) newtbl in
                          (add_to_const_tbl const newtbl ("MAKE_LITERAL_SYMBOL(const_tbl+"
                          ^ (string_of_int add_of_x) ^ ")") 9) 
    |_->(const_tbl,offset)
  | _ -> (const_tbl,offset)

  let make_consts_tbl asts = 
    let cnst_tbl=List.fold_left (fun acc curr-> (eval_const curr acc))
   ([(Void, (0, "MAKE_VOID"));
    (Sexpr(Nil), (1, "MAKE_NIL"));
    (Sexpr(Bool false), (2, "MAKE_BOOL(0)"));
    (Sexpr(Bool true), (4, "MAKE_BOOL(1)"))],6) asts 
    
  let make_fvars_tbl asts = raise X_not_yet_implemented;;
  let generate consts fvars e = raise X_not_yet_implemented;;
end;;

