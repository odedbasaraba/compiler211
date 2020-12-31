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
     For example: [((Sexpr(Nil), (1, "T_NIL")),1)]
added one more int to the tuple to represent the end on the object
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string * int)) list
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
  val generate : (constant * (int * string * int)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct

  let rec eval_const ast const_tbl= 
    match ast with
    | If'(test,dit,dif) -> 
      let const_tbl = (eval_const test const_tbl) in
       let const_tbl = (eval_const dit const_tbl) in
    | LambdaSimple'(args,body) -> (eval_const body const_tbl)
    | LambdaOpt'(args,arg,body) -> (eval_const body const_tbl)
      (eval_const dif const_tbl)
    | Set' (y, z)-> 
      let const_tbl = (eval_const y const_tbl) in
      (eval_const z const_tbl) 
    | Or' (args) -> (List.fold_left (fun acc curr-> (eval_const curr acc))  const_tbl args)
    | Def' (variable,y) ->
      let const_tbl = (eval_const variable const_tbl) in
      (eval_const y const_tbl) 
    | Applic' (op,args)-> 
      let const_tbl= (List.fold_left (fun acc curr-> (eval_const curr acc))  const_tbl args) in
      (eval_const op const_tbl)
    | ApplicTP' (op,args) ->
      let const_tbl= (List.fold_left (fun acc curr-> (eval_const curr acc))  const_tbl args) in
      (eval_const op const_tbl)
    | Seq' (body)-> (List.fold_left (fun acc curr-> (eval_const curr acc))  const_tbl body)
    | BoxSet'(var,exp)-> (eval_const exp const_tbl)
    | Const'(const)-> 
      const_Handler const const_tbl 
    | exp -> const_tbl

  and cnst_founder var tbl =
  (List.find (fun curr-> 
            let ((type_,(addr,_)),_) = curr in
            var=type_)
    const_table);;

  and const_Handler const tbl=
  let address_at_const_table= (find_const const table) in  
+  match address_at_const_table with
+  | Some((prefix,(address,command)),size) -> table
+  | None-> 
+  (
+    match const with
+    | Sexpr(Bool x)-> table
+    | Sexpr(Char x)-> (add_to_const_table const table ("MAKE_LITERAL_CHAR("^(string_of_int(Char.code x))^")") 2)
+    | Sexpr(Number(Int(x))) -> (add_to_const_table const table ("MAKE_LITERAL_INT(" ^ (string_of_int x) ^ ")") 9 )
+    | Sexpr(Number(Float(x))) -> (add_to_const_table const table ("MAKE_LITERAL_FLOAT(" ^ (string_of_float x) ^ ")") 9 )
+    | Sexpr(Pair(x,y)) -> let newtbl = (dealing_with_const_before_enter_to_table (Sexpr x) table ) in 
+                            let newtbl = (dealing_with_const_before_enter_to_table (Sexpr y) newtbl ) in
+                              let add_of_x = find_address (Sexpr x) newtbl in
+                                let add_of_y = find_address (Sexpr y) newtbl in
+                                (add_to_const_table const newtbl ("MAKE_LITERAL_PAIR(const_tbl+"
+                                   ^ (string_of_int add_of_x) ^ ", const_tbl+"^ (string_of_int add_of_y)^")") 17) 
+    | Sexpr(String x) -> (add_to_const_table const table ("MAKE_LITERAL_STRING "^(make_string_to_fvar x)^" ") ((String.length x)+9))
+    | Sexpr(Symbol x) -> let newtbl = (dealing_with_const_before_enter_to_table (Sexpr(String x)) table ) in 
+                          let add_of_x = find_address (Sexpr(String x)) newtbl in
+                          (add_to_const_table const newtbl ("MAKE_LITERAL_SYMBOL(const_tbl+"
+                          ^ (string_of_int add_of_x) ^ ")") 9) 
  
  let make_consts_tbl asts = 
    let cnst_tbl=List.fold_left (fun acc curr-> (eval_const curr acc))
   [((Void, (0, "MAKE_VOID")),1);
    ((Sexpr(Nil), (1, "MAKE_NIL")),2);
    ((Sexpr(Bool false), (2, "MAKE_BOOL(0)")),4);
    ((Sexpr(Bool true), (4, "MAKE_BOOL(1)")),6)] asts 
  let make_fvars_tbl asts = raise X_not_yet_implemented;;
  let generate consts fvars e = raise X_not_yet_implemented;;
end;;

