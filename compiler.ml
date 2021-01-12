#use "code-gen.ml";;
#use "prims.ml";;

(* 
   Auxiliary function to load the contents of a file into a string in memory.
   Note: exceptions are not handled here, and are expected to be handled 
   by the caller. We already took care of this in main code block below.
 *)
let file_to_string f =
  let ic = open_in f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s;;

(* This procedure creates the assembly code to set up the runtime environment for the compiled
   Scheme code. *)
let make_prologue consts_tbl fvars_tbl =
  (* The table defines a mapping from the names of primitive procedures in scheme to their labels in 
     the assembly implementation. *)
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
    "car","car"; "cdr","cdr"; "cons","cons"; "set-car!","set_car";"set-cdr!","set_cdr";
  ] in
  let make_primitive_closure (prim, label) =
    (* This implementation assumes fvars are addressed by an offset from the label `fvar_tbl`.
       If you use a different addressing scheme (e.g., a label for each fvar), change the 
       addressing here to match. *)
    "MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, " ^ label  ^ ")\n" ^
      "mov [fvar_tbl+" ^  (string_of_int (List.assoc prim fvars_tbl)) ^ "], rax" in
  let constant_bytes (c, (a, s)) =
    (* Adapt the deconstruction here to your constants data generation scheme.
       This implementation assumes the bytes representing the constants are pre-computed in
       the code-generator and stored in the last column of a three-column constants-table structure *)
    s in
";;; All the macros and the scheme-object printing procedure
;;; are defined in compiler.s
%include \"compiler.s\"

section .bss
;;; This pointer is used to manage allocations on our heap.
malloc_pointer:
    resq 1

;;; here we REServe enough Quad-words (64-bit \"cells\") for the free variables
;;; each free variable has 8 bytes reserved for a 64-bit pointer to its value
fvar_tbl:
    resq " ^ string_of_int (List.length fvars_tbl) ^ "

section .data
const_tbl:
" ^ (String.concat "\n" (List.map constant_bytes consts_tbl)) ^ "

;;; These macro definitions are required for the primitive
;;; definitions in the epilogue to work properly
%define SOB_VOID_ADDRESS const_tbl+" ^ (string_of_int (fst (List.assoc Void consts_tbl))) ^ "
%define SOB_NIL_ADDRESS const_tbl+" ^ (string_of_int (fst (List.assoc (Sexpr Nil) consts_tbl))) ^ "
%define SOB_FALSE_ADDRESS const_tbl+" ^ (string_of_int (fst (List.assoc (Sexpr (Bool false)) consts_tbl))) ^ "
%define SOB_TRUE_ADDRESS const_tbl+" ^ (string_of_int  (fst (List.assoc (Sexpr (Bool true)) consts_tbl))) ^ "

global main
section .text
main:
    ;; set up the heap
    mov rdi, GB(2)
    call malloc
    mov [malloc_pointer], rax

    ;; Set up the dummy activation frame
    ;; The dummy return address is T_UNDEFINED
    ;; (which a is a macro for 0) so that returning
    ;; from the top level (which SHOULD NOT HAPPEN
    ;; AND IS A BUG) will cause a segfault.
    push SOB_NIL_ADDRESS  ; oded and raviv addition, the magic argument for the dummy frame as well 
    push 0                ; argument count
    push SOB_NIL_ADDRESS  ; lexical environment address
    push T_UNDEFINED      ; return address
    push rbp                    
    mov rbp, rsp                ; anchor the dummy frame

    ;; Set up the primitive stdlib fvars:
    ;; Since the primtive procedures are defined in assembly,
    ;; they are not generated by scheme (define ...) expressions.
    ;; This is where we simulate the missing (define ...) expressions
    ;; for all the primitive procedures.
" ^ (String.concat "\n" (List.map make_primitive_closure primitive_names_to_labels)) ^ "

user_code_fragment:
;;; The code you compiled will be added here.
;;; It will be executed immediately after the closures for 
;;; the primitive procedures are set up.\n";;

let clean_exit =
  ";;; Clean up the dummy frame, set the exit status to 0 (\"success\"), 
   ;;; and return from main
   pop rbp
   add rsp, 4*8
   mov rax, 0

   ret";;

exception X_missing_input_file;;

(* 
   This is the bit that makes stuff happen. It will try to read a filename from the command line arguments
   and compile that file, along with the contents of stdlib.scm.
   The result is printed to stduot. This implementation assumes the compiler user redirects the output to a 
   file (i.e. "ocaml compiler.ml some_file.scm > output.s").
   This assumption is already handled correctly in the provided makefile.
 *)
try
  (* Compile a string of scheme code to a collection of analyzed ASTs *)
  
  let string_to_asts s = List.map Semantics.run_semantics
                           (Tag_Parser.tag_parse_expressions
                              (Reader.read_sexprs s)) in

  (* get the filename to compile from the command line args *)
  let infile = Sys.argv.(1) in  

  (* load the input file and stdlib *)
  (* let code =  (file_to_string "stdlib.scm") ^ (file_to_string infile) in *)
  let code = (file_to_string infile) in

  (* generate asts for all the code *)
  let asts = string_to_asts code in

  (* generate the constants table *)
  let consts_tbl = Code_Gen.make_consts_tbl asts in

  (* generate the fvars table *)
  let fvars_tbl = Code_Gen.make_fvars_tbl asts in  

  (* Generate assembly code for each ast and merge them all into a single string *)
  let generate = Code_Gen.generate consts_tbl fvars_tbl 0 in 
  let code_fragment = String.concat "\n\n"
                        (List.map
                           (fun ast -> (generate ast) ^ "\n\tcall write_sob_if_not_void")
                           asts) in

  (* merge everything into a single large string and print it out *)
  print_string ((make_prologue consts_tbl fvars_tbl)  ^ 
                  code_fragment ^ clean_exit ^
                    "\n" ^ Prims.procs)

(* raise an exception if the input file isn't found *)
with Invalid_argument(x) -> raise X_missing_input_file;;
