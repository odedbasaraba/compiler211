#use "reader.ml";;
#use "semantics_analyser.ml";;
#use "tag-parser.ml";;
#use "code-gen.ml";;
open Reader;;
open Tag_Parser;;
open Semantics;;
open Code_Gen;;
let test_til_3 program=(List.map run_semantics (tag_parse_expressions (read_sexprs program)));;
let test_constans program=make_consts_tbl(List.map run_semantics (tag_parse_expressions (read_sexprs program)));;
let test_fvars program=make_fvars_tbl(List.map run_semantics (tag_parse_expressions (read_sexprs program)));;

