
#use "pc.ml";;

open PC;;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Fraction of int * int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;
  
  let rec gcd a b =
    if      a = 0 then b
    else if b = 0 then a
    else if a > b then gcd b (a mod b)
    else               gcd a (b mod a)

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;

module Reader: sig
  val read_sexprs : string -> sexpr list
end
= struct
  
  let digit str = range '0' '9' str ;;
  (*symbol parser*)
let char_alphabet str = disj (range 'a' 'z') (range 'A' 'Z') str;;

let special_symbol str =one_of "!$^*-_=+><?/:" str;; 

let symbol_char_no_dot str = 
(disj_list[
     digit;
     char_alphabet;
     special_symbol]) str;;

let dot str = (char '.') str;;

let symbol_char str = disj symbol_char_no_dot dot str ;;

let cc= pack (caten symbol_char (plus (symbol_char))) (fun (l,r)-> List.append [l] r) ;;
let symbol str = pack(disj cc (pack symbol_char_no_dot (fun a->[a]))) (fun a-> Symbol(String.lowercase_ascii(list_to_string a))) str;;
(* struct Reader *)

(*\\\\\\\\\\\\\\\\\\\\\\\\\\\\bool parser///////////////////////////////*)
let _bool_ str= pack (disj (word_ci "#T") (word_ci "#F"))  
          ( fun l-> match l with
          | ['#';'T']-> Bool true
          | ['#';'t']-> Bool true
          | ['#';'F']-> Bool false
          | ['#';'f']-> Bool false
          | _->raise X_this_should_not_happen) str;;

let string_meta_char str = pack (disj_list[(caten (char '\\')(char 'r'));
                                     (caten (char '\\')(char 'n'));
                                     (caten (char '\\')(char 't'));
                                     (caten (char '\\')(char 'f'));
                                     (caten (char '\\')(char '\\'));
                                     (caten (char '\\')(char '\"'));]) (fun (slash,meta_char)-> match meta_char with
                                                                                         |'r'-> (Char.chr 13)
                                                                                         |'n'-> (Char.chr 10)
                                                                                         |'t'-> (Char.chr 9)
                                                                                         |'f'-> (Char.chr 12)
                                                                                         |'\\'-> (Char.chr 92)
                                                                                         |'\"'-> (Char.chr 34)
                                                                                         |_-> raise X_this_should_not_happen) str;;
(*\\\\\\\\\\\\\\natural-number/////////////////////*)
let nat str = plus digit str  ;;
(*\\\\\\\\\\\\\\operator///////////////////////////*)



 let operator str = pack (maybe (disj (char '+') (char '-')))(fun op -> match op with
                                                                | None ->'+'
                                                                | Some oper->oper)str;;
let integer str = pack (caten operator nat) (fun (l,r)->l::r) str;;
let integer_parser_to_number str = (pack integer 
                (fun integer_array-> Number(Fraction(int_of_string(list_to_string integer_array),1))) )str


let float_parser str = pack (caten (caten integer dot) nat) (fun ((a,b),c)->
         let num_list = List.append (List.append a [b]) c in 
         Number (Float (float_of_string (list_to_string num_list)))) str;;
let frac str = pack (caten (caten integer (char '/')) nat) (fun ((a,b),c)->
         let num1 = (int_of_string (list_to_string (a))) in
         let num2 = (int_of_string (list_to_string (c))) in
         let the_gcd = gcd (abs (num1)) (abs (num2)) in
        Number (Fraction (num1/the_gcd ,num2/the_gcd)))str;;
let number_parser str= (disj_list[float_parser;frac;integer_parser_to_number]) str;;

(*\\\\\\\\\\\\\\\\\\\\\StringLiteralChar///////////////////////*)

let scien_nat str = not_followed_by nat (diff (diff nt_any digit) (char_ci 'e')) str ;;

let scien_int str =pack (caten operator (caten (caten scien_nat (char_ci 'e')) (caten operator nat)) ) (*for example "-32e+23" ((-,((32,e),(+,23))) *)

            (fun (op1,((l,e),(op,r)))->

            Number (Float (float_of_string (list_to_string (List.append [op1] (List.append (List.append l [e]) (List.append [op] r))))))) str;;

               

let scien_float str = (*32.222e+22-> ((((32,(.,222),e),+),22)) *)

let nt_after_dot = (caten (caten dot scien_nat) (char_ci 'e')) in

let nt_left = (caten nat nt_after_dot) in

let nt = (caten (caten nt_left operator) nat)in

let nt = pack nt (fun (((left,((dot,rest),e)),op),exp)->

                let num= (List.append (List.append left ['.']) rest) in

                float_of_string (list_to_string (List.append (List.append num [e]) (List.append [op] exp))))in

                pack (caten operator nt) 

                     (fun (op,num)->

                     if op = '-' then Number (Float (-1.0 *. num)) 

                     else Number (Float num)) str ;;

let science_extenstion str= (disj  scien_float scien_int) str;;


(* //////////////////////////////////////////////////////////// - scince exten *)
let number_parser_after_extension str= (disj  science_extenstion number_parser) str ;;
(* //////////////////////////////////////////////////////////// - final number parser after extens *)
let nt_to_remove str = disj (char '\\') (char '\"') str;; (*chars to disclude*)
let string_literal_char str = pack (diff nt_any nt_to_remove) (fun e -> e) str;;
(*\\\\\\\\\\\\\\\\\\\\\\\\StringChar//////////////////////////////*)
let string_char str = disj string_literal_char string_meta_char str;;
(* \\\\\\\\\\\\\\\\\\\\\\\\\stringParser/////////////////////////////*)
let coma str = char '\"' str;;
let string_nt str = caten (caten coma  (star string_char)) coma str;;
let _string_ str = pack string_nt (fun ((a,b),c)->
      let string_in_list = b in
      String (list_to_string string_in_list)) str ;;

let _string2_ str = pack string_nt (fun ((a,b),c)->
let string_in_list = b in
String (list_to_string string_in_list)) str ;;


let char_prefix str =pack (caten (char '#') (char '\\')) (fun (a,b)-> a::[b]) str;; 

let named_char str = pack (disj_list[word_ci "return";
                               word_ci "newline";
                               word_ci "tab";
                               word_ci "page";
                               word_ci "nul";
                               word_ci "space"])(fun named_ch->
                               let lower =String.lowercase_ascii (list_to_string named_ch) in
                               match lower with 
                               | "return" -> Char (Char.chr 13)
                               | "newline" -> Char (Char.chr 10)
                               | "tab" -> Char (Char.chr 9)
                               | "page" -> Char (Char.chr 12)
                               | "nul" -> Char (Char.chr 0)
                               | "space" -> Char (Char.chr 32)
                               | _ -> raise X_this_should_not_happen) str ;;
(*\\\\\\\\\\\\\\\\\\\\\\\\\\\VisibleSimpleChar////////////////////// *)
let visible_simple_char str = pack (diff nt_any nt_whitespace) (fun vschar-> Char vschar) str ;;
(* (\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\theChar//////////////////////////////) *)


let final_char str = pack (caten char_prefix (disj named_char visible_simple_char))
                    (fun (l,r)->r) str ;;

(* //////////////////////////////////////////////// *)
let throw_away _= []
let quotes str= pack(disj 
(pack(caten (char ',')  (char '@'))

(fun (a,b)->a::[b])) 
(pack (disj_list([char '\'';char '`';char ','])) (fun a->[a]))) 
(fun quote->
match quote with
| [',';'@']->"unquote-splicing"
| [',']->"unquote"
| ['\'']->"quote"
| ['`']->"quasiquote"
| _ ->raise X_this_should_not_happen) str;;

let white_spaces_star str= (star (char ' ')) str;;
let left_parenthasis_with_white_spaces str=(pack
(caten_list[white_spaces_star;
                        pack(char '(') (fun a->[a]);  white_spaces_star])
                        throw_away) str
;;
let right_parenthasis_with_white_spaces str=(pack
(caten_list[white_spaces_star;
                        pack(char ')') (fun a->[a]);  white_spaces_star])
                        throw_away )str
;;
let rec sublist b e l = 
match l with
[] -> failwith "sublist"
| h :: t -> 
let tail = if e=0 then [] else sublist (b-1) (e-1) t in
if b>0 then tail else h :: tail
;;
(* ///////////////////////////////////////////////////// *)
let nt_whitespaces = star nt_whitespace;;
let nt_whitespaces_plus = plus nt_whitespace;;
let make_paired nt_left nt_right nt =
let nt = caten nt_left nt in
let nt = pack nt (function (_, e) -> e) in
let nt = caten nt nt_right in
let nt = pack nt (function (e, _) -> e) in
nt;;

let make_spaced nt =
make_paired nt_whitespaces nt_whitespaces nt;;

let tok_lparen = make_spaced ( char '(');;

let tok_rparen = make_spaced ( char ')');;
let tok_dot = make_spaced (char '.');;
let pair_up_list lst=List.fold_right ( fun a b -> Pair(a,b))   lst    Nil;;
let pair_up_list_2 lst cdr=match cdr with
                            |[tail]->List.fold_right ( fun a b -> Pair(a,b))   lst    tail
                            |_->raise X_this_should_not_happen;;

(* /////////////////////////////////////////////////////// Raviv  - parenthasis *)
let throw_away _= [];;
let sexpr_comments_prefix str = (pack (make_spaced (caten (char '#') (char ';') )) (throw_away))  str;;
let line_cmt str = (caten (char ';') (star (diff nt_any (disj (char '\n') (pack nt_end_of_input (fun e->'\n')))))) str;; 
let line_comment str =(caten (pack (char ';') throw_away )(disj (star (diff nt_any (caten (char '\\')(char 'n')) )) (star(diff nt_any nt_end_of_input)))) str ;;
(* //////////////////////////////////////////////////////// Raviv - comments *)
let rec sexpr_pars str= make_spaced  (
   (  
pack (disj_list 
  [_bool_;
  final_char;
  (not_followed_by number_parser_after_extension symbol); 
  _string_;
  symbol;
  _quoteds_;
  _list_;
  _dotted_list_
  ]) 
  (fun a->[a]))


(* (pack (caten(pack (maybe line_cmt) (fun _->[])) sexpr_pars) (fun lst->match lst with 
                                                                      | ([],b)->b
                                                                      |_ -> raise X_this_should_not_happen)) *)
)
str

and sexpr_comments str=(pack(caten sexpr_comments_prefix (disj sexpr_pars (pack(caten_list[sexpr_comments_prefix;sexpr_comments_recursive;sexpr_pars])(fun a->[]) )))
                        (fun a->[]))str
and sexpr_comments_recursive str= (pack(disj sexpr_pars (pack(caten_list[sexpr_comments_prefix;sexpr_comments_recursive;sexpr_pars]) (fun a->[])   )) 
                                      (fun a->[]))str
and star_sexpr_without_comments str= (pack (star ( disj (pack line_cmt throw_away) (disj sexpr_comments sexpr_pars) )   ) 
                                      (fun lst->List.filter (fun item -> match item with 
                                                                        | []->false
                                                                        | _ -> true) lst))str
and comments str =(pack(star (disj (pack line_cmt throw_away) sexpr_comments  )) throw_away)str
and new_star_sexpr_without_comments str =  (pack (star (make_paired comments comments  sexpr_pars)) 
                                                (fun lst->List.filter (fun item -> match item with 
                                                                        | []->false
                                                                        | _ -> true) lst) )str
and plus_without_comments str=(pack (plus (make_paired comments comments  sexpr_pars)) 
                                                (fun lst->List.filter (fun item -> match item with 
                                                                        | []->false
                                                                        | _ -> true) lst) )str
and _quoteds_ str=pack(caten quotes sexpr_pars )
(fun a->
match a with 
| name,[sexpr]->Pair(Symbol(name),Pair(sexpr,Nil))
| _ -> raise X_no_match) str

and  _list_ str= pack (make_paired tok_lparen tok_rparen (
              pack(new_star_sexpr_without_comments)
              (fun lst-> List.map(fun a -> match a with 
                                          |[b]->b
                                          |_ ->raise X_this_should_not_happen) lst)
              ))
(pair_up_list)  str

and _dotted_list_ str=(pack (caten(make_paired tok_lparen tok_dot (
  pack(plus_without_comments)
  (fun lst-> List.map(fun a -> match a with
  |  [b]->b
  | _->raise X_this_should_not_happen) lst)
  ))   
(make_paired nt_whitespaces tok_rparen sexpr_pars) )
  (fun (lst,last_item)-> pair_up_list_2 lst last_item ))
   str;;


let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;


  let read_sexprs string = 
    let ((a,b),s) =(caten (pack (new_star_sexpr_without_comments) 
    (fun lst-> List.map(fun item->match item with
                                |[c]->c
                                | _-> raise X_this_should_not_happen) lst)) (nt_end_of_input) (string_to_list string)) in
    a;;
  
end;; (* struct Reader *)
