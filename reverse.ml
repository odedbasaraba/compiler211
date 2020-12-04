let unread_number = function
| Fraction(n1,n2) -> Printf.sprintf "%d/%d" n1 n2
| Float(f) -> Printf.sprintf "%f" f

let unread_char c = 
match c with
| '\n' -> "#\\newline"
| '\t' -> "#\\tab"
| ' ' -> "#\\space"
(* Fuck it...Skipping the rest of the named chars *)
| _ -> Printf.sprintf "#\\%c" c;;

let rec unread sexpr = 
match sexpr with
| Nil -> "()"
| Bool(true) -> "#t"
| Bool(false) -> "#f"
| Number(n) -> unread_number n
| Char(c) -> unread_char c
| String(s) -> Printf.sprintf "\"%s\"" s (* Fuck it...Skipping string meta chars *)
| Symbol(s) -> s
| Pair(s1, s2) -> unread_list sexpr

and unread_list = function
| Pair(a, b) -> Printf.sprintf " %s%s" (unread a) (unread_list b)
| Nil -> ")"
| sexpr -> Printf.sprintf "%s)" (unread sexpr);;

let untag expr = 
let rec untag_rec expr is_nested = 
match expr with
| Const(Sexpr(s)) -> unread s
| Const(Void) when is_nested -> "#<void>"
| Const(Void) -> ""
| Var(name) -> unread (Symbol(name))
| If(test, dit, dif) -> Printf.sprintf "(if %s %s %s)" (untag_nested test) (untag_nested dit) (untag_nested dif)
| Seq(exprs) -> Printf.sprintf "(begin %s)" (untag_list exprs)
| Or(exprs) ->  Printf.sprintf "(or %s)" (untag_list exprs)
| Set(expr1, expr2) -> Printf.sprintf "(set! %s %s)" (untag_nested expr1) (untag_nested expr2)
| Def(expr1, expr2) -> Printf.sprintf "(define %s %s)" (untag_nested expr1) (untag_nested expr2)
| LambdaSimple(args, expr) -> Printf.sprintf "(lambda (%s) %s)" (String.concat " " args) (untag_nested expr)
| LambdaOpt([], arg, expr) -> Printf.sprintf "(lambda %s %s)" arg (untag_nested expr)
| LambdaOpt(args, arg, expr) -> Printf.sprintf "(lambda (%s . %s) %s)" (String.concat " " args) arg (untag_nested expr)
| Applic(expr, args) -> Printf.sprintf "(%s %s)" (untag_nested expr) (untag_list args) 
and untag_nested expr = untag_rec expr true 
and untag_list exprs = String.concat " " (List.map untag_nested exprs) in
untag_rec expr false

let print_exprs exprs = 
let exprs = List.map untag exprs in
Printf.printf "%s\n" (String.concat "\n" exprs);;