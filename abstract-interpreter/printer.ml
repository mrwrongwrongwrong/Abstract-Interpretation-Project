(* File printer.ml  © P. Cousot 2018 *)

open AbstractSyntaxExpressions
open AbstractSyntax
open AbstractDomain
open AbstractTree

let rec string_of_aexpr a = match a with
| Num i -> string_of_int i
| Var v -> v
| Minus (a1, a2) -> "("^(string_of_aexpr a1)^" - "^(string_of_aexpr a2)^")"
| Plus (a1, a2)  -> "("^(string_of_aexpr a1)^" + "^(string_of_aexpr a2)^")"

let print_aexpr a = print_string (string_of_aexpr a)

let rec string_of_bexpr a = match a with
| Lt (a1, a2)   -> "("^(string_of_aexpr a1)^" < "^(string_of_aexpr a2)^")"
| Eq (a1, a2)   -> "("^(string_of_aexpr a1)^" == "^(string_of_aexpr a2)^")"
| Neq (a1, a2)  -> "("^(string_of_aexpr a1)^" != "^(string_of_aexpr a2)^")"
| Gt (a1, a2)   -> "("^(string_of_aexpr a1)^" > "^(string_of_aexpr a2)^")"
| Nand (b1, b2) -> "("^(string_of_bexpr b1)^" nand "^(string_of_bexpr b2)^")"

let rec print_bexpr b = print_string (string_of_bexpr b)

let rec print_tree s = match s with 
    | Prog (sl, l) -> print_treelist sl; print_newline ()
    | Assign (v, a, l) -> print_string (v ^ " = "); print_aexpr a; print_string "; "
    | Emptystmt l -> print_string "; "
    | If (b, st, l) -> print_string "(if "; print_bexpr b; print_string " "; print_tree st; print_string ") "
    | Ifelse (b, st, se, l) -> print_string "(if "; print_bexpr b; print_string " "; print_tree st; 
                            print_string " else "; print_tree se; print_string ") "
    | While (b, st, l) -> print_string "(while "; print_bexpr b; print_string " "; print_tree st; print_string ") "
    | Break l -> print_string "break; "
    | Stmtlist (sl, l) -> print_string "{ "; print_treelist sl; print_string "} "
and print_treelist sl = match sl with 
    | [] -> ()
    | [s] -> print_tree s
    | s :: sl' -> print_treelist sl'; print_tree s;; (* nodes in inverse order *)

let string_of_label l = "l" ^ (string_of_int l) ;;
let print_label l = print_string ((string_of_label l) ^ ": ");;
let bool_to_string  b = if b then "tt" else "ff";;
let labelmargin = 60;;
let rec print_space m = if (m>0) then (print_string " "; print_space (m-1));;
let rec print_margin m = print_space (labelmargin + m + 1)
let print_labelling (at,atP,af,afP,es,br,brP) m =
      let s = "<" ^ (string_of_label at) ^ ":" ^ (stringofaP atP) ^ "; " ^ 
                    (string_of_label af) ^ ":" ^ (stringofaP afP) ^ "; " ^ 
                    (bool_to_string es) ^ "; " ^
                    (string_of_label br) ^ ":" ^ (stringofaP brP) ^ ">"  in
       print_string s;
       if String.length s < labelmargin then print_space (labelmargin - (String.length s)+ 1);
       print_space m;;
let print_at (at,atP,af,afP,es,br,brP) = print_label at;;
let print_after (at,atP,af,afP,es,br,brP) = print_label af;;
let rec print_labelled_node s m = match s with 
    | Prog (sl, ls) -> print_labelling ls m; print_string "Prog:\n"; print_labelled_nodelist sl (m+3); 
                        print_margin m; print_after ls; print_newline ()
    | Assign (v, a, ls) -> print_labelling ls m; print_at ls; print_string (v ^ " = "); print_aexpr a; print_string "; "
    | Emptystmt ls -> print_labelling ls m;  print_at ls; print_string "; " 
    | If (b, st, ls) -> print_labelling ls m; print_string "(if "; print_at ls; print_bexpr b; print_newline ();
                         print_labelled_node st (m+3); print_string ") "
    | Ifelse (b, st, se, ls) ->  print_labelling ls m; print_string "(if "; print_at ls; print_bexpr b; print_newline (); 
                                  print_labelled_node st (m+3); print_newline ();
                                  print_margin m; print_string " else "; print_newline ();
                                  print_labelled_node se (m+3); print_string ") "
    | While (b, sb, ls) -> print_labelling ls m; print_string "(while "; print_at ls; print_bexpr b; print_newline (); 
                            print_labelled_node sb (m+3); print_string ") "
    | Break ls -> print_labelling ls m; print_at ls;  print_string "break; "
    | Stmtlist (sl, ls) -> print_labelling ls m; print_string "Stmtlist: {"; print_newline ();
                               (print_labelled_nodelist sl (m+3); 
                                print_margin m; print_string "} ")
and print_labelled_nodelist sl m = match sl with 
    | [] -> ()
    | [s] -> print_labelled_node s m; print_newline ()
    | s :: sl' -> print_labelled_nodelist sl' m; 
                  print_labelled_node s m; print_newline ();; (* nodes in inverse order *)
    
