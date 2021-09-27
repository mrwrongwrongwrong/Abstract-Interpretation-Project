open AbstractSyntaxExpressions
open AbstractTree
open AbstractDomain
open AbstractSyntax

(*brP is break-to and br is break-of *)
let rec abstractInterpreter tree_node pre = match tree_node with
    | Emptystmt (at,atP,af,afP,es,br,brP) -> (Emptystmt (at,pre,af,pre,false,br,bot())) (*(Emptystmt (at,pre,af,atP,false,br,bot()))*)
    | Assign (v, a, (at,atP,af,afP,es,br,brP)) -> (
        let atP' = pre and afP' = assign v a pre in (*using atP' and afP' to substitude the original value in Assign *)
            Assign (v,a, (at, atP', af,afP', false, br, bot () ))
    )
    | Break (at, atP, af, afP, es, br, brP) -> Break (at, pre, af, afP, true, br, pre) (*Break (at, pre, af, bot(), es, at/ afP, pre/ afP) *) (*Break (at, pre, af, afP, true, at/ afP, pre/ afP) *)
    | Prog (sl, (at,atP,af,afP,es,br,brP)) -> ( (* let rec interpreting sl = 
        match sl with 
            | s::sl' -> abstractInterpreter s pre :: interpreting sl'
            | [] -> [] (*[(Emptystmt (at,atP,af,atP,false,br,bot()))] *) (* <l1: T ; l2: T ; ff; l0: T > *)
            in let sl' = interpreting sl and atP' = pre and afP' = pre (*brP' is the fold of all atP where es is true *) (* br is atP and brP is afP, when es is true*)
            in Prog (sl',(at,atP',af,afP',es,br,brP))   

        let rec select_after_list sl = 
            match sl with
            | [] -> pre
            | [sl'] ->  property_at sl'
            | s::sl' -> select_after_list sl'
        in *)
        let rec select_after_list sl = 
            match sl with
            | [] -> pre
            | [sl'] ->  property_at sl'
            | s::sl' -> select_after_list sl'
        in

        let rec interpret_prog (*abstractInterpreter*) (*select_after*) sl pre =  

            match sl with 
                | [] ->  failwith "error message: Empty!"
                | [s_1] -> [abstractInterpreter s_1 pre] (* using 递归  *) 
                (*| s:: sl' -> abstractInterpreter s (select_after (interpret_stmt sl' pre)) *)
                |s :: sl' -> (let sl'' = interpret_prog sl' pre in
                         (abstractInterpreter s (select_after_list sl'' )) :: sl'')

                         (*let sl2 = interpret_stmt sl' pre in *)
                        (*(let sl'' = interpret_stmt sl' pre in 
                        abstractInterpreter s (select_after_list sl'')) :: sl'' ) *)

        in

        let rec find_break sl =
            match sl with 
                | [s_1] -> property_break s_1
                | s::sl' -> join  (property_break s) (find_break sl')
                | [] -> bot()
        in
        let rec findatP sl pre= 
            match sl with 
                | [] -> pre
                | [s_1] -> property_at s_1 
                | s_1::sl' -> findatP sl' pre
        in
        let sl' = interpret_prog sl pre and atP' = findatP sl pre and brP' = find_break sl 
        in Prog(sl', (at, atP', af, afP, es, br, brP'))
    )
    (*
    | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> (let rec interpret_stmt sl = 
        match sl with 
            | s :: sl' -> abstractInterpreter s pre :: interpret_stmt sl' (* using 递归 *)
            | [] -> []
            in let sl' = interpret_stmt sl and es' = false and brP = brP
            in Stmtlist(sl', (at, atP, af, afP, es', br, brP))
    )  *)
    | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> ( (*let append l1 l2 =
            let rec loop acc l1 l2 =
            match l1, l2 with
                | [], [] -> List.rev acc
                | [], h :: t -> loop (h :: acc) [] t
                | h :: t, l -> loop (h :: acc) t l
        in loop [] l1 l2
        in *)
        let rec select_after_list sl = 
            match sl with
            | [] -> pre
            | [sl'] ->  property_at sl'
            | s::sl' -> select_after_list sl'
        in

        let rec interpret_stmt (*abstractInterpreter*) (*select_after*) sl pre =  

            match sl with 
                | [] -> failwith "error message: Empty!"
                | [s_1] -> [abstractInterpreter s_1 pre] (* using 递归  *) 
                (*| s:: sl' -> abstractInterpreter s (select_after (interpret_stmt sl' pre)) *)
                |s :: sl' -> (let sl'' = interpret_stmt sl' pre in
                         (abstractInterpreter s (select_after_list sl'' )) :: sl'')

                         (*let sl2 = interpret_stmt sl' pre in *)
                        (*(let sl'' = interpret_stmt sl' pre in 
                        abstractInterpreter s (select_after_list sl'')) :: sl'' ) *)

        in

        let rec find_break sl =
            match sl with 
                | [s_1] -> property_break s_1
                | s::sl' -> join  (property_break s) (find_break sl')
                | [] -> bot()
        in
        let rec findatP sl pre= 
            match sl with 
                | [] -> pre
                | [s_1] -> property_at s_1 
                | s_1::sl' -> findatP sl' pre
        in
        let sl' = interpret_stmt sl pre and atP' = findatP sl pre and brP' = find_break sl 
        in Stmtlist(sl', (at, atP', af, afP, es, br, brP'))
    )

    | If (b, st, (at, atP, af, afP, es, br, brP)) -> (
        let condition = test b pre in
        let true_st = abstractInterpreter st condition in
        let atP' = pre in
        let afP' = join (property_after true_st) (nottest b pre) in
        let get_brPst true_stmt = 
            match  true_stmt with
                | Emptystmt (at,atP,af,afP,es,br,brP) -> brP
                | Assign (v, a, (at,atP,af,afP,es,br,brP)) -> brP
                | Prog (sl, (at,atP,af,afP,es,br,brP)) -> brP
                |Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> brP
                | If (b, st, (at, atP, af, afP, es, br, brP))  -> brP
                | Break (at, atP, af, afP, es, br, brP) -> brP
                | Ifelse (b, st, se, (at,_,af,_,es,br,brP)) -> brP
                | While (b, sb, (at,atP,af,afP,es,br,brP)) -> brP

        in
        let brPst = get_brPst true_st in
        If (b, true_st, (at, atP',af, afP', es, br, brPst))
    )

    | Ifelse (b, st, se, (at,atP,af,afP,es,br,brP)) -> ( 
        let condition_st = test b pre in 
        let condition_se = nottest b pre in
        let true_stmt = abstractInterpreter st condition_st in
        let false_stmt = abstractInterpreter se condition_se in
        let atP' = pre in 
        let afP' = join (property_after true_stmt) (property_after false_stmt) in 

        let get_brPst true_stmt = 
            match  true_stmt with
                | Emptystmt (at,atP,af,afP,es,br,brP) -> brP
                | Assign (v, a, (at,atP,af,afP,es,br,brP)) -> brP
                | Prog (sl, (at,atP,af,afP,es,br,brP)) -> brP
                |Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> brP
                | If (b, st, (at, atP, af, afP, es, br, brP))  -> brP
                | Break (at, atP, af, afP, es, br, brP) -> brP
                | Ifelse (b, st, se, (at,_,af,_,es,br,brP)) -> brP
                | While (b, sb, (at,atP,af,afP,es,br,brP)) -> brP

        in

        let get_brPse false_stmt =  (*a local function *)
            match  false_stmt with
            | Emptystmt (at,atP,af,afP,es,br,brP) -> brP
            | Assign (v, a, (at,atP,af,afP,es,br,brP)) -> brP
            | Prog (sl, (at,atP,af,afP,es,br,brP)) -> brP
            | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> brP
            | If (b, st, (at, atP, af, afP, es, br, brP))  -> brP
            | Break (at, atP, af, afP, es, br, brP) -> brP
            | Ifelse (b, st, se, (at,_,af,_,es,br,brP)) -> brP
            | While (b, sb, (at,atP,af,afP,es,br,brP)) -> brP
        in 
             

        let brPst = get_brPst true_stmt in
        let brPse = get_brPse false_stmt in 
        let brP' = join brPst brPse in 
        (*let (if brPst = brPse then brP' = brPse else brP' = T ) in *)

        Ifelse (b, true_stmt, false_stmt, (at, atP', af, afP', es, br, brP'))
    )(*let brP' = bot() in*)

    (*| While (b, sb, (at,atP,af,afP,es,br,brP)) -> (let rec function_lpf while_loop pre = 
        match while_loop with
            | inv -> inv
            | While (b, sb, (at,atP,af,afP,es,br,brP)) -> 
                let condition =  test b pre in
                let true_stmt = abstractInterpreter sb condition in
                let atP' = join pre (property_after true_stmt) in
                let brP' = property_break true_stmt in
                let afP' = join (nottest b atP') brP' in 
                if leq atP' pre then While (b, true_stmt, (at, atP',af, afP', es,br, brP )) 
                else function_lpf (While (b, true_stmt, (at, atP',af, afP', es,br, brP ))) atP' 
        in 
        function_lpf (While(b, sb, (at, atP, af, afP, es, br, brP))) pre
    ) *)
    | While (b, sb, (at,atP,af,afP,es,br,brP)) -> (let rec function_lpf while_loop pre = 
                let condition =  test b pre in
                let true_stmt = abstractInterpreter sb condition in
                let atP' = join pre (property_after true_stmt) in
                let brP' = property_break true_stmt in
                let afP' = join (nottest b atP') brP' in 
                if leq atP' pre then While (b, true_stmt, (at, atP',af, afP', es,br, brP )) 
                else function_lpf (While (b, true_stmt, (at, atP',af, afP', es,br, brP ))) atP' 
        in 
        function_lpf (While(b, sb, (at, atP, af, afP, es, br, brP))) pre
    )

    (*in ifelse and if, the afP value in St is as same the afP value in afP in the entire statement S *)

    (* in stmtlist atP or brP is changed *)

(*
    | Prog (sl, (at,atP,af,afP,es,br,brP)) -> (
        let rec Interpreting sl=
        match sl with
        | s::sl' -> abstractInterpreter s pre; Interpreting sl';;
        | [] -> ;;
        in  Interpreting sl;;
    )
(*a function that takes any node, return atP, return afP, return brP, a node with test of B and atP, where the condition of test is taken in count modifying atP afP 
 and write a tree function to do that*)
    | If (b, st, (at,atP,af,afP,es,br,brP)) -> (
        let true_st = abstractInterpreter st (at,atP,af,afP,true,br,bot()) in
        let false_st = abstractInterpreter st (at,atP,af,afP,false,br, bot()) in
        let atP' = abstractInterpreter (test b pre) atP in
        let afP' = abstractInterpreter (at,atP(*test of B atP *),af,afP',es,br,bot())(*and this has to be st node *) (nottest b atP) in
        If (b, true_st, false_st, pre)
    )

    | If (b, st, (at,atP,af,afP,es,br,brP)) -> (match b with 
        | true -> let true_st = abstractInterpreter st (at,atP,af,afP,true,br,bot()) in
            let afP' = abstractInterpreter st (at,atP,af,afP,true,br,bot())
        
        |false -> let false_st = abstractInterpreter st (at,atP,af,afP,false,br,bot()) in
            let afP' = abstractInterpreter st (at,atP,af,afP,true,br,bot())
    )
        | Ifelse (b, st, se, (at,atP,af,afP,es,br,brP)) -> (let rec interpret_if b st se =
        match b with
            | true -> abstractInterpreter st pre 
            (* in ifelse, the label at st is equal to atS? *)
            | false -> abstractInterpreter se pre
        in let afP' = 
        in Ifelse (b, st, se, (at, atP, af, afP', es, br, brP))
    )
    | If (b, st, (at,atP,af,afP,es,br,brP)) -> (let rec interpret_if b st =
        match b with 
            | true -> abstractInterpreter st pre 
            | false -> af' = pre 
            in let 
            in If (b, st, (at, atP, af, afP, es, br, brP))
    )
*)