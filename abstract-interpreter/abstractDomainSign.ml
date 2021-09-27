(* file abstractDomainSign.ml  Yue 2021 *)

open AbstractSyntaxExpressions

(*type t = BOT | Neg| TOP  | Pos | Neg0 | Pos0 | Is | Not0  *)

type abstractSign = BOT | NEG | IS0 | POS | NEG0 | NOT0 | POS0 | TOP 
type t = string -> abstractSign
type aexpr = One | Var of string | Minus of aexpr * aexpr | Plus of aexpr * aexpr | Join of aexpr * aexpr
            |Meet of aexpr * aexpr | Mul of aexpr * aexpr | Div of aexpr * aexpr;; 

let bot = 0 and neg = 1 and is0 = 2 and pos =3 and 
    neg0 = 4 and not0 = 5 and pos0 = 6 and top = 7;;

let print s = match s with 
   0 -> "bot" | 1 -> "neg" | 2 -> "is0" | 3 -> "pos" | 
   4 -> "neg0" | 5 -> "not0" | 6 -> "pos0" | 7 -> "top" |
   _ -> failwith "incorrect sign";;

let minus =[|[|bot; bot; bot; bot; bot; bot; bot; bot|];
             [|bot; top; neg; neg; top; top; neg; top|];
             [|bot; pos; is0; neg; pos0; not0; neg0; top|];
             [|bot; pos; pos; top; pos; top; top; top|];
             [|bot; pos; neg0; top; top; top; neg0; top|];
             [|bot; top; not0; top; top; top; top; top|];
             [|bot; pos; pos0; top; pos0; top; top; top|];
             [|bot; top; top; top; top; top; top; top|];
           |];;

let plus =[|[|bot; bot; bot; bot; bot; bot; bot; bot|];
            [|bot; neg; neg; top; neg; top; top; top|];
            [|bot; neg; is0; pos; neg0; not0; pos0; top|];
            [|bot; top; pos; pos; top; top; pos; top|];
            [|bot; neg; neg0; top; neg0; top; top; top|];
            [|bot; top; not0; top; top; not0; top; top|];
            [|bot; top; pos0; pos; top; top; pos0; top|];
            [|bot; top; top; top; top; top; top; top|];
          |];;

let join = [|[|bot; neg; is0; pos; neg0; not0; pos0; top|];
             [|neg; neg; neg0; not0; neg0; not0; top; top|];
             [|is0; neg0; is0; pos0; neg0; top; pos0; top|];
             [|pos; not0; pos0; pos; top; not0; pos0; top|];
             [|neg0; neg0; neg0; top; neg0; top; top; top|];
             [|not0; not0; top; pos; top; not0; top; top|];
             [|pos0; top; pos0; pos0; top; top; pos0; top|];
             [|top; top; top; top; top; top; top; top|];
           |];;

let meet = [|[|bot; bot; bot; bot; bot; bot; bot; bot|];
             [|bot; neg; bot; bot; neg; neg; bot; neg|];
             [|bot; bot; is0; bot; is0; bot; is0; top|];
             [|bot; bot; bot; pos; bot; pos; pos; pos|];
             [|bot; neg; is0; bot; neg0; neg; is0; neg0|];
             [|bot; neg; bot; pos; neg; not0; pos; not0|];
             [|bot; bot; is0; pos; is0; pos; pos0; pos0|];
             [|bot; neg; is0; pos; neg0; not0; pos0; top|];
           |];;

let mul = [|[|bot; bot; bot; bot; bot; bot; bot; bot|];
            [|bot; pos; is0; neg; pos0; not0; neg0; top|];
            [|bot; is0; is0; is0; is0; is0; is0; is0|];
            [|bot; neg; is0; pos; neg0; not0; pos0; top|];
            [|bot; pos0; is0; neg0; pos0; top; neg0; top|];
            [|bot; not0; is0; not0; top; not0; top; top|];
            [|bot; neg0; is0; pos0; neg0; top; pos0; top|];
            [|bot; top; is0; top; top; top; top; top|];
          |];;

let div = [|[|bot; bot; bot; bot; bot; bot; bot; bot|];
            [|bot; pos; bot; neg; pos; not0; neg; not0|];
            [|bot; is0; bot; is0; is0; is0; is0; is0|];
            [|bot; neg; bot; pos; neg; not0; pos; not0|];
            [|bot; pos0; bot; neg0; pos0; top; neg0; top|];
            [|bot; not0; bot; not0; not0; not0; not0; not0|];
            [|bot; neg0; bot; pos0; neg0; top; pos0; top|];
            [|bot; top; bot; top; top; top; top; top|];
         |];;

type environment = (string * int) list ;;

let rec sign a r = match a with 
   | One -> pos
   | Var x -> List.assoc x r
   | Minus (a1, a2) -> minus.(sign a1 r).(sign a2 r)
   | Plus (a1, a2) -> plus.(sign a1 r).(sign a2 r)
   | Mul (a1, a2) -> mul.(sign a1 r).(sign a2 r)
   | Div (a1, a2) -> div.(sign a1 r).(sign a2 r)
   | Join (a1, a2) -> join.(sign a1 r).(sign a2 r)
   | Meet (a1, a2) -> meet.(sign a1 r).(sign a2 r);;
   
let initialize vl = ()
let leq x y = match x, y with
   | BOT, _-> true
   | _, BOT -> false
   | _, TOP-> true
   | TOP,_ -> false
   | NEG, _ -> true  
   | _, NEG -> false 
   | NEG0, _ -> true
   | _, NEG0 -> false 
   | POS, _ -> true
   | _, POS -> false 
   | POS0, _ -> false
   | _, POS0 -> true 
   | NOT0, _ -> false 
   | _, NOT0 -> false 
   | IS0, _ -> false

let bot () = BOT
(*let join x y = if (x=BOT && y=BOT) then BOT else TOP *)
(*let initialP () = IS0 *)
let initialP () = function x -> IS0

(*let assign x a r = function y -> if (x=y) then (evala a r) else (r y) *)
let rec evala a r = match a with
   | One -> pos 
   | Var x -> List.assoc x r
   | Minus (a1,a2) -> minus.(sign a1 r). (sign a2 r)
   | Plus (a1,a2) -> plus.(sign a1 r). (sign a2 r)
   | Join (a1,a2) -> join.(sign a1 r). (sign a2 r)
   | Meet (a1,a2) -> meet.(sign a1 r). (sign a2 r)
   | Mul (a1,a2) -> mul.(sign a1 r). (sign a2 r)
   | Div (a1,a2) -> div.(sign a1 r). (sign a2 r)
   ;;
 let assign x a r = function y -> if (x=y) then (evala a r) else (r y)              
let rec test b r = match b with 
   | Lt (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | _, TOP-> r
                   | TOP,_ -> bot ()
                   | _, _ -> r)
   | Eq (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | _, TOP-> bot()
                   | TOP,_ -> bot ()
                   | _, _ -> r)
   | Neq (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | _, _ -> r)
   | Gt (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | _, TOP-> bot()
                   | TOP,_ -> r
                   | _, _ -> r)
   | Nand (b1,b2) -> test b2 r (* coarse approximation *)
let nottest b r = match b with 
   | Lt (a1,a2) -> (match (evala a1 r), (evala a2 r) with 
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | _, TOP-> bot()
                   | TOP,_ -> r
                   | _, _ -> r)
   | Eq (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | _, TOP-> r
                   | TOP,_ -> r
                   | _, _ -> r)
   | Neq (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | _, TOP-> bot()
                   | TOP,_ -> bot ()
                   | _, _ -> r)
   | Gt (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                  | BOT, _ -> bot ()
                  | _, BOT -> bot ()
                  | _, TOP-> r
                  | TOP,_ -> bot ()
                  | _, _ -> r)
   | Nand (b1,b2) -> r (* coarse approximation *)
let stringofaP r = "x=" ^ (stringofaC (r "x")) ^ ", y=" ^ (stringofaC (r "y"))
