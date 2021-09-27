(* file abstractDomainConstancy.ml  © P. Cousot 2018 *)

open AbstractSyntaxExpressions

type abstractConstant = BOT | INT of int | TOP

let cleq a1 a2 = match (a1,a2) with
   | (BOT,_) -> true
   | (_,BOT) -> false
   | (_,TOP) -> true
   | (TOP,_) -> false
   | (INT v1, INT v2) -> (v1=v2)

let cjoin a1 a2 = match (a1,a2) with
   | (BOT,a2) -> a2
   | (a1,BOT) -> a1
   | (_,TOP) -> TOP
   | (TOP,_) -> TOP
   | (INT v1, INT v2) -> if (v1=v2) then INT v1 else TOP

let stringofaC a =  match a with
  | BOT -> "_|_"
  | INT i -> string_of_int i
  | TOP -> " T "

(* environments are represented as a function of "x" and "y" only *)
type t = string -> abstractConstant
let initialize vl = ()
let leq r1 r2 = (cleq (r1 "x") (r2 "x")) &&
                (cleq (r1 "y") (r2 "y"))
let bot () = function x -> BOT
let join r1 r2 = function x -> cjoin (r1 x) (r2 x)
let initialP () = function x -> INT 0
let rec evala a r = match a with
   | Num i -> INT i
   | Var x -> if (x="x") || (x="y") then (r x)
              else failwith "AbstractDomainConstancy: undeclared variable"
   | Minus (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                  | BOT, _ -> BOT
                  | _, BOT -> BOT
                  | INT i, INT j -> INT (i-j)
                  | _, _ -> TOP)
   | Plus (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                  | BOT, _ -> BOT
                  | _, BOT -> BOT
                  | INT i, INT j -> INT (i+j)
                  | _, _ -> TOP)
let assign x a r = function y -> if (x=y) then (evala a r) else (r y)
let rec test b r = match b with 
   | Lt (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | INT i, INT j -> if i<j then r else bot ()
                   | _, _ -> r)
   | Eq (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | INT i, INT j -> if i=j then r else bot ()
                   | _, _ -> r)
   | Neq (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | INT i, INT j -> if i!=j then r else bot ()
                   | _, _ -> r)
   | Gt (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | INT i, INT j -> if i>j then r else bot ()
                   | _, _ -> r)
   | Nand (b1,b2) -> test b2 r (* coarse approximation *)
let nottest b r = match b with 
   | Lt (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | INT i, INT j -> if i>=j then r else bot ()
                   | _, _ -> r)
   | Eq (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | INT i, INT j -> if i!=j then r else bot ()
                   | _, _ -> r)
   | Neq (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | BOT, _ -> bot ()
                   | _, BOT -> bot ()
                   | INT i, INT j -> if i==j then r else bot ()
                   | _, _ -> r)
   | Gt (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                  | BOT, _ -> bot ()
                  | _, BOT -> bot ()
                  | INT i, INT j -> if i<=j then r else bot ()
                  | _, _ -> r)
   | Nand (b1,b2) -> r (* coarse approximation *)
let stringofaP r = "x=" ^ (stringofaC (r "x")) ^ ", y=" ^ (stringofaC (r "y"))
