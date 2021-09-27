(* file abstractDomainTop.ml  © P. Cousot 2018 *)

open AbstractSyntaxExpressions

type t = TOP
let initialize vl = ()
let leq x y = true
let bot () = TOP
let join x y = TOP
let initialP () = TOP
let assign x a r0 = TOP
let test b r0 = r0
let nottest b r0 = r0
let stringofaP r0 = " T "
