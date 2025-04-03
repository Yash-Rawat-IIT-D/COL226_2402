type exp = Num of int | Plus of exp * exp | Times of exp * exp
type opcode = LD of int | PLUS | TIMES 

exception Stuck 

let rec size (e : exp) : int = match e with
  | Num(n) -> 1
  | Plus(e1,e2) -> 1 + size(e1) + size(e2)
  | Times(e1,e2) -> 1 + size(e1) + size(e2)

let rec ht (e : exp) : int = match e with
  | Num(n) -> 0
  | Plus(e1,e2) -> 1 + max (ht e1) (ht e2)
  | Times(e1,e2) -> 1 + max (ht e1) (ht e2)

let rec eval (e : exp) : int = match e with
  | Num(n) -> n
  | Plus(e1,e2) -> eval(e1) + eval(e2)
  | Times(e1,e2) -> eval(e1) + eval(e2)

let rec compile (e : exp) : opcode list = match e with
  | Num(n) -> LD(n)::[]
  | Plus(e1,e2) -> compile e1 @ compile e2 @ [PLUS]
  | Times(e1,e2) -> compile e1 @ compile e2 @ [TIMES]


let rec stackmc (s : exp list) (c : opcode list) : exp = match s,c with
  | x::[], [] -> x
  | xs , LD(n):: xc -> stackmc (Num(n)::xs)  xc
  | Num(x1)::Num(x2)::xs , PLUS::xc -> stackmc (Num(x1+x2)::xs) xc
  | Num(x1)::Num(x2)::xs , TIMES::xc -> stackmc (Num(x1*x2)::xs) xc
  | _ -> raise Stuck   

