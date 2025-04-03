type exp = Num of int | Plus of exp * exp | Times of exp * exp
          | Bool of bool | Not of exp | And of exp * exp | Or of exp * exp
          | Eq of exp * exp | Gt of exp * exp 

(*

ans is a subset of Exp -> A Canonical representation to which an expression
is equivalent to under Operational semantics (In our Case the Base Cases of 0 ht)

*)

type values = N of int | Bl of bool

type typ = INT_T | BOOL_T

type opcode = LDN of int | LDB of bool | PLUS | TIMES | NOT | AND | OR | EQ | GT

exception Invalid_Type of (exp * string)
exception Stuck of (exp list * opcode list)

let rec size (e : exp) : int = match e with
  | Num(n) -> 1
  | Bool(b) -> 1
  | Plus(e1,e2) -> 1 + size(e1) + size(e2)
  | Times(e1,e2) -> 1 + size(e1) + size(e2)
  | Not(e1) -> 1 + size(e1)
  | And(e1,e2) -> 1 + size(e1) + size(e2)
  | Or(e1, e2) -> 1 + size(e1) + size(e2)
  | Eq(e1,e2) -> 1 + size(e1) + size(e2)
  | Gt(e1,e2) -> 1 + size(e1) + size(e2)

let rec ht (e : exp) : int = match e with
  | Num(n) -> 0 | Bool(b) -> 0  
  | Plus(e1,e2) -> 1 + max (ht e1) (ht e2) 
  | Times(e1,e2) -> 1 + max (ht e1) (ht e2)
  | Not(e1) -> 1 + ht e1
  | And(e1,e2) -> 1 + max (ht e1) (ht e2)
  | Or(e1,e2) -> 1 + max (ht e1) (ht e2)
  | Eq(e1,e2) -> 1 + max (ht e1) (ht e2) 
  | Gt(e1,e2) -> 1 + max (ht e1) (ht e2)

let rec type_of (e : exp) : typ = match e with 
| Num(n) -> INT_T
| Bool(b) -> BOOL_T  
| Plus(e1,e2) -> 
  (
    if(type_of e1 = INT_T)
    then
      if(type_of e2 = INT_T)
      then
        INT_T
      else
        raise (Invalid_Type (e2,"Invalid Type : Expected of type INT"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type INT"))
  ) 
| Times(e1,e2) -> 
  (
    if(type_of e1 = INT_T)
    then
        if(type_of e2 = INT_T)
        then
          INT_T
        else
          raise (Invalid_Type (e2,"Invalid Type : Expected of type INT"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type INT"))
  )
| Not(e1) ->
  (
      if(type_of e1 = BOOL_T)
      then
        BOOL_T
      else
        raise (Invalid_Type(e1, "Invalid Type : Expected of type BOOL"))
  )
| And(e1,e2) -> 
  (
    if(type_of e1 = BOOL_T)
    then
        if(type_of e2 = BOOL_T)
        then
          BOOL_T
        else
          raise (Invalid_Type (e2,"Invalid Type : Expected of type BOOL"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type BOOL"))
  )
| Or(e1,e2) -> 
  (
    if(type_of e1 = BOOL_T)
    then
        if(type_of e2 = BOOL_T)
        then
          BOOL_T
        else
          raise (Invalid_Type (e2,"Invalid Type : Expected of type BOOL"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type BOOL"))
  )
| Eq(e1,e2) -> 
  (
    if(type_of e1 = INT_T)
    then
        if(type_of e2 = INT_T)
        then
          BOOL_T
        else
          raise (Invalid_Type (e2,"Invalid Type : Expected of type INT"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type INT"))
  )
| Gt(e1,e2) -> 
  (
    if(type_of e1 = INT_T)
    then
        if(type_of e2 = INT_T)
        then
          BOOL_T
        else
          raise (Invalid_Type (e2,"Invalid Type : Expected of type INT"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type INT"))
  )


(* Eval / Evaluator needs to take from a set of exp to a set of Values *)
(* We will fix the squiggles later by type checking *)
let rec eval (e : exp) : values = 
  (
  let _ = type_of e in 
  match e with
  | Num(n) -> N(n)
  | Bool(b) -> Bl(b)  
  | Plus(e1,e2) -> let N(n1) = eval(e1) and N(n2) = eval(e2) in N(n1+n2) 
  | Times(e1,e2) -> let N(n1) = eval(e1) and N(n2) = eval(e2) in N(n1+n2)
  | Not(e1) -> let Bl(b) = eval(e1) in Bl(not b)
  | And(e1,e2) -> let Bl(b1) = eval(e1) and Bl(b2) = eval(e2) in Bl(b1 && b2)
  | Or(e1,e2) -> let Bl(b1) = eval(e1) and Bl(b2) = eval(e2) in Bl(b1 || b2)
  | Eq(e1,e2) -> let N(n1) = eval(e1) and N(n2) = eval(e2) in Bl(n1 = n2) 
  | Gt(e1,e2) -> let N(n1) = eval(e1) and N(n2) = eval(e2) in Bl(n1 > n2)
  )
let rec compile (e : exp) : opcode list = match e with
  | Num(n) -> LDN(n)::[]
  | Bool(b) -> LDB(b)::[]
  | Plus(e1,e2) -> compile e1 @ compile e2 @ [PLUS]
  | Times(e1,e2) -> compile e1 @ compile e2 @ [TIMES]
  | Not(e1) -> compile e1 @ [NOT]
  | And(e1,e2) -> compile e1 @ compile e2 @ [AND]
  | Or(e1,e2) -> compile e1 @ compile e2 @ [OR]
  | Eq(e1,e2) -> compile e1 @ compile e2 @ [EQ] 
  | Gt(e1,e2) -> compile e1 @ compile e2 @ [TIMES]

let rec stackmc (s : exp list) (c : opcode list) : exp = match s,c with
  | x::[], [] -> x
  | xs , LDN(n)::xc -> stackmc (Num(n)::xs)  xc
  | xs , LDB(b)::xc -> stackmc (Bool(b)::xs) xc
  | Num(x1)::Num(x2)::xs , PLUS::xc -> stackmc (Num(x1+x2)::xs) xc
  | Num(x1)::Num(x2)::xs , TIMES::xc -> stackmc (Num(x1*x2)::xs) xc
  | Bool(b)::xs , NOT::xc -> stackmc (Bool(not b)::xs) xc
  | Bool(b1)::Bool(b2)::xs , AND::xc -> stackmc (Bool(b1 && b2)::xs) xc
  | Bool(b1)::Bool(b2)::xs , OR::xc -> stackmc (Bool(b1 || b2)::xs) xc
  | Num(x1)::Num(x2)::xs , EQ::xc -> stackmc (Bool(x1 = x2)::xs) xc
  | Num(x1)::Num(x2)::xs , GT::xc -> stackmc (Bool(x1 > x2)::xs) xc 
  | _ -> raise (Stuck(s,c))


  


