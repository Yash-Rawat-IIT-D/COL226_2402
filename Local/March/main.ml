
type exp = Num of int | Plus of exp * exp | Times of exp * exp
          | Bool of bool | Not of exp | And of exp * exp | Or of exp * exp
          | Eq of exp * exp | Gt of exp * exp
          | V of string 
          | IFTE of exp * exp * exp
          | Pair of exp * exp 
          | Fst of exp | Snd of exp
          | LET_IN_NI of def * 

type def = Adef of string * exp 
(*

ans is a subset of Exp -> A Canonical representation to which an expression
is equivalent to under Operational semantics (In our Case the Base Cases of 0 ht)

*)

type values = N of int | Bl of bool | PV of values * values

let rec str_value (v:values) = match v with
| N(n) ->  string_of_int n
| Bl(b) -> string_of_bool b
| PV(v1,v2) -> "( " ^ (str_value v1) ^" , "^ (str_value v2) ^ " )"

let print_value (v:values) = print_endline (str_value v)

type typ = INT_T | BOOL_T | PT of typ * typ

type opcode = LDN of int | LDB of bool | LOOKUP of string | PLUS | TIMES | NOT | AND | OR | EQ | GT | COND of opcode list * opcode list 
             | OP_PAIR | FST | SND 

exception Invalid_Type of (exp * string)
exception Stuck of (values list * opcode list)
exception Wrong
exception Not_In_Domain of string

let rec size (e : exp) : int = match e with
  | Num(n) -> 1
  | Bool(b) -> 1
  | V(s) -> 1
  | Plus(e1,e2) -> 1 + size(e1) + size(e2)
  | Times(e1,e2) -> 1 + size(e1) + size(e2)
  | Not(e1) -> 1 + size(e1)
  | And(e1,e2) -> 1 + size(e1) + size(e2)
  | Or(e1, e2) -> 1 + size(e1) + size(e2)
  | Eq(e1,e2) -> 1 + size(e1) + size(e2)
  | Gt(e1,e2) -> 1 + size(e1) + size(e2)
  | IFTE(e1,e2,e3) -> 1 + size(e1) + size(e2) + size(e3) 
  | Pair(e1,e2) -> 1 + size(e1) + size(e2)
  | Fst(e1) -> 1 + size(e1)
  | Snd(e1) -> 1 + size(e1)
  | Adef(s,e) -> 1 + 1 + size(e) 

let rec ht (e : exp) : int = match e with
  | Num(n) -> 0 | Bool(b) -> 0  | V(s) -> 0
  | Plus(e1,e2) -> 1 + max (ht e1) (ht e2) 
  | Times(e1,e2) -> 1 + max (ht e1) (ht e2)
  | Not(e1) -> 1 + ht e1
  | And(e1,e2) -> 1 + max (ht e1) (ht e2)
  | Or(e1,e2) -> 1 + max (ht e1) (ht e2)
  | Eq(e1,e2) -> 1 + max (ht e1) (ht e2) 
  | Gt(e1,e2) -> 1 + max (ht e1) (ht e2)
  | IFTE(e1,e2,e3) -> 1 + max (ht e1) (max (ht e2) (ht e3))
  | Pair(e1,e2) -> 1 + max (ht e1) (ht e2)
  | Fst(e) -> 1 + ht e
  | Snd(e) -> 1 + ht e
  | Adef(s,e) -> 1 + max (1) (ht e)

let rec type_of (tau : string -> typ) (e : exp) : typ = match e with 
| Num(n) -> INT_T
| Bool(b) -> BOOL_T  
| V(s) -> tau s
| Plus(e1,e2) -> 
  (
    if(type_of tau e1 = INT_T)
    then
      if(type_of tau e2 = INT_T)
      then
        INT_T
      else
        raise (Invalid_Type (e2,"Invalid Type : Expected of type INT"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type INT"))
  ) 
| Times(e1,e2) -> 
  (
    if(type_of tau e1 = INT_T)
    then
        if(type_of tau e2 = INT_T)
        then
          INT_T
        else
          raise (Invalid_Type (e2,"Invalid Type : Expected of type INT"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type INT"))
  )
| Not(e1) ->
  (
      if(type_of tau e1 = BOOL_T)
      then
        BOOL_T
      else
        raise (Invalid_Type(e1, "Invalid Type : Expected of type BOOL"))
  )
| And(e1,e2) -> 
  (
    if(type_of tau e1 = BOOL_T)
    then
        if(type_of tau e2 = BOOL_T)
        then
          BOOL_T
        else
          raise (Invalid_Type (e2,"Invalid Type : Expected of type BOOL"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type BOOL"))
  )
| Or(e1,e2) -> 
  (
    if(type_of tau e1 = BOOL_T)
    then
        if(type_of tau e2 = BOOL_T)
        then
          BOOL_T
        else
          raise (Invalid_Type (e2,"Invalid Type : Expected of type BOOL"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type BOOL"))
  )
| Eq(e1,e2) -> 
  (
    if(type_of tau e1 = INT_T)
    then
        if(type_of tau e2 = INT_T)
        then
          BOOL_T
        else
          raise (Invalid_Type (e2,"Invalid Type : Expected of type INT"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type INT"))
  )
| Gt(e1,e2) -> 
  (
    if(type_of tau e1 = INT_T)
    then 
        if(type_of tau e2 = INT_T)
        then
          BOOL_T
        else
          raise (Invalid_Type (e2,"Invalid Type : Expected of type INT"))
    else
        raise (Invalid_Type (e1,"Invalid Type : Expected of type INT"))
  )

| IFTE(e1,e2,e3) ->
  ( match type_of tau e1 with 
    | BOOL_T -> (
        let t1 = type_of tau e1 in let t2 = type_of tau e2 in
        if t1 = t2 then t1
        else
          raise (Invalid_Type(e1,"Invalide Type : Expected to be of same type as e2"))
      )
    | _ -> raise(Invalid_Type(e1,"Invalid Type : Expected of type BOOL")))
| Pair(e1,e2) ->
  ( let t1 = type_of tau e1 in
    let t2 = type_of tau e2 in
    PT(t1,t2))
| Fst(e) -> ( match type_of tau e with
              | PT(t1,_) -> t1 
              | _ -> raise(Invalid_Type(e,"Invalid Type : Expected of type t1 * t2 ")))
| Snd(e) -> ( match type_of tau e with
              | PT(_,t2) -> t2 
              | _ -> raise(Invalid_Type(e,"Invalid Type : Expected of type t1 * t2 ")))

(* Eval / Evaluator needs to take from a set of exp to a set of Values *)
(* We will fix the squiggles later by type checking *)
let rec eval_hlp (rho : string-> values) (e : exp) : values = 
  match e with
  | Num(n) -> N(n) | Bool(b) -> Bl(b) | V(s) -> rho s
  | Plus(e1,e2) ->  let N(n1) = (eval_hlp rho e1) and N(n2) = (eval_hlp rho e2) in N(n1+n2) 
  | Times(e1,e2) -> let N(n1) = (eval_hlp rho e1) and N(n2) = (eval_hlp rho e2) in N(n1+n2)
  | Not(e1) ->      let Bl(b) = (eval_hlp rho e1) in Bl(not b)
  | And(e1,e2) ->   let Bl(b1) = (eval_hlp rho e1) and Bl(b2) = (eval_hlp rho e2) in Bl(b1 && b2)
  | Or(e1,e2) ->    let Bl(b1) = (eval_hlp rho e1) and Bl(b2) = (eval_hlp rho e2) in Bl(b1 || b2)
  | Eq(e1,e2) ->    let N(n1) = (eval_hlp rho e1) and N(n2) = (eval_hlp rho e2) in Bl(n1 = n2) 
  | Gt(e1,e2) ->    let N(n1) = (eval_hlp rho e1) and N(n2) = (eval_hlp rho e2) in Bl(n1 > n2)
  | IFTE(e1,e2,e3) -> let Bl(b) =  (eval_hlp rho e1) in if(b) then (eval_hlp rho e2) else (eval_hlp rho e3)
  | Pair(e1,e2) -> let v1 = eval rho e1 in let v2 = eval rho v2 in PV(v1,v2)
  | Fst(e) -> let PV(v1,_) = eval rho e in v1
  | Snd(e) -> let PV(_,v2) = eval rho e in v2

let rec eval_top (rho : string-> values) (tau : string->typ) (e : exp) : values = 
  let _ = type_of tau e in eval_hlp rho e 

let rec compile (e : exp) : opcode list = match e with
  | Num(n) -> LDN(n)::[]
  | Bool(b) -> LDB(b)::[]
  | V(s) -> LOOKUP(s)::[]
  | Plus(e1,e2) -> compile e1 @ compile e2 @ [PLUS]
  | Times(e1,e2) -> compile e1 @ compile e2 @ [TIMES]
  | Not(e1) -> compile e1 @ [NOT]
  | And(e1,e2) -> compile e1 @ compile e2 @ [AND]
  | Or(e1,e2) -> compile e1 @ compile e2 @ [OR]
  | Eq(e1,e2) -> compile e1 @ compile e2 @ [EQ] 
  | Gt(e1,e2) -> compile e1 @ compile e2 @ [TIMES]
  | IFTE(e1,e2,e3) -> compile e1 @ [COND (compile e2,compile e3)]
  | Pair(e1,e2) -> compile e1 @ compile e2 @ [OP_PAIR]
  | Fst(e) -> compile e @ [FST]
  | Snd(e) -> compile e @ [SND]

let rec stackmc (g : string -> values) (s : values list) (c : opcode list) : values = match s,c with
  | x::[], [] -> x
  | xs , LDN(n)::xc -> stackmc g (N(n)::xs)  xc
  | xs , LDB(b)::xc -> stackmc g (Bl(b)::xs) xc
  | xs , LOOKUP(s)::xc -> stackmc g (g(s)::xs) xc
  | N(x1)::N(x2)::xs , PLUS::xc -> stackmc g (N(x1+x2)::xs) xc
  | N(x1)::N(x2)::xs , TIMES::xc -> stackmc g (N(x1*x2)::xs) xc
  | Bl(b)::xs , NOT::xc -> stackmc g (Bl(not b)::xs) xc
  | Bl(b1)::Bl(b2)::xs , AND::xc -> stackmc g (Bl(b1 && b2)::xs) xc
  | Bl(b1)::Bl(b2)::xs , OR::xc -> stackmc g (Bl(b1 || b2)::xs) xc
  | N(x1)::N(x2)::xs , EQ::xc -> stackmc g (Bl (x1 = x2) ::xs) xc
  | N(x1)::N(x2)::xs , GT::xc -> stackmc g (Bl(x1 > x2)::xs) xc 
  | Bl(true)::xs, COND(x1,x2)::xc -> stackmc g xs (x1 @ xc)
  | Bl(false)::xs, COND(x1,x2)::xc -> stackmc g xs (x2 @ xc)
  | v1::v2::xs, OP_PAIR::xc -> stackmc g (PV(v1,v2)::xs) xc
  | _ -> raise (Stuck(s,c))

let e : exp = IFTE(IFTE(Gt(Num(4),Num(5)),(Bool(true)),(Bool(false))),(Num(14)),(Num(41)))

let dummy_env _ = raise (Invalid_Type(V(""), "No variables in the expression"))
let compiled_code = compile e 
