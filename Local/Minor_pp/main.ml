type exp =  Num of int | Bl of bool | V of string
          | Plus of exp * exp | Times of exp * exp
          | And of exp * exp | Or of exp * exp | Not of exp * exp
          | Gt of exp * exp | Eq of exp * exp
          | IFTE of exp * exp * exp
          | Pexp of exp * exp | Fst of exp | Snd of exp

type values = N of int | B of bool | Pval of values * values

type types = T_INT | T_BOOL | PT of types * types

exception Type_Error of exp * string


let rec type_of (e : exp) (sigma : string -> types) : types = match e with
| Num n -> T_INT
| Bl b -> T_BOOL
| Plus(e1,e2) ->
  (
    if(type_of e1 sigma = T_INT)
    then
      if(type_of e2 sigma = T_INT)
        then 
          T_INT
        else
          raise (Type_Error(e2, "Incorrect type of second expression in Plus"))
    else
      raise (Type_Error(e1, "Incorrect type of first expression in Plus"))
  )

| Pexp(e1,e2) -> 
  (
    let t1 = type_of e1 sigma
    and t2 = type_of e2 sigma 
    in PT(t1,t2)
  )
| _ -> T_INT

let rec eval (e : exp) (rho : string -> values) : values = match e with
| Num n -> N n
| Bl b -> B b
| V x -> rho x
| Plus (e1, e2) -> let N (n1) = eval e1 rho
                   and N (n2) = eval e2 rho
                   in N (n1 + n2) 
| Times (e1, e2) -> let N (n1) = eval e1 rho
                    and N (n2) = eval e2 rho
                    in N (n1 * n2)

| _ -> B (true)




