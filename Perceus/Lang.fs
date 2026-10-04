module Lang

type Id = string

type Pattern = PCons of Id * Id list

and Expr = 
| Var of Id
| Lam of Id*Expr
| App of Expr*Expr
| Let of Id*Expr*Expr
| Match of Id * (Pattern*Expr) list
| Con of Id * Expr list
