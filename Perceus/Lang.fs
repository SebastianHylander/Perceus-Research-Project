module Lang

type Id = string
type C = string

type Pattern = PCons of C * Id list

type Expr = // This matches page 11 of the paper:  Fig. 2. Syntax and semantics of 𝜆1
| Var of Id
| Lam of Id*Expr // lambda x.e  (x: Id, e: Expr)
| App of Expr*Expr
| Let of Id*Expr*Expr
| Match of Expr * (Pattern*Expr) list 
| Con of C * Expr list // Id is Head and tail is expr list fx Con("1", [])

type IExpr = // This matches page 12 of the paper: The normalized linear resource calculus 𝜆1n
| IVar of Id
| ILam of Id*IExpr*(Id Set) // Now a closure with set of free variables
| IApp of IExpr*Id // Now the arg has to be a variable
| ILet of Id*IExpr*IExpr
| IMatch of Id * (Pattern*IExpr) list // Now has to match with a value
| ICon of C * Id list // Now expressions flattened to values
| Dup of Id * IExpr
| Drop of Id * IExpr
| DropRu of Id * Id *IExpr // (x, r, e) from "r <-dropru x; e"