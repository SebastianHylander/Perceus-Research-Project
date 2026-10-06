module Lang

type Id = string
type C = string

type Pattern = C * Id list

type Expr = // This matches page 11 of the paper:  Fig. 2. Syntax and semantics of 𝜆1
| Var of Id
| Lam of Id*Expr // lambda x.e  (x: Id, e: Expr)
| App of Expr*Expr
| Let of Id*Expr*Expr
| Match of Expr * (Pattern*Expr) list 
| Con of C * Expr list // Id is Head and tail is expr list fx Con("1", [])

type IId = int
type IC = int

type IPattern = IC * IId list

type IExpr = // This matches page 12 of the paper: The normalized linear resource calculus 𝜆1n
| IVar of IId
| ILam of IId*IExpr*(IId Set) // Now a closure with set of free variables
| IApp of IExpr*IId // Now the arg has to be a variable
| ILet of IId*IExpr*IExpr
| IMatch of IId * (IPattern*IExpr) list // Now has to match with a value
| ICon of IC * IId list // Now expressions flattened to values
| Dup of IId * IExpr
| Drop of IId * IExpr
| DropRu of IId * IId *IExpr // (x, r, e) from "r <-dropru x; e"