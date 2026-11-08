open Lang
open Normalization
open Monad
open Perceus

let unit x = Con(x, [])

let e1 = Lam ("x", Con("42", []))

let n1 = ILam(0, ICon("42", []), Set.empty) 

let e2 = Let("x", Con("42", []), App(App(Var "take", Var "x"), Var "x"))

let n2 = ILet (0, ICon("42", []), IApp(IApp(IVar 2, 0), 0), Set.empty) // free vars not correct

let e3 = Con("Cons", [
  unit "1"; 
  Con("Cons", [
    unit "2"; 
    Con("Cons", [
      unit "3"; 
      unit "nil"
    ])
  ])
])

let map : Expr =
    Let("map",
        Lam("xs",
            Lam("f",
                Match(
                    Var "xs",
                    [
                        ("Cons", ["x"; "xx"]),
                         Con("Cons", [
                             App(Var "f", Var "x");
                             App(App(Var"map", Var "xx"), Var "f") 
                         ]);

                        ("Nil", []),
                         Con("Nil", [])
                    ]
                )
            )
        ),
        Var "map"
    )

let rec pp (e : IExpr) : string =
    match e with
    | IVar x -> "var" + string x
    | ICon (c, []) -> c
    | ICon (c, xs) -> sprintf "%s(%s)" c (xs |> List.map string |> String.concat ", ")
    | IApp (IVar f, x) -> sprintf "%d %d" f x
    | IApp (f, x) -> sprintf "(%s) %d" (pp f) x
    | ILam (x, body, _) -> sprintf "fun var%d -> \n %s" x (pp body)
    | ILet (x, e1, e2, _) -> sprintf "let var%d = %s in %s" x (pp e1) (pp e2)
    | IMatch (x, cases) ->
        let arm ((c, xs), body) =
            let pat = String.concat " " (c :: List.map string xs)
            sprintf "%s -> %s" pat (pp body)
        sprintf "match %d with \n %s" x (cases |> List.map arm |> String.concat " | ")
    | Dup (x, e) -> sprintf "dup var%d in %s" x (pp e)
    | Drop (x, e) -> sprintf "drop var%d in %s" x (pp e)
    | DropRu (x, r, e) -> sprintf "dropru var%d -> %d in %s" x r (pp e)


let run (SM(f)) = 
    f (State.new_state ())



let source = "let map = fun xs -> fun f ->
    match(xs) {
        Cons(x,xx) {
            match(f x) {
                Error(err) -> { Error(err); }
                Ok(y) -> { match(map xx f) {
                    Error(err) -> Error(err)
                    Ok(ys) -> Cons(y,ys)
                }}
            }
        }
    }
in map end
                
"
let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString source
let parsed = Parse.Main Lex.Token lexbuf
printfn "Parsed: \n%A\n" parsed

let map_normalized, st = run (normalize parsed)  


printfn "Normalized \n%A\n" st
printfn "Normalized \n%A\n" (pp map_normalized)

let map_perceus = perceus map_normalized {borrowed = Set.empty; owned = Set.empty } 
printfn "Perceus \n%A\n" (pp map_perceus)
