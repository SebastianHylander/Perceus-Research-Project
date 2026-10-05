open Lang
open Normalization
open System
let unit x = Con(x, [])

let e1 = Lam ("x", Con("42", []))
let e2 = Let("x", Con("42", []), App(App(Var "take", Var "x"), Var "x"))

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
                        PCons("Cons", ["x"; "xx"]),
                         Con("Cons", [
                             App(Var "f", Var "x");
                             App(App(Var"map", Var "xx"), Var "f") 
                         ]);

                        PCons("Nil", []),
                         Con("Nil", [])
                    ]
                )
            )
        ),
        Var "map"
    )

print(normalize e2)
print(normalize e2 |> free_vars)