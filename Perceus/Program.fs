open Lang
open Normalization
open Monad
open State

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

let run (SM(f)) = 
    f (State.new_state ())

printfn "%A" (run (normalize e2))
printfn "%A" (run (normalize e2 >>= fun n2 -> free_vars n2 |> ret))