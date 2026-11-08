open FSharp.Text.Lexing
open IExprPrint
open Normalization
open Monad
open Perceus
open Examples


let run (SM(f)) =
    f (State.new_state ())

let runExample (example : Example) =
    let parsed = Parse.Main Lex.Token (LexBuffer<char>.FromString example.Source)
    let normalized, st = run (normalize parsed)
    let perceusExpr = perceus normalized {borrowed = Set.empty; owned = Set.empty}

    printfn "===== %s =====" example.Name
    printfn "Normalized:\n%s\n" (iexpr_print st.names normalized)
    printfn "Perceus:\n%s\n" (iexpr_print st.names perceusExpr)

Examples.all |> List.iter runExample
