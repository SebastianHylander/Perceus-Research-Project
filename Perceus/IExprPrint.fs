module IExprPrint

open Lang

/// Print an intermediate expression, using source names where available.
let iexpr_print (names : Map<int, string>) (e : IExpr) : string =
    let name i =
        defaultArg (Map.tryFind i names) ("var" + string i)

    let indent (s : string) =
        "  " + s.Replace("\n", "\n  ")

    let rec appParts (e : IExpr) =
        match e with
        | IApp (f, x) ->
            let head, args = appParts f
            head, args @ [x]
        | _ -> e, []

    let rec letBindings (e : IExpr) acc =
        match e with
        | ILet (x, e1, e2, _) -> letBindings e2 ((x, e1) :: acc)
        | _ -> List.rev acc, e

    let rec effectParts (e : IExpr) acc =
        match e with
        | Dup (x, body) ->
            effectParts body (sprintf "dup %s;" (name x) :: acc)
        | Drop (x, body) ->
            effectParts body (sprintf "drop %s;" (name x) :: acc)
        | DropRu (x, r, body) ->
            effectParts body (sprintf "dropru %s -> %s;" (name x) (name r) :: acc)
        | _ -> List.rev acc, e

    let rec go (e : IExpr) : string =
        match e with
        | IVar x -> name x
        | ICon (c, []) -> c
        | ICon (c, xs) -> sprintf "%s(%s)" c (xs |> List.map name |> String.concat ", ")
        | IApp _ ->
            let head, args = appParts e
            let headStr = go head
            let headStr =
                match head with
                | IVar _ | ICon _ -> headStr
                | _ -> sprintf "(%s)" headStr
            sprintf "%s %s" headStr (args |> List.map name |> String.concat " ")
        | ILam (x, body, _) ->
            sprintf "fun %s ->\n%s" (name x) (indent (go body))
        | ILet _ ->
            let bindings, body = letBindings e []
            let lines =
                bindings
                |> List.map (fun (x, e1) ->
                    let e1s = go e1
                    if e1s.Contains "\n" then
                        sprintf "let %s =\n%s\nin" (name x) (indent e1s)
                    else
                        sprintf "let %s = %s in" (name x) e1s)
            String.concat "\n" (lines @ [go body])
        | IMatch (x, cases) ->
            let arm ((c, xs), body) =
                let pat = String.concat " " (c :: List.map name xs)
                let bodyStr = go body
                if bodyStr.Contains "\n" then
                    sprintf "| %s ->\n%s" pat (indent bodyStr)
                else
                    sprintf "| %s -> %s" pat bodyStr
            sprintf "match %s with\n%s"
                (name x)
                (cases |> List.map arm |> String.concat "\n")
        | Dup _ | Drop _ | DropRu _ ->
            let effects, body = effectParts e []
            String.concat " " (effects @ [go body])

    go e
