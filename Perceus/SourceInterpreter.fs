module SourceInterpreter

open Lang

type Env = Map<Id, Value>

and Value =
    | VClosure of Id * Expr * Env
    | VCon of C * Value list

let rec eval (env : Env) (expr : Expr) : Value =
    match expr with
    | Var x ->
        match Map.tryFind x env with
        | Some value -> value
        | None -> failwith $"Unbound variable: {x}"

    | Lam (x, body) ->
        VClosure (x, body, env)

    | App (e1, e2) ->
        let f = eval env e1
        let arg = eval env e2

        match f with
        | VClosure (x, body, closureEnv) ->
            let env' = Map.add x arg closureEnv
            eval env' body

        | _ ->
            failwith "Attempted to apply a non-function"

    | Let (x, e1, e2) ->
        let value = eval env e1
        let env' = Map.add x value env
        eval env' e2

    | Con (c, exprs) ->
        let values =
            exprs
            |> List.map (eval env)

        VCon (c, values)

    | Match (e, branches) ->
        let value = eval env e

        match value with
        | VClosure _ ->
            failwith "Cannot pattern match on a function"

        | VCon (constructorName, fields) ->
            evalMatch env constructorName fields branches

and evalMatch (env : Env) (constructorName : C) (fields : Value list) (branches : (Pattern * Expr) list) : Value =
    match branches with
    | [] ->
        failwith $"No matching branch for constructor {constructorName}"

    | (PCons (c, ids), body) :: rest ->
        if c = constructorName then

            if List.length ids <> List.length fields then
                failwith "Pattern arity mismatch"

            let env' =
                List.zip ids fields
                |> List.fold
                    (fun env (id, value) ->
                        Map.add id value env)
                    env

            eval env' body

        else
            evalMatch env constructorName fields rest

let run expr =
    eval Map.empty expr