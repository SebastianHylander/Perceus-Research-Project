open Lang
open System

let rec _used_vars (expr : Expr) (ids : Id Set) : Id Set = 
    match expr with
    | Var id                -> ids |> Set.add id
    | Lam (id, e1)          -> ids |> Set.add id |> _used_vars e1
    | App (e1, e2)          -> ids |> _used_vars e1 |> _used_vars e2
    | Let (id, e1, e2)      -> ids |> Set.add id |> _used_vars e1 |> _used_vars e2
    | Match (id, patterns)  -> ids |> Set.add id  |> List.foldBack (fun (PCons (id, pids), e) ids -> ids |> Set.add id |> List.foldBack Set.add pids |> _used_vars e) patterns
    | Con (id, expr_lst)    -> ids |> Set.add id |> List.foldBack _used_vars expr_lst

let  used_vars (expr : Expr) : Id Set = _used_vars expr Set.empty

let rec fresh_var used_vars = 
    let id = "_"+ Guid.NewGuid().ToString("N")
    if not (Set.contains id used_vars) then id else fresh_var used_vars

let rec _normalize (expr : Expr) (ids : Id Set) = 
    match expr with
    | Var _                 -> expr
    | Lam (id, e1)          -> Lam(id, _normalize e1 ids)
    | App (e1, e2)          -> 
        let id1 = fresh_var ids
        let ids = Set.add id1 ids
        let id2 = fresh_var ids
        let ids = Set.add id2 ids
        Let(id1, _normalize )
    | Let (id, e1, e2)      -> failwith "not implemented"
    | Match (id, patterns)  -> failwith "not implemented"
    | Con (id, expr_lst)    -> failwith "not implemented"

let normalize (expr : Expr) = _normalize expr (used_vars expr)
