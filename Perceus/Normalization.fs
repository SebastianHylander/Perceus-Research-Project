module Normalization

open Lang
open System


let rec _used_vars (expr : Expr) (ids : Id Set) : Id Set = 
    match expr with
    | Var id                -> ids |> Set.add id
    | Lam (id, e1)          -> ids |> Set.add id |> _used_vars e1
    | App (e1, e2)          -> ids |> _used_vars e1 |> _used_vars e2
    | Let (id, e1, e2)      -> ids |> Set.add id |> _used_vars e1 |> _used_vars e2
    | Match (e, patterns)  -> ids |> _used_vars e  |> List.foldBack (fun (PCons (id, pids), e) ids -> ids |> Set.add id |> List.foldBack Set.add pids |> _used_vars e) patterns
    | Con (id, expr_lst)    -> ids |> Set.add id |> List.foldBack _used_vars expr_lst

let  used_vars (expr : Expr) : Id Set = _used_vars expr Set.empty

let rec fresh_var used_vars = 
    let id = "_"+ Guid.NewGuid().ToString("N")
    if not (Set.contains id used_vars) then id else fresh_var used_vars

let rec fresh_var_n used_vars n = 
    let rec loop used_vars n l =
        if n = 0 then l, used_vars else 
        let id = fresh_var used_vars
        let used_vars = Set.add id used_vars
        loop used_vars (n-1) (id::l)
    loop used_vars n []

// Finds all not bound vars in expr
let rec _free_vars (expr : IExpr) (ids : Id Set) : Id Set = 
    match expr with
    | IVar id                -> ids |> Set.add id
    | ILam (_, _, free_vars)          -> free_vars
    | IApp (e1, id)          -> ids |> _free_vars e1 |> Set.add id
    | ILet (id, e1, e2)      -> _free_vars e2 Set.empty |> Set.remove id |> _free_vars e1 |> Set.union ids
    | IMatch (id, patterns)  -> ids |> Set.add id  |> 
                                List.foldBack (fun (PCons (_, pids), e) ids -> ids |> _free_vars e |> List.foldBack Set.remove pids) patterns
    | ICon (_, args)    -> ids |> Set.union (Set.ofList args)    
    | _ -> ids


let  free_vars (expr : IExpr) : Id Set = _free_vars expr Set.empty


// This function does the normalization described on p 12: Fig. 3 & 4
let rec _normalize (expr : Expr) (used_ids : Id Set) = 
    match expr with
    | Var x                 -> IVar x
    | Lam (id, e1)  -> 
        let n =  _normalize e1 used_ids 
        ILam (id, n, free_vars n )

    | App (e1, e2)          ->         
        let id1 = fresh_var used_ids
        let used_ids = Set.add id1 used_ids
        let id2 = fresh_var used_ids
        let used_ids = Set.add id2 used_ids
        ILet(id1, _normalize e1 used_ids, ILet(id2, _normalize e2 used_ids, IApp(IVar id1, id2) ))

    | Con (id, expr_lst)    -> 
        let ids, used_ids = fresh_var_n used_ids expr_lst.Length
        let init = ICon (id,  ids)
        List.foldBack (fun (e: Expr, id: Id) (cur : IExpr)  -> 
                ILet (id, _normalize e used_ids, cur)  // TODO: this normalizes in the same used_ids, We should find a nicer way to get fresh vars
                ) (List.zip expr_lst ids) init

    | Match (e, patterns)  -> 
        let id = fresh_var used_ids
        let used_ids = Set.add id used_ids
        ILet(id, _normalize e used_ids, IMatch (id, List.map ( fun (p, e) -> p, _normalize e used_ids) patterns ))

    | Let (id, e1, e2)      -> ILet (id, _normalize e1 used_ids, _normalize e2 used_ids)


let normalize (expr : Expr) = _normalize expr (used_vars expr)




