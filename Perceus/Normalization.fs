module Normalization

open Lang
open Monad

// Finds all not bound vars in expr
let rec _free_vars (expr : IExpr) (bound_vars : IId Set) : IId Set = 
    match expr with
    | IVar s ->
        if Set.contains s bound_vars then Set.empty else Set.empty |> Set.add s    
    | ILam(x, _, free) -> Set.add x bound_vars |> Set.difference free 
    | IApp(e, x) -> _free_vars e bound_vars |> if Set.contains x bound_vars then id else Set.add x
    | ILet(id, e1, _, free) -> Set.union (_free_vars e1 bound_vars) (Set.add id bound_vars |> Set.difference free)
    | IMatch(x, patterns) ->
        List.map (
            fun ((_, ids) , e) -> 
                let ids = Set.ofList ids 
                _free_vars e (Set.union bound_vars ids)
        ) patterns
        |> Set.unionMany |> if Set.contains x bound_vars then id else Set.add x
    | ICon(_, ids) -> ids |> List.filter (fun x -> Set.contains x bound_vars) |> Set.ofList
    | Dup(x, e) -> _free_vars e bound_vars |> if Set.contains x bound_vars then id else Set.add x
    | Drop(x, e) -> _free_vars e bound_vars |> if Set.contains x bound_vars then id else Set.add x
    | DropRu(x, r, e) -> _free_vars e bound_vars |> (if Set.contains x bound_vars then id else Set.add x) |> if Set.contains r bound_vars then id else Set.add r

let  free_vars (expr : IExpr) : IId Set = _free_vars expr Set.empty


// This function does the normalization described on p 12: Fig. 3 & 4
let rec normalize (expr : Expr) : IExpr statemonad = 
    match expr with
    | Var s -> 
        get_id s >>= fun i -> sm_frame (ret (IVar i))

    | Lam (id, e) -> 
        sm_frame (
            get_fresh_id id >>= fun id -> normalize e >>= fun n -> ret (ILam (id, n, free_vars n))
        )

    | App (e1, e2) -> 
        sm_frame (
            inc_counter () >>= 
                fun id1 -> inc_counter () >>= (
                fun id2 -> normalize e1 >>= (
                fun n1 -> normalize e2 >>= (
                fun n2 -> 
                    ret (ILet(id1, n1, ILet(id2, n2, IApp(IVar(id1), id2), [id1; id2] |> Set.ofList), free_vars n2 |> Set.add id1)))
            ))
        )

    | Con (id, expr_lst) -> 
        sm_frame (
            List.init (List.length expr_lst) (fun _ -> inc_counter ()) |> sequence >>= 
                fun ids -> List.map (fun e -> normalize e) expr_lst |> sequence >>= (
                    fun iexpr_lst -> (List.zip ids iexpr_lst |>  List.foldBack (fun (i, e) n -> ILet(i, e, n, free_vars n))) (ICon(id,  ids)) |> ret
                )
        )

    | Match (e, patterns) -> 
        sm_frame (
            inc_counter () >>= 
            fun id -> normalize e >>= (
            fun n -> 
                List.map (
                    fun ((c, ids), e) -> sm_frame (
                        List.map get_id ids |> sequence >>= (
                        fun ids -> normalize e >>= (
                        fun n -> ret ((c, ids), n)
                    )))
                ) patterns |> sequence >>= fun patterns -> ret (ILet(id, n , IMatch(id, patterns), free_vars (IMatch(id, patterns))))
            )
        )
    
    | Let (id, e1, e2) -> 
        sm_frame (
            get_fresh_id id >>= fun id -> normalize e1 >>= fun n1 -> normalize e2 >>= fun n2 -> ret (ILet (id, n1, n2, free_vars n2))
        )
