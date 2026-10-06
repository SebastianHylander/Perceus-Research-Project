module Normalization

open Lang
open Monad

// Finds all not bound vars in expr
let rec _free_vars (expr : IExpr) (ids : IId Set) : IId Set = 
    match expr with
    | IVar id                -> ids |> Set.add id
    | ILam (_, _, free_vars)          -> free_vars
    | IApp (e1, id)          -> ids |> _free_vars e1 |> Set.add id
    | ILet (id, e1, e2)      -> _free_vars e2 Set.empty |> Set.remove id |> _free_vars e1 |> Set.union ids
    | IMatch (id, patterns)  -> ids |> Set.add id  |> 
                                List.foldBack (fun ((_, pids), e) ids -> ids |> _free_vars e |> List.foldBack Set.remove pids) patterns
    | ICon (_, args)    -> ids |> Set.union (Set.ofList args)    
    | _ -> ids

let  free_vars (expr : IExpr) : IId Set = _free_vars expr Set.empty


// This function does the normalization described on p 12: Fig. 3 & 4
let rec normalize (expr : Expr) : IExpr statemonad = 
    match expr with
    | Var s -> 
        get_id s >>= fun i -> sm_frame (ret (IVar i)) 

    | Lam (id, e) -> 
        sm_frame (
            get_id id >>= fun id -> normalize e >>= fun n -> ret (ILam (id, n, free_vars n))
        )

    | App (e1, e2) -> 
        sm_frame (
            inc_counter () >>= 
                fun id1 -> inc_counter () >>= (
                fun id2 -> normalize e1 >>= (
                fun n1 -> normalize e2 >>= (
                fun n2 -> ret (ILet(id1, n1, ILet(id2, n2, IApp(IVar(id1), id2)))))
            ))
        )

    | Con (id, expr_lst) -> 
        sm_frame (
            get_id id >>= fun id -> 
                List.init (List.length expr_lst) (fun _ -> inc_counter ()) |> sequence >>= 
                    fun ids -> List.map (fun e -> normalize e) expr_lst |> sequence >>= (
                        fun iexpr_lst -> (List.zip ids iexpr_lst |>  List.foldBack (fun (i, e) n -> ILet(i, e, n))) (ICon(id,  ids)) |> ret
                    )
        )   

    | Match (e, patterns) -> 
        sm_frame (
            inc_counter () >>= 
            fun id -> normalize e >>= (
            fun n -> 
                List.map (
                    fun ((c, ids), e) -> sm_frame (
                        get_id c >>= (
                        fun c -> List.map get_id ids |> sequence >>= (
                        fun ids -> normalize e >>= (
                        fun n -> ret ((c, ids), n)
                    ))))
                ) patterns |> sequence >>= fun patterns -> ret (ILet(id, n , IMatch(id, patterns)))
            )
        )
    
    | Let (id, e1, e2) -> 
        sm_frame (
            get_id id >>= fun id -> normalize e1 >>= fun n1 -> normalize e2 >>= fun n2 -> ret (ILet (id, n1, n2))
        )
