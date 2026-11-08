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
    | ICon(_, ids) -> ids |> List.filter (fun x -> not (Set.contains x bound_vars)) |> Set.ofList
    | Dup(x, e) -> _free_vars e bound_vars |> if Set.contains x bound_vars then id else Set.add x
    | Drop(x, e) -> _free_vars e bound_vars |> if Set.contains x bound_vars then id else Set.add x
    | DropRu(x, r, e) -> _free_vars e bound_vars |> (if Set.contains x bound_vars then id else Set.add x) |> if Set.contains r bound_vars then id else Set.add r

let  free_vars (expr : IExpr) : IId Set = _free_vars expr Set.empty


/// When true, bindIfNeeded reuses an existing variable instead of adding a
/// redundant let. This is not the defined behaviour in "Fig. 3. Normalization" 
/// in the frame limited reuse paper but makes code much more readable and should not change the output.
let use_bind_if_needed = true

/// Bind a normalized expression to a fresh id only when it is not already a variable.
let bindIfNeeded (n : IExpr) : (IId * (IExpr -> IExpr)) statemonad =
    match n with
    | IVar i when use_bind_if_needed -> ret (i, id)
    | _ ->
        inc_counter () >>= fun i ->
        ret (i, fun body -> ILet(i, n, body, free_vars body))


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
            normalize e1 >>= fun n1 ->
            normalize e2 >>= fun n2 ->
            bindIfNeeded n1 >>= fun (f, wrapF) ->
            bindIfNeeded n2 >>= fun (x, wrapX) ->
            ret (wrapF (wrapX (IApp(IVar f, x))))
        )

    | Con (id, expr_lst) -> 
        sm_frame (
            List.map normalize expr_lst |> sequence >>= fun iexpr_lst ->
            List.map bindIfNeeded iexpr_lst |> sequence >>= fun binds ->
            ret (List.foldBack (fun (_, wrap) body -> wrap body) binds (ICon(id, List.map fst binds)))
        )

    | Match (e, patterns) -> 
        sm_frame (
            inc_counter () >>= 
            fun id -> normalize e >>= (
            fun n -> 
                List.map (
                    fun ((c, ids), e) -> sm_frame (
                        List.map get_fresh_id ids |> sequence >>= (
                        fun ids -> normalize e >>= (
                        fun n -> ret ((c, ids), n)
                    )))
                ) patterns |> sequence >>= fun patterns -> ret (ILet(id, n , IMatch(id, patterns), free_vars (IMatch(id, patterns))))
            )
        )
    
    | Let (id, e1, e2) ->  // TODO: Should let not remove the bound id from its free-vars?
        sm_frame (
            get_fresh_id id >>= fun id -> normalize e1 >>= fun n1 -> normalize e2 >>= fun n2 -> ret (ILet (id, n1, n2, free_vars n2))
        )
