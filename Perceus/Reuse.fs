module Reuse

open Lang
open ReuseMonad
type Reuse_vars = Map<IId, int>

let get_var_of_size x sizes = Map.tryFindKey (fun _ v -> v = x) sizes

let add_drops r_list e = List.fold (fun acc ri -> Drop(ri, acc)) e r_list

let map_intersect (a: Map<'a, 'b>) (b: Map<'a, 'd>) = Map.filter (fun k _ -> b.ContainsKey k) a
let map_difference (a: Map<'a, 'b>) (b: Map<'a, 'd>) = Map.filter (fun k _ ->  not (b.ContainsKey k) ) a

let rec reuse (e : IExpr) (r: Reuse_vars): (IExpr * Reuse_vars) statemonad = 
    match e with    
    
    | Drop(x, e) -> // RDROP-REUSE && RDROP1 && RDROP2
        get_s () >>= fun s ->
        match s.TryFind x with
        | None ->  reuse e r >>= fun (e', r') -> //RDROP1
            ret ( Drop(x, e'), r')
        | Some n -> 
            inc_counter_monad () >>= fun r_key -> 
            reuse e (Map.add r_key n r) >>= fun (e', r') ->
            if Map.containsKey r_key r' then // RDROP2
                ret ( Drop(x, e'), r')
            else // RDROP-REUSE
                ret ( DropRu(x, r_key, e'), r')
        

    | ILet(x, e1, e2, free) -> // Choice point 2: reuse in e1 or e2.
        reuse e1 r >>= fun (e1', r') -> 
        reuse e2 r' >>= fun (e2', r'') -> 
        ret (ILet(x, e1',e2', free), r'')


    | IMatch(x, l) -> // RMATCH
        get_s () >>= fun s ->
        match Map.tryFind x s with
        | None -> 
            l |> List.map (fun ((c, ids), ei) -> 
                    reuse ei r >>= fun (ei', ri') ->
                        ret (((c, ids), ei'), ri')
                ) |> sequence >>= fun result ->
                    let patterns, r_list = List.unzip result
                    let r' = List.fold map_intersect r_list[0] r_list
                    ret (IMatch(x, patterns), r')

        | Some x -> 
            l |> List.map (fun ((c, ids), ei) -> 
                    set_s (Map.add x ids.Length s) >>>=
                    reuse ei r >>= fun (ei', ri') ->
                        ret (((c, ids), ei'), ri')
                ) |> sequence >>= fun result ->
                    let _, r_list = List.unzip result
                    let r' = List.fold map_intersect r_list[0] r_list
                    let with_drops = result |> List.map (fun (((c, ids), ei), ri) -> 
                        let ei' = ei |> add_drops (map_difference ri r' |> Map.keys |> List.ofSeq) 
                        (c, ids), ei'
                    )

                    ret (IMatch(x, with_drops), r')

          
    | IVar _ -> ret (e, r) // RVAR

    | ICon(c, list, ru) ->  
        if Option.isSome ru then failwith "Con already has reuse token" else
        match get_var_of_size list.Length r with
        | Some x ->   // RCON-Reuse
            ret (ICon(c, list, Some x), Map.remove x r)
        | None -> ret (e, r) // RCON

    | IApp(e, x) -> // RAPP
        reuse e r >>= fun (e', r') -> ret (IApp(e', x), r')

    | ILam(x, e, free)-> // RLAM (reaches into monad in non-nice way)
        get_s () >>= fun s ->
        set_s Map.empty >>>= 
        reuse e Map.empty >>= fun (e', r') ->
        set_s s >>>= (
            assert (r'.Count = 0)
            ret (ILam(x, e', free), r)
        )

    | Dup(x, e) ->  // RDUP
        reuse e r >>= fun (e', r') -> ret (Dup(x, e'), r')

    | DropRu(x, reuse_var, e) -> // RDROPRU
        reuse e r >>= fun (e', r') -> ret (DropRu(x, reuse_var, e'), r')

