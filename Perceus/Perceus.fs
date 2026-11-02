module Perceus

open Lang

type Context = {borrowed : IId Set; owned : IId Set}

let is_owned x c = Set.contains x c.owned

let is_borrowed x c = Set.contains x c.borrowed

let owned_size c = Set.count c.owned

let is_empty_owned = owned_size >> (=) 0

let get_owned c = c.owned

let get_borrowed c = c.borrowed


let rec perceus (e : IExpr) (ctx : Context) : IExpr = 
    match e with    
    | IVar x when owned_size ctx = 1 && is_owned x ctx -> IVar x // svar
    | IVar x when is_empty_owned ctx && is_borrowed x ctx -> Dup (x, IVar x) // svar-dup
    | IVar _ -> failwith "forbidden: IVar"
    
    | ICon(t, list) -> // scon
        let ys = // multiset difference
            List.foldBack (fun id (l, s) -> 
            if Set.contains id s then 
                l, Set.remove id s
            else 
                id :: l, s
            ) list ([], get_owned ctx) |> fst // TODO: Is it okay to ignore unmatched owned things
        List.fold (fun acc e -> Dup(e, acc)) (ICon(t, list)) ys

    | ILam(x, e, free) when Set.contains x free -> // slam
        let ys = Set.remove x free
        let d1 = Set.difference ys (get_owned ctx)
        Set.fold (fun acc x' -> Dup (x', acc)) (ILam(x, perceus e {borrowed = Set.empty; owned = Set.add x ys}, ys)) d1

    | ILam(x, e, free)  -> // slam-d
        let ys = Set.remove x free
        let d1 = Set.difference ys (get_owned ctx)
        Set.fold (fun acc x' -> Dup (x', acc)) (ILam(x, Drop(x, perceus e {borrowed = Set.empty; owned = ys}), ys)) d1

    | IApp(e, x) when is_owned x ctx -> // sapp
        let e' = perceus e {borrowed = Set.add x (get_borrowed ctx); owned = Set.remove x (get_owned ctx)}
        IApp(e', x) 

    | IApp(e, x) when is_borrowed x ctx -> // sapp-dup
        let e' = perceus e ctx
        Dup(x, IApp(e', x)) 

    | IApp _ -> failwith "forbidden: IApp"

    | ILet(x, e1, e2, free) when Set.contains x free -> // slet (with fv(e2) = free)
        let gamma2 = Set.intersect (get_owned ctx) free
        let e1' = perceus e1 {borrowed = Set.union (get_borrowed ctx) gamma2; owned = Set.difference (get_owned ctx) gamma2}
        let e2' = perceus e2 {borrowed =  get_borrowed ctx; owned = Set.add x gamma2}
        ILet(x, e1', e2', free)

    | ILet(x, e1, e2, free)  -> // slet-d 
        let gamma2 = Set.intersect (get_owned ctx) free
        let e1' = perceus e1 {borrowed = Set.union (get_borrowed ctx) gamma2; owned = Set.difference (get_owned ctx) gamma2}
        let e2' = perceus e2 {borrowed =  get_borrowed ctx; owned = gamma2}
        ILet(x, e1', Drop(x, e2'), free)

    | IMatch(x, l) -> // smatch ( TODO: Should x not be removed from the owned context? )
        let l' = 
            List.foldBack (fun  (p, e) acc -> 
                let _, z = p
                let fv_ei = Normalization.free_vars e
                let combined = Set.union (get_owned ctx) (Set.ofList z)
                let gamma_i = Set.intersect fv_ei combined
                let gamma_i' = Set.difference combined gamma_i
                let e' = perceus e {borrowed =  get_borrowed ctx; owned = gamma_i}

                let e'' = Set.fold (fun acc x -> Drop(x, acc)) e' gamma_i'
                (p, List.fold (fun acc x -> Dup(x, acc)) e'' z) :: acc
            ) l []
        IMatch(x, l')

    | Dup(_, _) -> failwith "Not Implemented"
    | Drop(_, _) -> failwith "Not Implemented"
    | DropRu(_, _, _) -> failwith "Not Implemented"
