module Reuse

open Lang

let hasReuseToken list = 
    list |> List.length |> Map.tryFind >> Option.defaultValue [] >>  ((=) []) >> not

let rec reuse (e : IExpr) (R : Map<int, IId list>) (S : Map<IC, int>) = 
    match e with    
    | IVar x -> IVar x, R // RVAR

    | ICon(t, list, _) when not (hasReuseToken list R) 
        -> ICon(t, list, None), R // RCON

    | ICon(t, list, _) when hasReuseToken list R
        -> let rus = Map.find (List.length list) R
           let R' = Map.add (List.length list) (List.tail rus) R
           ICon(t, list, Some (List.head rus)), R' // RCON-REUSE

    | IApp(e, x) 
        -> let e', R' = reuse e R S
           IApp(e', x), R' // RAPP

    | ILam(_, _, _) -> failwith "Not Implemented"
    
    | ILet(_, _, _, _) -> failwith "Not Implemented"
    | IMatch(_, _) -> failwith "Not Implemented"
    
    | Dup(_, _) -> failwith "Not Implemented"
    | Drop(_, _) -> failwith "Not Implemented"
    | DropRu(_, _, _) -> failwith "Not Implemented"