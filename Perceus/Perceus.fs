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
    | IVar x when owned_size ctx = 1 && is_owned x ctx -> IVar x
    | IVar x when is_empty_owned ctx && is_borrowed x ctx -> Dup (x, IVar x)
    | IVar _ -> failwith "forbidden"

    | ILam(x, e, free) when Set.contains x free -> 
        let ys = Set.remove x free
        let d1 = Set.difference ys (get_owned ctx)
        Set.fold (fun acc x' -> Dup (x', acc)) (ILam(x, perceus e {borrowed = Set.empty; owned = Set.add x ys}, ys)) d1

    | IApp(_, _) -> failwith "Not Implemented"
    | ILet(_, _, _, _) -> failwith "Not Implemented"
    | IMatch(_, _) -> failwith "Not Implemented"
    | ICon(_, _) -> failwith "Not Implemented"
    | Dup(_, _) -> failwith "Not Implemented"
    | Drop(_, _) -> failwith "Not Implemented"
    | DropRu(_, _, _) -> failwith "Not Implemented"
