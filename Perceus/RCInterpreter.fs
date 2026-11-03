module Interpreter

open Lang

// ----- HELPER TYPES -----
type Loc = int

type Env = Map<IId, Loc>

type Value =
    | VClosure of IId * IExpr * Env
    | VCon of IC * Loc list

type HeapEntry = {
    rc: int
    value: Value
}

type Heap = Map<Loc, HeapEntry>

type State = {
    heap: Heap
    nextLoc: Loc
}

// ----- HELPER FUNCTIONS -----
let alloc (value: Value) (state: State): Loc * State =
    let loc = state.nextLoc

    let entry = {
        rc = 1
        value = value
    }

    let state' = {
        state with
            heap = Map.add loc entry state.heap
            nextLoc = loc + 1
    }

    loc, state'

let dupLoc loc state =
    // Updates reference count for given entry location
    let entry = state.heap[loc]

    let entry' = { entry with rc = entry.rc + 1 }

    { state with heap = Map.add loc entry' state.heap }

// Note this function covers drop, dlam and dcon.
let rec dropLoc loc state =
    let entry = state.heap[loc]

    if entry.rc <= 0 then
        failwith $"Reference count for location {loc} is already zero."

    if entry.rc > 1 then 
        let entry' = { entry with rc = entry.rc - 1}
        
        { state with heap = Map.add loc entry' state.heap }
    else
        // rc = 1, so this object is being freed
        let state' = { state with heap = Map.remove loc state.heap }
        match entry.value with
        // Drop for dlam
        | VClosure (_,_,closureEnv) ->
            // Drop all the free variables in the closure environment
            closureEnv |> Map.values |> Seq.fold (fun st childLoc -> dropLoc childLoc st) state'
        // Drop for dcon
        | VCon (_, fields) ->
            // Drop all the fields in the constructor
            fields |> List.fold (fun st childLoc -> dropLoc childLoc st) state'

// ----- EVALUATION -----
let rec eval (env: Env) (state: State) (expr: IExpr) : Loc * State =
    match expr with
    | IVar x ->
        env[x], state

    | ILam (x, body, freeVars) ->
        // ClosureEnv is a subset of the current environment containing only the free variables needed by the lambda.
        let closureEnv = freeVars |> Set.fold (fun acc fv -> Map.add fv env[fv] acc) Map.empty
        // We don't call eval yet because we want the lambda to be lazy.
        alloc (VClosure (x, body, closureEnv)) state
    
    | ICon (c, xs) ->
        let fields = xs |> List.map (fun x -> env[x])
        alloc (VCon (c, fields)) state

    | IApp (f, y) ->
        // Evaluate the function expression to its heap location.
        let fLoc, state1 = eval env state f
        match state1.heap[fLoc].value with
        | VClosure (x, body, closureEnv) ->
            // Evaluate the argument expression to its heap location.
            let yLoc = env[y]
            
            // dup z
            let state2 = closureEnv 
                        |> Map.values
                        |> Seq.fold (fun st loc -> dupLoc loc st)
                         state1
            // drop f
            let state3 = dropLoc fLoc state2

            // Equivalent to e[x := y]
            let bodyEnv = closureEnv |> Map.add x yLoc
            eval bodyEnv state3 body
        
        | _ -> failwith "Attempted to apply a non-function"

    | IMatch (y, branches) ->
        match state.heap[env[y]].value with
        | VCon (c, zs) ->
            match branches |> List.tryFind (fun ((c', _), _) -> c = c') with
            | Some ((_, xs), e) ->
                // Bind pattern variables to the constructor's field locations (this is the equivalent of e[x := z] in the paper)
                let branchEnv =
                    List.zip xs zs
                    |> List.fold (fun acc (x, loc) ->
                        Map.add x loc acc) env

                eval branchEnv state e
            | _ -> failwith $"No matching branch found for constructor {c}"

        | _ -> failwith "Attempted to match on a non-constructor"
    
    | ILet (x, z, e, _) ->
        let loc1, state' = eval env state z
        // e[x := z]
        let env' = Map.add x loc1 env
        eval env' state' e

    | Dup (x, e) ->
        let state' = dupLoc env[x] state
        eval env state' e

    | Drop (x, e) ->
        let state' = dropLoc env[x] state
        eval env state' e

    | DropRu (x, r, e) ->
        expr |> failwith "not implemented" // TODO


// ----- INTERPRETER -----
let interpret expr =
    let initialState = {
        heap = Map.empty
        nextLoc = 0
    }

    let loc, state =
        eval Map.empty initialState expr

    loc, state

let rec showValue (loc: Loc) (state: State) : string =
    match state.heap[loc].value with
    | VCon (c, []) ->
        string c

    | VCon (c, fields) ->
        let args =
            fields
            |> List.map (fun loc -> showValue loc state)
            |> String.concat ", "

        $"{c}({args})"

    | VClosure _ ->
        "<function>"

let run expr = 
    let loc, state = interpret expr
    showValue loc state