module Interpreter

open Lang

// ----- HELPER TYPES -----
type Loc = int

type Env = Map<IId, Loc>
type ReuseEnv = Map<IId, Loc option>

type Value =
    | VClosure of IId * IExpr * Env
    | VCon of IC * Loc list
    | VReuse of int // reserved constructor storage: number of fields

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

// Note this function covers: drop, dlam and dcon.
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
        | VReuse _ -> state'

// Note: this function covers: dropru, dlamru and dconru. It returns a retains a location for the reusable value.
let dropReuseLoc (loc: Loc) (state: State): Loc option * State =
    let entry = state.heap[loc]

    match entry.value with
    | VCon (_, fields) when entry.rc = 1 ->
        // Retain the constructor cell, but remove its old references.
        let reserved = {
            rc = 1
            value = VReuse (List.length fields)
        }

        let state' = {
            state with heap = Map.add loc reserved state.heap
        }

        // Release the references formerly owned by its fields.
        let state'' =
            fields
            |> List.fold (fun st child -> dropLoc child st) state'

        Some loc, state''
    | VReuse _ -> failwith "Cannot apply dropru to a reuse token."
    | _ ->
        // Shared constructor or closure: ordinary drop, no storage.
        None, dropLoc loc state

let allocReuse (token: Loc option) (c: IC) (fields: Loc list) (state: State): Loc * State =
    match token with
    | None ->
        alloc (VCon (c, fields)) state
    | Some loc ->
        let entry = state.heap[loc]

        match entry.value with
        | VReuse capacity when entry.rc = 1 ->
            if capacity = List.length fields then
                let replacement = {
                    rc = 1
                    value = VCon (c, fields)
                }

                // Same location; nextLoc is unchanged.
                loc, {
                    state with
                        heap = Map.add loc replacement state.heap
                }
            else
                // Incompatible storage: release it and allocate.
                alloc (VCon (c, fields)) (dropLoc loc state)

        | _ ->
            failwith "Expected uniquely owned reserved storage."

// ----- EVALUATION -----
[<TailCall>]
let rec evalc (env: Env) (reuse: ReuseEnv) (state: State) (expr: IExpr) (cont: Loc * State -> Loc * State): Loc * State =
    match expr with
    | IVar x ->
        (env[x], state) |> cont

    | ILam (x, body, freeVars) ->
        // ClosureEnv is a subset of the current environment containing only the free variables needed by the lambda.
        let closureEnv = freeVars |> Set.fold (fun acc fv -> Map.add fv env[fv] acc) Map.empty
        // We don't call eval yet because we want the lambda to be lazy.
        alloc (VClosure (x, body, closureEnv)) state |> cont
    
    | ICon (c, xs) ->
        let fields = xs |> List.map (fun x -> env[x])
        alloc (VCon (c, fields)) state |> cont

    | IApp (f, y) ->

        // Evaluate the function expression to its heap location.
        evalc env reuse state f (fun (fLoc, state1) -> 
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
                evalc bodyEnv Map.empty state3 body (fun res -> res |> cont)
            
            | _ -> failwith "Attempted to apply a non-function"
        )

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

                evalc branchEnv reuse state e (fun res -> res |> cont)
            | _ -> failwith $"No matching branch found for constructor {c}"

        | _ -> failwith "Attempted to match on a non-constructor"
    
    | ILet (x, z, e, _) ->
        evalc env reuse state z (fun (loc1, state') ->
            // e[x := z]
            let env' = Map.add x loc1 env
            evalc env' reuse state' e (fun res -> res |> cont)
        )
    | DropRu (x, r, e) ->
        let token, state' = dropReuseLoc env[x] state
        let reuse' = Map.add r token reuse
        evalc env reuse' state' e cont

    // Must appear before the general Drop case.
    | Drop (r, ICon (c, xs)) when Map.containsKey r reuse ->
        let fields = xs |> List.map (fun x -> env[x])
        allocReuse reuse[r] c fields state |> cont

    | Drop (x, e) ->
        let state' =
            match Map.tryFind x reuse with
            | Some (Some loc) -> dropLoc loc state
            | Some None -> state
            | None -> dropLoc env[x] state

        evalc env (Map.remove x reuse) state' e cont

    | Dup (x, e) ->
        let state' = dupLoc env[x] state
        evalc env reuse state' e (fun res -> res |> cont)

let eval (env: Env) (reuse: ReuseEnv) (state: State) (expr: IExpr) = evalc env reuse state expr id

// ----- INTERPRETER -----
let interpret expr =
    let initialState = {
        heap = Map.empty
        nextLoc = 0
    }
    let reuse = Map.empty

    let loc, state =
        eval Map.empty reuse initialState expr

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
    | VReuse _ ->
        "<reserved storage>"

let run expr = 
    let loc, state = interpret expr
    showValue loc state