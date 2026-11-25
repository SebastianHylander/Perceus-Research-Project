module ReuseMonad
open Lang

type Sizes = Map<IId, int>

type State = {
    counter: int;
    s : Sizes;
}

let new_state () = { counter = -1; s = Map.empty }

let inc_counter s = s.counter, {s with counter = s.counter - 1}


type 'a statemonad = SM of (State -> ('a * State))

let bind (SM f) (g : 'a -> statemonad<'b>) =  
    SM (fun m ->  
        let x, m = f m
        let (SM h) = g x
        h m
    )

let new_state_monad () = 
    SM (fun m ->
        (), new_state ()
    )

let inc_counter_monad () =
    SM (fun m ->
        inc_counter m
    )

let get_s () = SM( fun state -> state.s, state ) 

let set_s new_s = SM( fun state -> (), {state with s = new_s} ) 

let ret x = SM (fun m -> x, m)
let (>>=) a f = bind a f
let (>>>=) a b = a >>= (fun _ -> b)

let sequence lst =
    List.foldBack
        (fun x acc ->
            x >>= fun x ->
            acc >>= fun xs ->
            ret (x :: xs))
        lst
        (ret [])

