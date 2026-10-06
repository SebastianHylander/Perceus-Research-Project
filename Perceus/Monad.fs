module Monad

open State

type 'a statemonad = SM of (State -> ('a * State))

let bind (SM f) (g : 'a -> statemonad<'b>) =  
    SM (fun m ->  
        let x, m = f m
        let (SM h) = g x
        h m
    )

let sm_frame (SM f) = 
    SM (fun m ->
        let x, m' = f m
        x, {counter = m'.counter; used_vars = m.used_vars}
    )

let new_state () = 
    SM (fun m ->
        ((), State.new_state ())
    )



let inc_counter () =
    SM (fun m ->
        State.inc_counter m
    )

let get_id str = 
    SM (fun m ->
        State.get_id str m
    )

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
