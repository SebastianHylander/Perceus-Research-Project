module State 

type State = {
    counter : int;
    used_vars : Map<string, int>;
    names : Map<int, string>
}

let new_state () = {counter = 0; used_vars = Map.empty; names = Map.empty}

let inc_counter s = (s.counter, {s with counter = s.counter + 1})

let get_fresh_id str s = 
    let i, s = inc_counter s
    i, {s with
            used_vars = Map.add str i s.used_vars
            names = Map.add i str s.names}

let get_id str s = 
    match Map.tryFind str s.used_vars with
    | Some i -> i, s
    | None -> get_fresh_id str s


