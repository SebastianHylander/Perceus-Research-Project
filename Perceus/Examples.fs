module Examples

type Example = {
    Name : string
    Source : string
}

let map = """
let map = fun xs -> fun f ->
    match(xs) {
        Cons(x,xx) {
            Cons (f x, map xx f)
        }
        Nil() -> Nil()
    }
in map end
"""

let e1 = """
let e1 = fun xs -> fun f ->
    match(xs) {
        Cons(x,xx) {
            match(f x) {
                Error(err) -> { Error(err); }
                Ok(y) -> { match(e1 xx f) {
                    Error(err) -> Error(err)
                    Ok(ys) -> Cons(y,ys)
                }}
            }
        }
    }
in e1 end
"""

let identity = """
fun x -> x
"""

let predecessor = """
let pred = fun n ->
    match(n) {
        Zero() -> Zero()
        Succ(k) -> k
    }
in
pred Succ(Succ(Zero()))
end
"""

let shadowing = """
let x = Zero() in
match(x) {
    Zero() -> x
    Succ(x) -> x
}
end
"""

let all = [
    { Name = "map"; Source = map };
    { Name = "e1"; Source = e1 };
    { Name = "identity"; Source = identity };
    { Name = "predecessor"; Source = predecessor };
    { Name = "shadowing"; Source = shadowing }
]
