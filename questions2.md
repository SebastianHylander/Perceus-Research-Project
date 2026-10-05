# Questions and Answers

## Session 2

### How do we efficiently generate fresh vars, monad or mutable reference?

Rename everything into numbers, variables are also used as heap
locations (mapping from string to number var, so we can reuse it)

Also renaming bound variables, so they are unique

Global counter

(potentially reader monad from var to number)

### Should we have value type in our input language?

Its just a predicate, needed for evaluation, we can keep it as
Expr and just run a check

(Even if the syntax does not allow a constructor to have fx an application,
it should be allowed for us. But this needs to be mentioned in our paper)

### What is the next goal, define lambda1n as syntax directed?

Whatever works best for us
It is non-deterministic currently, we have to fix this (fx by making it syntax directed)

## Session 2 Extra

### "(co) inductive data types" and "either inductive or coinductive"

depends on if we look at a type as constructing a fx list by combining
cons and nil, this is inductive as in fsharp, or if we deconstruct an exisisting,
potentially infinite list, to get a cons, this is coinductive

### "and (4) multiplicity of each member in Δ, Γ is 1"

Each instance counts as 1, so "gamma,x" adds one instance of x to the multiset

### Notes

The global counter can be higher than owned variables, because variables can
be in borrowed, and will later be dropped by some other code.
