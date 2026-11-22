# Questions and Answers

## Session 1

### Do we understand data constructors correctly?

yes, the bar above v is a list of values,
and when we drop v_bar we need to drop all elements

### Do we understand let rule condition correctly?

Only let has the branches, where we need to make decisions
So this is the only place we need the condition after normalization

### What are the next steps: Make absyn in fsharp?

First step is to do normalisation, then add dup and drop.

Normalization is to avoid arbitrary evaluation with "e e",
we want to get to 'e x'

We normalize to what is called A-Normal form

## Session 1 Notes

Unlike the paper we should explicitly have C@r in the language.

What makes syntax directed deterministic is the conditions added,
specifically the free variables are important

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
<<<<<<< Updated upstream
=======

## Session 3

### Should let closure keep x in it? (They seem to have both in the paper)

No, the rules we implement check for x being in the closure, so to make this work we should not.
They are inconsistent in the papers, so we need to write in the report that this is our choice.

### Recursion in the interpreter, how should this be handled?

- slet: are we allowed to keep x as borrowed when evaluating e1?

Top level definitions are special and can be recursive,
and we do not want to reference count them.

### Should we use a result monad instead of failwith?

If it keeps the code simpler, keep the failwith, as this is not the important part.


### Should RCIntepreter do the drop and dup? Should they not be expressions added in the IExpr already, and the intepreter just executes them

Some things need to be added dynamically, so dub and drop should be added in both places.

### Should we implement the Invariants as tests for debugging
Might be a nice extra to have

### Extra
delta and gamma can be implemented as reader monads

## Session 4

# Supervision - reuse

## Questions

### The rules for reuse never remove variables from R, should the Con not do this?

Answer: We should add this rule

### Should we make the same choices as the paper for the choice points?

Answer: Yes, we should follow their

### Should we call up, if a reuse var is not used?

"we actually do this in a single pass by passing all available reuse
tokens R statefully and after transforming the sub expression we only apply drop-reuse if the reuse
token was actually used (and otherwise we leave it unchanged as rdrop)."

So when we come to an expression were reuse vars does not match the expected size, we should call up ?


Answer: 
We could have R -> R' in every rule, as a way to enforce how reuse variables are used.
We should make the reuse rules in this format so x -> x, has R -> R
and drop -> dropru would check that R' no longer contains r

So it would look like S |- e|R -> r' | R'


#### Maybe todo
We can add mutual recursion, just go through all top level expressions
Example traversing a tree

>>>>>>> Stashed changes
