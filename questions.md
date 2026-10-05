# Questions

Confirm understanding of data constructors

Confirm understanding of let rule condition

Next steps:
Make absyn in fsharp
Similar to fsharp from

## Notes

Unlike the paper we should explicitly have C@r in the language.

Only let has the branches, where we need to make decisions

First step is to do normalisation, then add dup and drop.

Normalization is to avoid arbitrary evaluation with "e e",
we want to get to 'e x'

We normalize to what is called A-Normal form

What makes syntax directed deterministic is the conditions added,
specifically the free variables are important
