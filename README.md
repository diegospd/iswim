# My ISWIM

This is a toy implementation of [Landin](https://en.wikipedia.org/wiki/Peter_Landin)'s [ISWIM](https://en.wikipedia.org/wiki/ISWIM).
It seeks to be as cool as Haskell, but is terribly underpowered yet.

## Features
 * Small [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus); just 8 constructions
 * Extensible set of primitives
 * [Rank-1 polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism)
 * [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) type system
 * Decidable [type inference](https://en.wikipedia.org/wiki/Type_inference)
 * Proven [type safety](https://en.wikipedia.org/wiki/Type_safety)

## Operational Semantics
 * [Formal semantics](https://www.sciencedirect.com/science/article/pii/S1571066106000429)
 * [Call by value](http://homepages.inf.ed.ac.uk/wadler/topics/call-by-need.html)
 * [Eager evaluation](https://en.wikipedia.org/wiki/Eager_evaluation)
 * Custom [CEK machine](https://www.cs.umd.edu/class/summer2015/cmsc330/material/cek/)

## Lacking
 * [Algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type)
 * [Type clasess](https://en.wikipedia.org/wiki/Type_class) (needs [type annotations](https://en.wikipedia.org/wiki/Type_signature))
 * [Objects](http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html) & [object-oriented](https://lispcast.com/object-oriented-vs-functional-duals/) programming
 * [Imperative programming](https://pdfs.semanticscholar.org/5d39/afc383e7e7a323859b74da812422151e26a3.pdf) features
 

## Haskell coolness
 * Painless monadic code (State + Error)
