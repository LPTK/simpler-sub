# Simpler-sub Algorithm for Type Inference with Subtyping

This repository shows the implementation of **Simpler-sub**,
an alternative algorithm to [Simple-sub](https://github.com/LPTK/simple-sub) which is much easier to understand but also much more limited,
though it is probably enough for many practical use cases.

An online demo is available here: https://lptk.github.io/simpler-sub/


## Simplifications

By contrast to Simple-sub, Simpler-sub does not support:

  * (1) Recursive types: any recursive constraint will yield an error instead.
    
    If your language has externally-defined recursive data types (such as algebraic data types),
    you don't strictly need recursive types anyway.
    But in order to support field-recursing definitions of the form `let foo x = ... foo x.f ...`, you'll need a way to make it clear to the type checker which recursive data type's `f` field this `foo` recurses on
    (otherwise you'll get a recursive constraint error between inferred record types).
    One way to do that is to either reject the use of overloaded field names (like Haskell 98)
    or use a type class for field selection instead of subtyping (like recent Haskell)
    or use contextual information to disambiguate field selections when possible (like OCaml).
    
    The absence of recursive types makes some of the type inference algorithms simpler and more efficient,
    as they can now freely decompose types inductively without having to carry a cache around.
    
  * (2) Nested let polymorphism: in this prototype, a _local_ (i.e., nested) `let ... in ...` binding will never be assigned a polymorphic type.
  
    This simplifies the approach further in that we don't have to deal with levels.
    And there is precedent for it,
    for example see the paper [Let Should not be Generalised](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf) by Dimitrios Vytiniotis, Simon Peyton Jones, and Tom Schrijvers.
  
  * (3) Precise type-variable-to-type-variable constraints: any constraint between two type variables immediately unifies the two variables.
    
    The consequence of this is not as dire as one may think,
    thanks to pervasive polymorphism (though also see Restriction 2).
    Programs that would typically exhibit problematic loss of precision with this approach are those similar to
    `let test f x y = if f x then x else y`,
    which is typed by Simple-sub as `('a -> bool) -> 'a ∧ 'b -> 'b -> 'b`,
    but is now typed as `('a -> bool) -> 'a -> 'a -> 'a`
    – notice that `y` is forced to be typed as `'a`, the parameter type taken by `f`,
    even though it never flows into `f` and has in fact nothing to do with it.
    
    In Simple-sub, and in algebraic subtyping in general,
    precise graphs of type variable inequalities are recorded,
    and then need to be simplified aggressively before being displayed to the user.
    Unifying type variables aggressively, on the other hand,
    forces inferred type graphs to be almost as simple as in traditional Hindley-Milner type inference,
    and makes inferred type simplification much easier.

Restriction (3) destroys principal type inference:
there may now be well-typed terms that the type inference approach rejects.

Restriction (1) each destroy the principal type property:
there are now terms which cannot be ascribe a single most precise type
– those which would have been typed by Simple-sub through a recursive type,
but which can still be given less precise non-recursive types.
Simpler-sub will in fact plainly reject any such term.

Any of these simplification could be made independently, starting from Simple-sub.
I chose to implement them all together in this project to show how simple the result could look like,
while still being quite useful as a possible foundation for type inference in languages with subtyping.


