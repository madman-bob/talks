---
title: First-class Algebraic Presentations with Elaborator Reflection
author:
  - \underline{Robert Wright}
  - Ohad Kammar
institute: The University of Edinburgh
idris2-packages:
  - semantic-reflection
colorlinks: true
---

# Overview

<!--

The idea behind this work is this: Suppose we're creating a deeply embedded language. There are lots of things we need for this: a parser, a type-checker, maybe type-inference. But we've already got a perfectly good parser/type-checker/type-inference --- the host language! If our embedded language is similar enough to our host language, can we reuse bits of our host language, to speed up development of our embedded language?

We will be using Idris 2's elaborator reflection, to access some of the internals of Idris 2. The embedded languages we've looked at are singe- and many-sorted algebras.

-->

- Algebraic presentations
- First-class algebraic presentations
- Elaboration
- Elaborator reflection
- Put it all together

<!--

As a high-level overview of this talk:

First, I'll give a bit of motivation by talking about algebraic presentations.

Then I'll talk about encoding algebraic presentations as an embedded language, and show some of the problems that come up.

Then I'll give a brief introduction to elaboration, leading onto elaborator reflection.

Finally, I'll put these together, and show how we can use elaborator reflection for ergonomic first-class algebraic presentations.

-->

# Algebraic Presentations

<!--

An algebraic presentation is a mathematical object encoding the syntax of an algebra.

This allows us to manipulate algebraic structures themselves. This area's known as universal algebra.

For example, in many-sorted syntax, an algebraic presentation is a collection of operations, along with, for each operation, a list of sorts, and its result sort.

The list of sorts is called the operations arity. It represents the sorts of the arguments to the operation, and the result sort is the sort of the fully applied operation.

Here's an example algebraic presentation in many-sorted syntax:

-->
$$
\mathrm{SizedMonoidSyn} = \left\{\begin{array}{ll}
e &\mapsto ([], 0) \\
(*)_{n, m} &\mapsto ([n, m], n + m)
\end{array}\right\}
$$
<!--

For people familiar with graded algebras, I define a "sized monoid" to be a monoid graded by the additive natural numbers.

For everyone else, I'll explain it directly.

A sized monoid has morally two, but technically infinitely many operations. We've got a constant $e$, of sort $0$, and a binary operation $*$. The binary operation takes arguments of arbitrary sorts, and we get the result sort by adding together the argument sorts.

An example of a sized monoid would be vectors with concatenation. We take $e$ to be the empty vector, and given two vectors of known length, concatenation adds the lengths together.

-->

# First-class Algebraic Presentations {.fragile}

```{.idr .hide file="Algebra"}
import Syntax.ManySorted

%default total
%language ElabReflection

SizedMonoidSyn : Syntax Nat
SizedMonoidSyn = `[
    e : 0
    (*) : n -> m -> n + m
  ]
```

<!--

So, algebraic presentations are useful, but encoding them in a programming language tends to be quite verbose.

Even something as simple as this term with variables $x$ and $y$:

-->
$$
t = x * y
$$

. . .

<!--

Becomes something like this:

-->

```{.idr file="Algebra" namespace="A"}
someTerm : Term SizedMonoidSyn [<"x" :! 1, "y" :! 2] 3
someTerm = Operation (MkOp Here) [<1, 2] [<
    Var (There Here),
    Var Here
  ]
```

<!--

This work is a library in Idris 2, so all of my code snippets will be in Idris 2.

Even a very simple expression has to be on multiple lines to fit on the slide.

Let's break this down.

On the first line, we're declaring the type of `someTerm`. We're declaring it as a term in sized monoid syntax. The `Term` type comes from my library. The user defined `SizedMonoidSyn`. We're in a context with variables `x` and `y`, of sorts `1` and `2` respectively. The term as a whole has sort `3`.

On the second line, we start defining `someTerm`. We're applying some operation to two variables. Precisely which operation and which variables is not immediately obvious, as we're using proofs that the variables and operations are in scope, instead of their names. But you can take my word for it, that it's the same as the maths version.

-->

. . .

<!--

As a little teaser for what's coming, wouldn't it be much nicer if we could write it like this:

-->

```{.idr file="Algebra" namespace="B"}
someTerm : Term SizedMonoidSyn `[x : 1; y : 2] 3
someTerm = `(x * y)
```

<!--

Here, we're using Idris 2's Elaborator Reflection to switch into sized monoid syntax.

I'll explain this example in more detail later.

Notably, we don't make any changes to Idris 2. We use metaprogramming to reinterpret fragments of Idris 2 syntax, which allows us to provide this syntax as a library.

-->

# Elaboration {.fragile}

```{.idr .hide file="Elab"}
import Language.Reflection
```

<!--

We reinterpret syntax using elaborator reflection. To explain elaborator reflection, I'll first give a high-level idea of what elaboration is.

Elaboration is a large topic, so what I'm about to tell you barely scratches the surface.

Roughly speaking, we have a high-level surface syntax which leaves "obvious" things implicit, and a lower-level syntax, where everything is explicit.

Even a simple example like this has a lot of things left implicit:

-->

```{.idr file="Elab" type="IO ()"}
printLn $ 1 + 1
```

. . .

<!--

We can partially elaborate this example to this:

-->

```{.idr file="Elab" .expr}
printLn {io = IO} {a = Integer}
    @{the (HasIO IO) %search}
    @{the (Show Integer) %search} $
    (+) {ty = Integer} @{the (Num Integer) %search} 1 1
```

<!--

Here we've made explicit the implicit arguments: `io`, `a`, and so on. To fully elaborate this example, we'd also need to do interface resolution, give names to the interfaces, and replace the `%search`es with those names.

The elaborator is responsible for things like unification, interface resolution, and proof search.

-->

# Elaborator Reflection {.fragile}

<!--

On to elaborator reflection.

Elaborator reflection is a form of metaprogramming, where a language is given access to its own elaborator.

In this work, we mainly use syntax quoting, and elaboration scripts.



Quoting syntax gives us a code object, without evaluating it.

There are three different sorts of quote in Idris 2. These are expressions, declarations, and names:

-->

```{.idr file="Elab"}
someExpr : TTImp
someExpr = `(x * y)

someDecls : List Decl
someDecls = `[
    f : Nat -> Nat
    f x = 1 + x
  ]

someName : Name
someName = `{x}
```

<!--

The quotes are indicated by a backtick, and different sorts of brackets.

This is just raw syntax.

The `x` and `y` in `someExpr` are not evaluated, and indeed not even bound to anything. We don't define a function `f` in `someDecls`.



The other main thing we use is elaboration scripts. Elaboration scripts allow us to generate code at compile-time.

Put these things together, and we can reinterpret fragments of Idris 2 syntax with different semantics.

-->

# Many-Sorted Syntax {.fragile}

```{.idr .hide}
import Syntax.ManySorted

import Data.Vect

%default total
%language ElabReflection
```

<!--

So let's take a look at how we can use the library.

Here's the example many-sorted syntax from earlier:

-->
$$
\mathrm{SizedMonoidSyn} = \left\{\begin{array}{ll}
e &\mapsto ([], 0) \\
(*)_{n, m} &\mapsto ([n, m], n + m)
\end{array}\right\}
$$
<!--

And here's how it looks with our library:

-->

```idr
SizedMonoidSyn : Syntax Nat
SizedMonoidSyn = `[
    e : 0
    (*) : n -> m -> n + m
  ]
```

<!--

We're declaring `SizedMonoidSyn` as a `Syntax Nat`. The `Syntax` type comes from our library, and we're using `Nat` as our sorts.

Then we use a quoted declaration, which you may recognize from the previous slide. But the type has changed. It's `Syntax Nat`, not `List Decl`. That's because an elaboration script is reinterpreting it.

Syntactically, it looks like we're declaring two Idris 2 functions. But instead of types, the RHSs are sorts. The elaboration script expands these to the definitions of what operations are in our many-sorted syntax.

-->

# Term {.fragile}

<!--

We can now revisit the teaser from earlier. We're defining a term in our syntax.

-->

```idr
someTerm : Term SizedMonoidSyn `[x : 1; y : 2] 3
someTerm = `(x * y)
```

<!--

This is the same example from a few slides ago. We've got two backticks here.

In the type signature, we are using Idris 2 declaration syntax again. This time we're interpreting it as a context. Our sorts are still `Nat`s, so we're declaring that we've got a context with `x` of sort `1`, and `y` of sort `2`.

We then actually define the term. This time we're using Idris 2 expression syntax, instead of declaration. The elaboration script switches us into sized monoid syntax, implicitly introducing the operations `e` and `*`, and the variables `x` and `y`.

We reuse Idris 2's infix operation parsing. The `*` here is infix, and left-associative, as it is defined in the Idris 2 standard library as such. Following Idris 2 conventions, alphanumeric operations are prefix, while special character operations are infix, with user definable associativity and precedence.

-->

# Open Syntax {.fragile}

<!--

So now that we've got terms, how do we manipulate them?

I define the notion of "opening" a syntax, analogously to Agda's opening a module. This creates Idris 2-level equivalents of the operations of a syntax.

If we run this elaboration script:

-->

```{.idr namespace="Open"}
%runElab openSyn `{SizedMonoidSyn}
```

```{.idr .hide}
%hide Prelude.(*)
%hide Open.e
%hide Open.(*)
```

<!--

We generate functions with the following signatures:

-->

```idr
e : Interp SizedMonoidSyn u => u 0
(*) : Interp SizedMonoidSyn u =>
      u n -> u m -> u (n + m)
```

<!--

Note that these are Idris 2 level functions now, not operations in a syntax.

The sorts have to be wrapped in a universe `u`, which converts the sorts into Idris 2 types.

-->

. . .

<!--

Here's an example of using `*`:

-->

```idr
square : {n : Nat} ->
         Term SizedMonoidSyn ctx n ->
         Term SizedMonoidSyn ctx (n + n)
square t = t * t
```

<!--

We've moved to the meta-level now. `t` is a term, not a variable, and `*` is a function in Idris 2, not an operation in our syntax.

-->

. . .

```{.idr .hide namespace="Unquote"}
square : TTImp -> TTImp
```

```{.idr namespace="Unquote"}
square t = `(~(t) * ~(t))
```

<!--

As a side note, I would have preferred to do unquoting instead of opening syntax, like this. While the backtick takes us into the syntax, the tilde takes us back out.

But unfortunately, elaborator reflection hard-codes the assumption that unquotes use `TTImp`, and I didn't want to dig too deeply into the internals of elaborator reflection.

-->

# Interpretation {.fragile}

```idr
[VectInterp] Interp SizedMonoidSyn (\n => Vect n a) where
    impl = `[
        e = []
        xs * ys = xs ++ ys
    ]
```

<!--

We give interpretations to terms with the `Interp` interface, which is defined in the library.

The first line here is just a standard Idris 2 named interface implementation. We're defining an implementation called `VectInterp` of the `Interp` interface, for `SizedMonoidSyn`, with a universe of `Vect`.

I'm using a named implementation here because we're varying over the first parameter of `Vect`, and that confuses Idris 2 a little.

For the actual implementation, we're reinterpreting an Idris 2 case block, to allow us to pattern match on the operations of our syntax. The RHSs of the case block are the interpretations of the operations. These are interpreted with standard Idris 2 semantics, as objects of the right type and sorts.

So for `e`, we need to give a vector of length 0, while for `*` we're given vectors of length `n` and `m`, and need to give a vector of length `n + m`.

-->

. . .

<!--

With a datatype for environments:

-->

```idr
someEnv : Env `[x : 1; y : 2] (\n => Vect n Nat)
someEnv = [<[1], [2, 3]]
```

<!--

We can now do evaluation:

-->

```idr
eg : Vect 3 Nat
eg = unsafeEval @{VectInterp} someEnv someTerm
```

<!--

The `Env` type and `unsafeEval` function both come from my library.

We define an environment with variables `x` and `y`, of sorts `1` and `2` respectively, in the universe of `Vect` of `Nat`.

So the first variable is `Vect` of length `1`, and the second is a `Vect` of length `2`, matching the sorts.

The `<` symbol is snoc-list notation, as we think of contexts and environments as extending to the right, and this representation works more ergonomically with that.

The final `eg` is a standard Idris 2 function application. We're evaluating the previous term in this interpretation and environment, which gives us a `Vect` of length `3`.

-->

# Theory {.fragile}

```idr
SizedMonoidThy : Theory SizedMonoidSyn
SizedMonoidThy = `[
    assoc : {x : n} -> {y : m} -> {z : p} ->
            x * (y * z) = (x * y) * z
    leftId : {x : n} -> e * x = x
    rightId : {x : n} -> x * e = x
  ]
```

<!--

We can now talk about theories for our syntax.

A theory is a collection of equations between terms.

Similarly to before, we're using Idris 2 declaration syntax, with sorts where you'd expect the types to go.

This theory defines three classes of equations, associativity, and left- and right-identity. We parameterize over the sorts, so technically this is infinitely many equations.

-->

# Open Theory {.fragile}

<!--

Similarly to opening a syntax, we define the notion of opening a theory. This gives Idris 2 level accessors to the proofs of a theory.

If we run this elaboration script:

-->

```{.idr namespace="Open"}
%runElab openThy `{SizedMonoidThy}
```

<!--

We get the following functions:

-->

```idr
assoc : Model SizedMonoidThy u =>
        {x : u n} -> {y : u m} -> {z : u p} ->
        x * (y * z) ~=~ (x * y) * z
leftId : Model SizedMonoidThy u => {x : u n} ->
         e * x ~=~ x
rightId : Model SizedMonoidThy u => {x : u n} ->
          x * e ~=~ x
```

<!--

Similarly to opening a syntax, these are Idris 2 level functions now, so we need to wrap the sorts in a universe.

The `e` and `*` here are the ones defined by opening the syntax.

We're also using heterogeneous equality here, because the sorts might not line up. In `assoc` and `rightId`, the sorts are propositionally equal, but not judgementally equal.

I'm uncertain about the choice of using heterogeneous equality. For this example, we could instead require a proof that the sorts are equal when we defined the theory. We would then be able to use homogeneous equality here. But it could be the case that there's a compelling example where we explicitly *don't* want the sorts to be equal.

-->

# Model {.fragile}

```{.idr .hide}
namespace Utility
    export
    typesEq : {0 x : a} -> {0 y : b} -> (0 prf : x ~=~ y) -> a = b
    typesEq Refl = Refl

    export
    hcong : (0 p : a -> Type) ->
            (0 _ : Injective p) =>
            {0 q : a -> Type} ->
            (0 f : {i : a} -> p i -> q i) ->
            {0 x : p i} ->
            {0 y : p j} ->
            (0 prf : x ~=~ y) ->
            f x ~=~ f y
    hcong p f xy = do
        let pij = typesEq xy
        let Refl = irrelevantEq $ injective pij
        let Refl = irrelevantEq xy
        Refl
```

```idr
[VectModel] Model SizedMonoidThy (\n => Vect n a) where
    int = VectInterp

    satThy = [<
        MkSatAxiom $ \[<n, m, p], [<x, y, z] => vectAppendAssociative,
        MkSatAxiom $ \[<n], [<x] => Refl,
        MkSatAxiom $ \[<n], [<x] => vectAppendNilRightNeutral
      ]
```

<!--

We prove interpretations satisfy theories with the `Model` interface, which is defined in the library.

Similarly to interpretations, the first line here is defining a `Model` implementation called `VectModel`, for sized monoid theory, in a universe of `Vect`.

We provide the interpretation `VectInterp` for the underlying syntax, and then prove that this interpretation satisfies the axioms of sized monoid theory, using standard dependent type techniques.

The proofs are omitted here. As we're using heterogeneous equality, they're a bit ugly, but they're not much longer than the homogeneous equivalents.

-->

```{.idr .hide}
      where
        vectAppendAssociative : {xs : Vect n a} -> {0 ys : Vect m a} -> {0 zs : Vect p a} -> xs ++ (ys ++ zs) ~=~ (xs ++ ys) ++ zs
        vectAppendAssociative {xs = []} = Refl
        vectAppendAssociative {xs = x :: xs} =
            hcong (\n => Vect n a) @{MkInjective $ \Refl => Refl} (x ::) $
            vectAppendAssociative {xs} {ys} {zs}

        vectAppendNilRightNeutral : {xs : Vect n a} -> xs ++ [] ~=~ xs
        vectAppendNilRightNeutral {xs = []} = Refl
        vectAppendNilRightNeutral {xs = x :: xs} =
            hcong (\n => Vect n a) @{MkInjective $ \Refl => Refl} (x ::) $
            vectAppendNilRightNeutral {xs}
```

. . .

<!--

Finally, we can now do evaluation in our model, safe in the knowledge that our operations satisfy the laws of our theory:

-->

```idr
eg2 : Vect 3 Nat
eg2 = eval @{VectModel} someEnv someTerm
```

# Conclusion

## Summary

- <!-- You can --> Reinterpret fragments of your host syntax <!-- as syntax in your embedded language -->
- <!-- You can --> Use Elaborator Reflection <!-- to infer the "obvious" bits of your syntax, similarly to how elaboration infers the "obvious" bits of the host language -->

## Code

- \url{https://github.com/madman-bob/semantic-reflection}

## Future Work

- Second-order syntax

  <!--

  This approach allows us to use the binding syntax of the host language, without restricting us to the host semantics. For example, as with HOAS.

  -->
