---
title: First-class Algebraic Presentations with Elaborator Reflection
author:
  - name: Robert Wright
    email: robert.wright@ed.ac.uk
    affiliation: The University of Edinburgh
  - name: Ohad Kammar
    email: ohad.kammar@ed.ac.uk
    affiliation: The University of Edinburgh
date: 2024-06-04
abstract: |
  We present a library for the ergonomic creation, manipulation, and use of first-order algebras. We do not rely on hard-coded syntactic support for our embedded language. Instead, we use metaprogramming to provide syntactic sugar for creating and using user-definable deeply-embedded first-order algebras.

  We manipulate Idris 2 syntax with elaborator reflection at compile-time. We reinterpret the syntax with non-standard semantics, to provide utilities for creating, and writing terms in, user-defined algebras. We show how to use this to evaluate in an algebra internally to a category.
idris2-packages:
  - semantic-reflection
header-includes:
  - \usepackage{stmaryrd}
  - \newcommand\sem[1]{\left\llbracket{#1}\right\rrbracket}
citation-style: acm-sig-proceedings
link-citations: true
---

# Introduction

Deeply-embedded domain-specific languages (eDSLs) are useful [@when-dsl], but writing full data structures for them can be cumbersome. So it is convenient to have syntactic sugar for eDSLs. Such sugar is usually provided by either runtime string interpretation [@mysqli], or hard-coded syntax in the host language [@linq].

We instead use metaprogramming to provide eDSL syntactic sugar. This allows us to provide the syntactic sugar as a library, while still performing compile-time checks.

We will be using Idris 2's [@idris] elaborator reflection [@elab-reflection]. This restricts our syntactic sugar to code that is syntactically Idris 2, but with arbitrary semantics. Elaborator reflection gives us access to the Idris 2 type-checker, and other elaboration mechanisms. The contribution of this work is the use of elaborator reflection for first-class syntax.

# Elaborator Reflection

*Reflection* is a form of metaprogramming where a language is able to inspect itself [@reflection]. *Elaboration* is when the surface syntax of a language is turned into the core language [@idris].

So *elaborator reflection* is a form of metaprogramming where the language is given access to its own elaborator. We have objects representing the surface syntax, and the ability to elaborate terms into objects.

The surface syntax tends to be designed for humans, with lots of syntactic sugar, and "obvious" information left implicit. The core language tends to be designed for machines, being very explicit, with lots of typing information. So elaboration needs to fill in the gaps, inferring implicit arguments, and type-checking the result.

Idris 2 provides three different sorts of quoted syntax: quoted expressions, which use `` `() ``; quoted top-level declarations, which use `` `[] ``; and quoted names, which use `` `{} ``. We mostly use quoted expressions, but also use the other forms.

# Single-sorted Algebra

## Syntax

We will show this library by example, in parallel with the mathematical equivalents. We will define an example single-sorted algebra [@single-sorted]. An example many-sorted algebra can be found in appendix \ref{many-sorted-algebra}.

To define a *single-sorted syntax*, we need a collection of operations, each with an associated arity. For example, suppose we wish to define the syntax of monoids. Mathematically, we might express it like this:
$$
\mathrm{MonoidSyn} = \{e \mapsto 0, (*) \mapsto 2\}
$$

While with our library, we express it like this:

```{.idr .hide}
import Data.List
import Syntax.SingleSorted

%language ElabReflection
```

```idr
MonoidSyn : Syntax
MonoidSyn = `(\case e => 0; (*) => 2)
```

Note the backtick before the case block. The `Syntax`{.idr} elaboration script is run on this quoted syntax. It expects a case block, and constructs a `Syntax`{.idr} object, a first-class representation of monoid syntax.

## Context

For single-sorted syntax, a *context* is a collection of variable names. For example, for a context containing only the variables $x$ and $y$, mathematically we might write $[x, y]$. Or in our library, `[<"x", "y"]`{.idr type="SnocList ?"}.

We are not using reflection here. The `[<...]` is Idris 2 syntax for snoc-lists. We view contexts as extending towards the right, so a snoc-list representation is more ergonomic.

## Term

*Terms* are trees. The nodes are operations, with as many children as the arity of the operation; and leaves are variables.

For example, $x * (x * e) * y$ is a term in monoid syntax and context $[x, y]$. And in our library:

```idr
someTerm : Term MonoidSyn [<"x", "y"]
someTerm = `(x * (x * e) * y)
```

This time, the backtick shifts us into our custom syntax. The variables `x` and `y` are implicitly introduced, as are the operations `e` and `*`. We use dependent types so that using variables not in scope, or operations with the wrong number of arguments, is a compile-time error.

We reuse Idris 2's infix operation parsing. The `*` here is infix, and left-associative, as it is defined in the Idris 2 standard library as such. Following Idris 2 conventions, alphanumeric operations are prefix, while special character operations are infix, with user definable associativity and precedence.

## Environment

An *environment* for a context and collection of values is an assignment of variables in the context, to values. For example, in the context $[x, y]$, with lists of natural numbers as our values, we can define the environment that assigns $x$ the value $[1, 2]$, and $y$ the value $[3, 4]$. Or, more tersely: $[x = [1, 2], y = [3, 4]]$. Equivalently in our library:

```idr
someEnv : Env [<"x", "y"] (List Nat)
someEnv = [<[1, 2], [3, 4]]
```

As with contexts, we use snoc-lists, as environments grow to the right. We use dependent types to ensure environments have the right number of values.

## Interpretation

An *interpretation* is a set of values, and, for each operation, a function on that set, of appropriate arity. For example, concatenation of lists is an interpretation of monoid syntax. We use lists as our values, and assign $\{e \mapsto [], (*) \mapsto (++)\}$.

```idr
Interp MonoidSyn (List a) where
    impl = `(\case e => []; (*) => (++))
```

With an interpretation, we can now define evaluation of a term. Given a term and an environment, we recursively fold our term, by looking up variables in the environment, and applying the associated functions for each operation.

## Splicing

The backtick gets us into our syntax, but sometimes it is nice to get back out. Suppose we have a term $t$ in monoid syntax, and we want to multiply it by a variable $x$. Mathematically, we abuse notation and write $x * t$, despite $x$ being a variable, and $t$ a metavariable.

In Idris 2, we would ideally write `` `(x * ~(t)) ``{.idr type="{t : TTImp} -> ?"}, but elaborator reflection splicing is specialized Idris 2 syntax, and we cannot override it for our custom syntax.

We instead define *opening* a syntax. Analogously to opening a module, this allows us to locally refer to something defined elsewhere. At the top-level, we run the macro `openSyn`{.idr}. This macro defines meta-level equivalents of the operations of a syntax. For example, for monoid syntax, we run `` %runElab openSyn `{MonoidSyn} ``{.idr namespace="MonoidSyn" .decl}, to define the following functions:

```{.idr namespace="MonoidSyn2"}
e : Interp MonoidSyn a => a
(*) : Interp MonoidSyn a => a -> a -> a
```

For our previous example, we can now write `` `(x) * t ``{.idr namespace="MonoidSyn" type="{t : Term MonoidSyn [<\"x\"]} -> ?"}. This `*` is the meta-level operation.

## Theory

An *algebraic theory* has equations between terms. For example, associativity $x * (y * z) = (x * y) * z$. We use quoted top-level declarations `` `[] `` to give names to the axioms:

```idr
MonoidThy : Theory MonoidSyn
MonoidThy = `[assoc : x * (y * z) = (x * y) * z
              leftId : e * x = x
              rightId : x * e = x]
```

## Model

A *model* is an interpretation that satisfies a theory. We enforce this with dependent types:

```idr
Model MonoidThy (List a) where
    int = MkInterp `(\case e => []; (*) => (++))

    satThy = [<
        Prf appendAssociative,
        Prf $ \xs => Refl,
        Prf appendNilRightNeutral]
```

The proofs `appendAssociative`{.idr .expr type="List Nat -> ?"} and `appendNilRightNeutral`{.idr .expr type="List Nat -> ?"} come from the Idris 2 standard library.

Similarly to opening syntax, we can open a theory. This creates Idris level accessors for the proofs of a theory. For example, `` %runElab openThy `{MonoidThy} ``{.idr namespace="MonoidSyn" .decl} creates:

```{.idr .hide namespace="MonoidSyn2"}
%hide Prelude.(*)
%hide MonoidSyn.(*)
```

```{.idr namespace="MonoidSyn2"}
assoc : Model MonoidThy a =>
        (0 x : a) -> (0 y : a) -> (0 z : a) ->
        x * (y * z) = (x * y) * z
leftId : Model MonoidThy a =>
         (0 x : a) -> MonoidSyn.e * x = x
rightId : Model MonoidThy a =>
          (0 x : a) -> x * MonoidSyn.e = x
```

```{.idr .hide namespace="MonoidSyn2"}
%hide MonoidSyn.e
```

# Comparison with Interfaces

We could define monoids as an interface.

```{.idr .hide file="Interface"}
%hide Prelude.Monoid
```

```{.idr file="Interface"}
interface Monoid a where
    e : a
    (*) : a -> a -> a

    assoc : {x : a} -> {y : a} -> {z : a} ->
            x * (y * z) = (x * y) * z
    leftId : {x : a} -> e * x = x
    rightId : {x : a} -> x * e = x
```

## Automatic Variable Introduction

Suppose we want to define a term in monoid syntax, with a variable `x`{.idr type="{x : Term MonoidSyn [<\"x\"]} -> ?"} in context. If we are using interfaces, we have the operations `e` and `*` in scope at the Idris 2 level, but not the variable `x`{.idr type="{x : Term MonoidSyn [<\"x\"]} -> ?"}. The variable `x`{.idr type="{x : Term MonoidSyn [<\"x\"]} -> ?"} is represented by some element of our `Term`{.idr} type, which could be a string `"x"`{.idr}, or some proof `Var Here`{.idr type="Term MonoidSyn [<\"x\"]"}.

So to write the term `e * x`{.idr namespace="MonoidSyn2" type="{x : Term MonoidSyn [<\"x\"]} -> ?"}, we need to introduce the variable `x`{.idr type="{x : Term MonoidSyn [<\"x\"]} -> ?"}. We could do this with a `let` binding, and write something like `let x = Var Here in e * x`{.idr namespace="MonoidSyn2" type="Term MonoidSyn [<\"x\"]"}.

But with our approach, all variables in context are introduced automatically. So we would write the more compact `` `(e * x) ``{.idr type="Term MonoidSyn [<\"x\"]"}.

## Universal Algebra

Interfaces allow us to prove things about *one* algebra. We chose to use a generic underlying data-type, which allows us to prove things across *multiple* algebras at once. For example, you could define a notion of variable use across all syntaxes, instead of just for one.

```{.idr .hide file="UniAlg"}
import Data.List.Monoid
import Data.Vect.Pointwise
```

We demonstrate use of this approach in our library. For single-sorted syntax, we define a `Pointwise`{.idr file="UniAlg" type="Model MonoidThy (Vect 1 (List Nat))"} model, two different notions of variable use, and a property on theories.

The `Pointwise`{.idr file="UniAlg" type="Model MonoidThy (Vect 1 (List Nat))"} model takes a model of any single-sorted theory on a type `a`{.idr file="UniAlg" type="{a : Type} -> Type"}, and lifts it to a model on type `Vect n a`{.idr file="UniAlg" type="{a : Type} -> {n : Nat} -> Type"}, on the same theory, by applying the operations pointwise.

## Universe Homogeneity

```{.idr .hide file="UniHom"}
e : {u : Type -> Type} -> Nat
(*) : {u : Type -> Type} -> Nat -> Nat -> Nat
```

In more involved examples, Idris 2 cannot infer which universe we are working in. This can make writing expressions unwieldy when using interfaces. When we need to disambiguate universes, we would need to do it for all operations in an expression. For example, if we define sized monoids as an interface, the expression `e * x`{.idr namespace="MonoidSyn2" type="{x : Term MonoidSyn [<\"x\"]} -> ?"} in universe `u`{.idr type="{u : Type -> Type} -> ?"} would need to be written as something like `(*) {u} (e {u}) x`{.idr file="UniHom" type="{u : Type -> Type} -> {x : Nat} -> ?"}.

But while the sorts of subexpressions in a term may vary, we generally intend all subexpressions to live in the same universe. So if we instead write this expression by evaluating `` `(e * x) ``{.idr}, then we only need to specify the universe once. This effect gets more pronounced as expressions get longer.

# Internal Algebra

Our approach can be used to evaluate a term internally to a category. We will first define what it means to evaluate a term in a category.

Suppose we have some Cartesian category $C$, and a many-sorted syntax. For each sort $a$ of the syntax, we choose an object $O_a$ of $C$. Then, for each operation $\mathrm{op} : a_1 \times \cdots \times a_n \rightarrow b$, we choose an arrow $f_\mathrm{op} : O_{a_1} \times \cdots \times  O_{a_n} \rightarrow O_b$.

We interpret a context $\Gamma = [x_1 : a_1, \ldots, x_n : a_n]$, as the object $\sem{\Gamma} = O_{a_1} \times \cdots \times O_{a_n}$ of $C$, and interpret  terms of sort $a$ in context $\Gamma$ as an arrow $\sem{\Gamma} \rightarrow O_a$.

We proceed inductively. A variable is interpreted as the appropriate projection map $\sem{ x } = \pi_x$. An operation is interpreted as the product of the interpretations of its arguments, composed with the arrow of the operation.
$$
\sem{ \mathrm{op}(t_1, \ldots, t_n) } = f_\mathrm{op} \circ \langle \sem{t_1}, \ldots, \sem{t_n} \rangle
$$

For example, consider the syntax of linear algebra. We take the natural numbers as our sorts, and $m \times n$ matrices as our arrows $n \rightarrow m$. Our product is addition, by stacking matrices on top of each other, and our projection maps are slices of the identity matrix $I_{n + m}$. As our operations, consider $(+) : n \rightarrow n \rightarrow n$, and $(++) : n \rightarrow m \rightarrow n + m$. Think of our sorts as $n$-dimensional vector spaces, with $+$ as addition, and $++$ as concatenation.

Now consider the context $\Gamma = [x : 1, y : 1, z : 2]$, and the term $t = ((x + y + x) {++} y) + z$. We have $\sem{\Gamma} = 1 + 1 + 2 = 4$, and $\Gamma \vdash t : 2$, so $t$ is interpreted as a $2 \times 4$ matrix. Specifically:
$$
\begin{pmatrix}
2 & 1 & 1 & 0 \\
0 & 1 & 0 & 1
\end{pmatrix}
$$
It turns out that categorical evaluation is equivalent to standard evaluation. This follows from Lawvere's account of algebraic theories. A direct proof may be found in appendix \ref{equivalence-of-categoric-and-standard-evaluation}. So we can evaluate terms categorically using our approach, via this equivalence.

# Future Work

## Second-order Syntax

Our approach allows reusing Idris 2 syntax, with different semantics. So for second-order syntax, we could reuse Idris 2 binding syntax for our binding operators, without committing our underlying datatype to the higher-order abstract syntax [@hoas] approach. This allows ergonomic syntax, while allowing easy traversal of constructed terms.

## Models in Categories

As categorical and standard evaluation are equivalent, we could integrate categorical evaluation more deeply into our library. We could explore the space, and work out which problems are most ergonomically solved with categorical evaluation.

# References {.unnumbered}

::: {#refs}
:::

\appendix

# Many-sorted Algebra

## Syntax

We generalize the single-sorted results to many-sorted [@many-sorted]. We now have some collection of sorts. The arity of an operation is now a list of its argument sorts, and its return sort. For example, consider monoids graded [@graded-algebra] by the additive natural number, which we will call *sized monoids*. Directly, our sorts are natural numbers, with the following operations:
$$
\mathrm{SizedMonoidSyn} = \left\{e \mapsto ([], 0), (*)_{(n, m)} \mapsto ([n, m], n + m)\right\}
$$
In our library:

```{.idr .hide file="ManySorted"}
import Data.Vect
import Syntax.ManySorted

%language ElabReflection
```

```{.idr file="ManySorted"}
SizedMonoidSyn : Syntax Nat
SizedMonoidSyn = `[e : 0; (*) : n -> m -> n + m]
```

We use top-level declaration syntax `` `[] ``. But instead of types on the RHS, we have sorts, and use `->` as a separator.

## Context

Contexts are now a collection of name/sort pairs. For example, we write $[x : 1, y : 2]$ for the context with variables $x$ and $y$, of sorts $1$ and $2$, respectively. In our library, we write `` `[x : 1; y : 2] ``{.idr file="ManySorted" type="Context Nat"}.

## Term

As before, terms are trees, but must also be well-sorted. For example, in the syntax of sized monoids, and context $[x : 1, y : 2]$, the term $x * y$ has sort $3$. We leave the subscript of $*$, in this case $(1, 2)$, implicit. In our library:

```{.idr file="ManySorted"}
someTerm : Term SizedMonoidSyn `[x : 1; y : 2] 3
someTerm = `(x * y)
```

We use dependent types to enforce well-scoped and well-sorted terms.

## Environment

An environment now needs some way of turning sorts into collections of values. We call this a *universe*. For example, if our sorts are natural numbers, our values may be lists of that length. So for context $[x : 1, y : 2]$, we could take the environment $[x = [1], y = [2, 3]]$. The sorts are correct, as $[1]$ has length $1$, and $[2, 3]$ has length $2$. In our library:

```{.idr file="ManySorted"}
someEnv : Env `[x : 1; y : 2] (\n => Vect n Nat)
someEnv = [<[1], [2, 3]]
```

We use dependent types to ensure each assignment has the right sort.

## Interpretation

Our functions must now respect sorts. By clever choice of example, we can still use concatenation of lists for sized monoids:

```{.idr file="ManySorted"}
Interp SizedMonoidSyn (\n => Vect n a) where
    impl = `[e = []
             xs * ys = xs ++ ys]
```

## Splicing

As in the single-sorted case, we define `openSyn`, the syntax opening macro.

```{.idr file="ManySorted" namespace="SizedMonoidSyn"}
%runElab openSyn `{SizedMonoidSyn}
```

The meta-level operations now enforce the sorts. For example, for sized monoids:

```{.idr file="ManySorted" namespace="SizedMonoidSyn2"}
e : Interp SizedMonoidSyn u => u 0
(*) : Interp SizedMonoidSyn u =>
      u n -> u m -> u (n + m)
```

## Theory

Variables in theories can now be annotated with sorts.

```{.idr file="ManySorted"}
SizedMonoidThy : Theory SizedMonoidSyn
SizedMonoidThy = `[
    assoc : {x : n} -> {y : m} -> {z : p} ->
            x * (y * z) = (x * y) * z
    leftId : {x : n} -> e * x = x
    rightId : {x : n} -> x * e = x]
```

Unbound variables are implicitly assumed to quantify over all sorts. This is analogous to Idris 2's unbound implicits behaviour.

## Model

As in the single-sorted case, we use dependent types to enforce models satisfy their theories.

```{.idr file="ManySorted"}
Model SizedMonoidThy (\n => Vect n a) where
    int = `[e = []
            xs * ys = xs ++ ys]
    satThy = ?satThy
```

We omit the proofs in this example for brevity. The proofs use heterogeneous equality, as the sorts of each side of the equation need not match.

# Equivalence of Categoric and Standard Evaluation

We will show that evaluation in a locally small category is equivalent to "standard" evaluation. In one direction we specialize by choosing the category $\mathrm{Set}$. In the other we do a Yoneda embedding.

First, let us show that categorical evaluation gives us standard evaluation. So let us evaluate categorically in $\mathrm{Set}$. For each sort we need an object in $\mathrm{Set}$, a set of values; and for each operator we need an arrow, a function on values. This is exactly the information required for standard evaluation.

Evaluating a term categorically with this data gives us a function from environments to values. Expanding the definitions, we see that it is precisely the function for standard evaluation of the term in those environments. So categorical evaluation gives us standard evaluation.

Now let us go in the other direction. Suppose we have standard evaluation, and we want to do categorical evaluation in a locally small category $C$. For each sort $a$, we have some objects $O_a$, and for each operation $\mathrm{op}$, an arrow $f_\mathrm{op}$.

As we are doing standard evaluation, we need to choose a set $X_a$ for each sort $a$. First, let us fix an object $B$. Then we take $X_a$ to be the Yoneda embedding $X_a = Y O_a B = C(B, O_a)$, the collection of $C$ arrows $B \rightarrow O_a$. This is a set as $C$ is locally small.

We now need to choose a function for each operation $\mathrm{op} : a_1 \times \cdots \times a_n \rightarrow b$. We take the function $(g_1, \ldots, g_n) \mapsto f_\mathrm{op} \circ \langle g_1, \ldots, g_n \rangle$. Note that each $g_i$ is an arrow $B \rightarrow O_{a_i}$, and the result is an arrow $B \rightarrow O_b$, as required.

Now take some term $\Gamma \vdash t : a$. Choose $B = \sem{\Gamma}$, and standardly evaluate $t$ with the above data, in the projections environment $[x_1 = \pi_{x_1}, \ldots, x_n = \pi_{x_n}]$. This gives us an arrow $\sem{\Gamma} \rightarrow O_a$. Working through the definitions, it is precisely the categorical evaluation of $t$ in $C$.
