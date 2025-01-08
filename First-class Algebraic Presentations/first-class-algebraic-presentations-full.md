---
title: First-class Algebraic Presentations with Elaborator Reflection
author:
  - name: Robert Wright
    email: robert.wright@ed.ac.uk
    affiliation: The University of Edinburgh
  - name: Ohad Kammar
    email: ohad.kammar@ed.ac.uk
    affiliation: The University of Edinburgh
abstract: |
  We present an approach for easily constructing embedded languages by reusing fragments of the host language. We demonstrate this approach by creating an embedded language for first-order algebra as an Idris 2 library. We use Idris 2's elaborator reflection to gain access to Idris 2 internals, without modifying the language itself.

  By reusing fragments of the host language, we can avoid having to implement portions of our embedded language ourselves. In our library, we avoid having to implement variable lookup, sort-checking, and sort-inference by reusing parts of the Idris 2 type-checker.
idris2-packages:
  - semantic-reflection
header-includes:
  - \usepackage{stmaryrd}
  - \newcommand\sem[1]{\left\llbracket{#1}\right\rrbracket}
  - \newenvironment{psmallmatrix}{\left(\begin{smallmatrix}}{\end{smallmatrix}\right)}
citation-style: acm-sig-proceedings
link-citations: true
---

# Introduction

Deeply-embedded domain-specific languages (eDSLs) are useful [@when-dsl], but writing an eDSL can be a lot of work. We need to write variable lookup for our language, probably some sort of type-checker, maybe we want type-inference. But we already have a perfectly good variable lookup, type-checker, and type-inference --- in the host language.

In this work, we present an approach for reusing fragments of the host language to develop an eDSL. We demonstrate use of this approach by developing an Idris 2 [@idris] library for single- and many-sorted algebras[^library-url].

[^library-url]: \url{https://github.com/madman-bob/semantic-reflection}

We will be using Idris 2's elaborator reflection [@elab-reflection]. This restricts our syntactic sugar to code that is syntactically Idris 2, but with arbitrary semantics. Elaborator reflection gives us access to the Idris 2 type-checker, and other elaboration mechanisms. We make no modifications to Idris 2 itself, and interfere as little as possible in the elaboration process. Our use of reflection allows us to provide this work as a library, while still providing compile-time checks for our eDSL.

# Elaborator Reflection

To explain elaborator reflection, we will first explain elaboration, then reflection, before putting them together.

## Elaboration

```{.idr file="Elab" .hide}
import Language.Reflection

hasIO : HasIO IO
hasIO = %search

showInteger : Show Integer
showInteger = %search

numInteger : Num Integer
numInteger = %search
```

*Elaboration* is the process of turning the surface syntax of a language into its core representation [@idris].

The surface syntax tends to be designed for humans, with lots of syntactic sugar, and "obvious" information left implicit. The core language tends to be designed for machines, being very explicit, with lots of typing information. Elaboration needs to fill in the gaps, inferring implicit arguments, and type-checking the result.

For example, consider the seemingly simple Idris 2 expression `printLn $ 1 + 1`{.idr file="Elab" type="IO ()"}. This can be partially elaborated like so:

```{.idr file="Elab" type="IO ()"}
printLn {a = Integer} {io = IO}
    @{hasIO} @{showInteger} $
    (+) {ty = Integer} @{numInteger}
        (fromInteger 1 {ty = Integer} @{numInteger})
        (fromInteger 1 {ty = Integer} @{numInteger})
```

This is still the same expression, but now much more explicit. The function `printLn`{.idr file="Elab" type="Integer -> IO ()"} takes multiple implicit arguments: the type of the object to be printed, how to print it, what sort of I/O to use, and so on. These things are "obvious" from context, but something needs to work them out, and that thing is the elaborator.

I say *partially* elaborated, as we also need to do interface resolution. We gave names to the interfaces implementations to reference them in the above snippet. Specifically, the names `hasIO`{.idr file="Elab"}, `showInteger`{.idr file="Elab"}, and `numInteger`{.idr file="Elab"} refer to the standard implementations of the `HasIO IO`{.idr file="Elab"}, the `Show Integer`{.idr file="Elab"}, and the `Num Integer`{.idr file="Elab"} interfaces, respectively. The elaborator would instead give machine generated names to the interface implementations, before figuring out which ones to use here.

## Reflection

*Reflection* is a form of metaprogramming where a language is able to inspect itself [@reflection]. While reflection is most famous in Lisps [@lisp-reflection], some form of reflection has found its way into most mainstream programming languages, including Python [@python-reflection], JavaScript [@javascript-reflection], and Java [@java-reflection]. One way to provide reflection is to have an internal representation of your surface syntax, and the ability to convert terms in the surface syntax into the values they represent.

For example, consider specializing exponentiation using reflection. For a fixed exponent, we create a function that multiplies a variable by itself that many times. Specializing functions can be useful for optimization purposes, as the runtime behaviour is different to using closures. In a Lisp, specializing exponentiation might look something like this:

```lisp
(defun powBody (n)
    (if (= n 0) `1 `(* x ,(powBody (- n 1)))))
(defun pow (n) (eval `(lambda (x) ,(powBody n))))
```

The utility function `powBody`{.lisp} represents the body of the function. In the base case, we return a quoted integer literal. In the recursive case, we quasi-quote `* x`, and unquote in (`,`) the recursive call to `powBody`{.lisp}. The `x` and `*` are not bound to anything at this point, this construct is purely syntactic.

The `pow`{.lisp} function then wraps in a `lambda (x)`, again syntactically. This is then passed to `eval`{.lisp}, which converts our syntax into an actual Lisp lambda, in the process binding `x` and `*`. Concretely, in this example `(pow 3)`{.lisp} would evaluate to the Lisp object `(lambda (x) (* x (* x (* x 1))))`{.lisp}.

## Elaborator Reflection

Putting elaboration and reflection together, *elaborator reflection* is a form of metaprogramming where a language is given access to its own elaborator [@idris]. This allows users to not only turn quoted syntax into actual values, but also gives access to the type-checker, to proof-search, and to raise compile-time errors.

Idris 2 provides three different sorts of quoted syntax: quoted expressions, which use `` `() ``; quoted top-level declarations, which use `` `[] ``; and quoted names, which use `` `{} ``. In this work, we use all these forms of quotation.

Consider again specializing exponentiation. In Idris 2, this might look something like:

```{.idr file="Elab" .hide}
%hide pow
```

```{.idr file="Elab"}
pow : Nat -> Elab (Nat -> Nat)
pow n = check `(\x => ~(powBody n))
  where
    powBody : Nat -> TTImp
    powBody 0 = `(1)
    powBody (S n) = `(x * ~(powBody n))
```

We can now run `pow 3`{.idr file="Elab"} at compile-time to generate the function `\x => x * (x * (x * 1))`{.idr file="Elab" type="Nat -> Nat"}.

At a surface level, this example is very similar to the Lisp example. But elaborator reflection runs at compile-time, while Lisp's reflection happens at runtime. Moreover, through the elaborator we have access to the type-checker, so we can type-check the generated code, again at compile-time. This provides a greater degree of confidence that the generated code is correct.

The return type of the utility function `powBody`, `TTImp`{.idr file="Elab"}, which stands for "Type Theory (Implicit)", is the internal representation of Idris 2's own expressions. The built-in elaboration script `check`{.idr file="Elab" type="TTImp -> Elab (Nat -> Nat)"} runs the type-checker on this quoted code, and reflects it into an actual Idris 2 value. In this instance, a function `Nat -> Nat`{.idr file="Elab"}.

In this work, we use quoted Idris 2 syntax for our own custom syntax for algebraic presentations, and use elaborator reflection to give that syntax semantics. Through elaborator reflection, we reuse the Idris 2 type-checker for well-scoped and well-sorted terms, and reuse Idris 2's proof search for variable lookup and sort inference. Notably, we did not need to write our own type-checker --- we just reused the relevant portions of Idris 2.

# First-class Algebraic Presentations

In this work, we use elaborator reflection to provide first-class algebraic presentations. An *algebraic presentation* is a mathematical description of the operations of an algebra. This allows us to study many algebraic structures at once, an area known as universal algebra [@universal-algebra], or general algebra [@general-algebra].

There are many different sorts of algebraic presentations. In this library, we provide the ability to write algebraic presentations for single-sorted [@single-sorted] and many-sorted syntaxes [@many-sorted]. Given an algebraic presentation, we provide the ability to write and manipulate terms in that algebra. Our algebraic presentations are first-class. We can create them, pass them around, and manipulate them; like any other value.

We will now demonstrate use of the library, in parallel with the mathematical equivalents. We will use a running example in many-sorted syntax.

## Syntax

```{.idr .hide}
import Data.Vect
import Syntax.ManySorted

%language ElabReflection
```

We start with defining a many-sorted algebraic presentation. We represent this by the library's `Syntax`{.idr} data type. We call this type "syntax" rather than "algebraic presentation" as a user need not know about the underlying representation. From their perspective, all that's relevant is that it represents a many-sorted syntax.

A many-sorted algebraic presentation consists of three things. A collection of "sorts", a collection of "operators", and a mapping from operators to "arities". For many-sorted algebraic presentations, an *arity* is a list of the argument sorts, and the return sort.

For example, consider monoids graded [@graded-algebra] by the additive natural numbers, which we will call *sized monoids*. Phrased directly, our sorts are natural numbers, and we have an operation $e$, and an operation $(*)_{(n, m)}$ for each pair of natural numbers $n$, and $m$. The operation $e$ takes no arguments, with return sort $0$, and the operation $(*)_{(n, m)}$ takes two arguments, of sorts $n$ and $m$, with a return sort of $n + m$. Mathematically, we might represent the algebraic presentation of sized monoids like so:
$$
\mathrm{SizedMonoidSyn} = \left\{e \mapsto ([], 0), (*)_{(n, m)} \mapsto ([n, m], n + m)\right\}
$$
An example of a sized monoid is vectors under concatenation. The empty vector has length $0$, and concatenating two vectors adds the lengths.

In our library, we express sized monoids like this:

```{.idr}
SizedMonoidSyn : Syntax Nat
SizedMonoidSyn = `[e : 0; (*) : n -> m -> n + m]
```

Our `Syntax`{.idr} type is indexed by the type of sorts, in this case `Nat`{.idr}. For the definition of `SizedMonoidSyn`{.idr}, we then use quoted Idris 2 top-level declaration syntax `` `[] ``. Syntactically, these are top-level Idris 2 declarations. But our elaboration script reinterprets them as declarations of operations in our algebra. Instead of types on the RHS of the declarations, we have sorts, and the `->` is a purely syntactic separator.

Metavariables in syntax declarations, such as `n`{.idr type="{n : Nat} -> Nat"} and `m`{.idr type="{m : Nat} -> Nat"} above, are implicitly bound, and their types inferred automatically. The former reuses Idris 2's type-signature unbound implicits behaviour, and the latter Idris 2's type inference.

## Context

Before we can define terms in our algebra, we must first define contexts. In many-sorted syntax, a *context* is a collection of name/sort pairs. For example, mathematically, we might write $[x : 1, y : 2]$ for the context with variables $x$ and $y$, of sorts $1$ and $2$, respectively. In our library, we write `` `[x : 1; y : 2] ``{.idr type="Context Nat"}.

As before, we are using declaration syntax, and we index by the type of sorts. The RHS of the declarations are again interpreted not as types, but as sorts.

## Term

We can now define terms in our algebra. A *term* in a context, in many-sorted syntax, is a tree, where branches are operations and leaves are variables in the context, that is well-sorted. Such a tree is *well-sorted* when each operation in the tree has an many children as its arity, and the sort of each child matches the corresponding sort in its arity. The sort of a variable is looked up from the context, and the sort of a branch is the return sort of its corresponding operation.

For example, in the syntax of sized monoids, and context $[x : 1, y : 2]$, the term $x * y$ is well-defined, and has sort $3$. We leave the subscript of $*$, in this case $(1, 2)$, implicit. In our library:

```{.idr}
someTerm : Term SizedMonoidSyn `[x : 1; y : 2] 3
someTerm = `(x * y)
```

This time we are using Idris 2 quoted expression syntax `` `() ``. The variables `x`, `y`, and `*` don't exist at the Idris 2 level. Instead they are interpreted as operations and variables in the given syntax and context. We use dependent types to enforce well-scoped and well-sorted terms. Invalid terms are rejected at compile-time. More details about how this works are in section \ref{library-internals}.

We reuse Idris 2's infix operator parsing. The `*` here is infix, and left-associative, as it is defined in the Idris 2 standard library as such. Following Idris 2 conventions, alphanumeric operations are prefix, while special character operations are infix, with user definable associativity and precedence.

## Environment

To define evaluation of terms, we first need to define environments. An *environment* for a context consists of a mapping from sorts to collections of values, called a *universe*, and a mapping from variables in the context to values in the collection of corresponding sort. For example, if our sorts are natural numbers, we could take vectors of fixed length as our universe of values. Then for context $[x : 1, y : 2]$, we could take the environment $[x = [1], y = [2, 3]]$. The sorts are correct, as $[1]$ has length $1$, and $[2, 3]$ has length $2$. In our library:

```{.idr}
someEnv : Env `[x : 1; y : 2] (\n => Vect n Nat)
someEnv = [<[1], [2, 3]]
```

We are not using reflection in the second line. The `[<...]` is Idris 2 syntax for snoc list literals. We view contexts as extending towards the right, so a snoc-list representation is more ergonomic. We use dependent types to ensure each assignment has the right sort.

## Interpretation

We can now interpret terms. An *interpretation* of a many-sorted algebra is a sort indexed universe of values and, for each operator in the algebra, a corresponding operation, which is a function of appropriate arity in that universe.

For example, we can interpret sized monoid syntax by concatenation of vectors. We take vectors of fixed length as our universe of values, the empty vector as $e$, and concatenation as $*$. Mathematically, we might write this as $\{e \mapsto [], (*) \mapsto (++)\}$. In our library, we write:

```{.idr}
Interp SizedMonoidSyn (\n => Vect n a) where
    impl = `[
        e = []
        xs * ys = xs ++ ys
    ]
```

We are using quoted declaration syntax again, this time to define our operations. We introduce the arguments to the operation on the LHS, and define the result on the RHS. The sorts of the operation are enforced by dependent types.

Note that we have not yet defined any laws that the interpretation must satisfy. For example, `*` need not be associative. As an example of this, consider the universe of natural numbers at most $n$, with constant $0$, and binary operation absolute difference.

With an interpretation of an algebra, we can define evaluation of terms. Variables look up their values from the environment, while operations recursively apply the corresponding functions in the interpretation.

## Syntax Opening

We have methods of creating and interpreting terms, so now let's consider manipulating terms. Suppose we have a term $t$ in monoid syntax, and we want to multiply it by a variable $x$. Mathematically, we abuse notation and write $x * t$, despite $x$ being a variable, and $t$ a metavariable.

To express this manipulation, we define the notion of *opening* a syntax. Analogously to opening a module in Agda [@open-module], this allows us to locally refer to something defined elsewhere. In this case, we can refer at the Idris 2 level to operations defined in our algebra.

To do this, we run the macro `openSyn`{.idr}, at the top-level. For example, for sized monoid syntax, we run `` %runElab openSyn `{SizedMonoidSyn} ``{.idr namespace="SizedMonoidSyn" .decl}, to define functions of the following signatures:

```{.idr namespace="SizedMonoidSyn2"}
e : Interp SizedMonoidSyn u => u 0
(*) : Interp SizedMonoidSyn u =>
      u n -> u m -> u (n + m)
```

```{.idr .hide}
%hide Prelude.(*)
%hide SizedMonoidSyn2.e
%hide SizedMonoidSyn2.(*)
```

As we are now in Idris 2 land, we need to interpret the operations in some universe, hence the `Interp`{.idr type="Syntax s -> (s -> Type) -> Type"}, and assorted `u`{.idr type="{u : Nat -> Type} -> ?"}s. The type signature of the operations enforces the sort-correctness of the operations.

For our previous example, we can now write `` `(x) * t ``{.idr type="{t : Term SizedMonoidSyn [<\"x\" :! 1] 1} -> ?"}. The `x` is quoted, as it is a variable in the context. The `t` is *not* quoted, as it is a metavariable (from the perspective of our algebra). The `*` is the meta-level operation if it is not quoted, and an operation in our algebra if it is.

### Quoted Name

Note that in the `openSyn`{.idr} macro, we use quoted name `` `{SizedMonoidSyn} ``{.idr}, instead of referring to `SizedMonoidSyn`{.idr} directly.

If the macro referred to `SizedMonoidSyn`{.idr} directly, the generated functions would still have the right signatures. But the increased term size, by using the value of the algebraic presentation instead of its name, makes us much more likely to hit the Idris 2 maximum ambiguity depth. This causes Idris 2 to fail elaboration on even simple terms. So each use of `e`{.idr type="Interp SizedMonoidSyn u => u 0"} and `(*)`{.idr type="Interp SizedMonoidSyn u => {n : _} -> {m : _} -> u n -> u m -> u (n + m)"} would need to be manually disambiguated.

So we instead use quoted names, to avoid this unnecessary disambiguation. Unfortunately, this demonstrates that the Idris 2 elaborator is not stable under substitution.

### Unquoting

We can also unquote within Idris 2 quoted expressions. This allows us to have parts of quoted expressions that *are* evaluated. For example, if `t`{.idr type="{t : TTImp} -> ?"} evaluates to `` `(y) ``{.idr}, then `` `(x * ~(t)) ``{.idr type="{t : TTImp} -> ?"} evaluates to `` `(x * y) ``{.idr}.

Instead of syntax opening, we would have liked to use unquoting for manipulating terms. But unfortunately, Idris 2 unquoting is hard-coded to `TTImp`{.idr}, the type of quoted Idris 2 expressions. We cannot override it for our custom syntax.

## Theory

Now that we can create, manipulate, and use terms, we would like to prove things about our terms. We focus on algebraic theories. An *algebraic theory* for a syntax is a collection of equations between terms in that syntax, closed under deduction. Similarly to syntaxes, we can provide a *presentation* of an algebraic theory. This is a collection of equations from the theory, called *axioms*, from which all other equations in the theory can be deduced.

For example, in the theory of sized monoids, our axioms are associativity, left identity, and right identity. Mathematically, we might write associativity as $\forall x, y, z. x * (y * z) = (x * y) * z$. In our library, we write this theory as:

```idr
SizedMonoidThy : Theory SizedMonoidSyn
SizedMonoidThy = `[
    assoc : {x : n} -> {y : m} -> {z : p} ->
            x * (y * z) = (x * y) * z
    leftId : {x : n} -> e * x = x
    rightId : {x : n} -> x * e = x
  ]
```

We use quoted declaration syntax for our theories. This allows us to give names to the axioms.

Variables in axiom declarations can be optionally explicitly bound. Explicit binding allows fine-grained control of the sorts of variables used in equations. Unbound variables are implicitly assumed to quantify over all sorts. This reuses Idris 2's unbound implicits behaviour. Metavariables used in the sorts are also implicitly introduced, and we reuse Idris 2's type-inference to infer their types.

## Model

An interpretation that satisfies a theory is called a *model* of that theory. We use dependent types to enforce that single- and many-sorted models satisfy their theories:

```idr
Model SizedMonoidThy (\n => Vect n a) where
    int = `[
        e = []
        xs * ys = xs ++ ys
    ]
    satThy = ?satThy
```

We omit the proofs in this example for brevity. The proofs are written using standard dependent type methods. The proofs use heterogeneous equality [@het-eq], as the sorts of each side of the equation need not match.

Similarly to opening syntax, we define opening a theory. This creates Idris 2-level accessors for the proofs of a theory. For example, `` %runElab openThy `{SizedMonoidThy} ``{.idr .decl} creates:

```{.idr namespace="SizedMonoidSyn2"}
assoc : Model SizedMonoidThy u =>
        (0 x : u n) -> (0 y : u m) -> (0 z : u p) ->
        x * (y * z) ~=~ (x * y) * z
leftId : Model SizedMonoidThy u =>
         (0 x : u n) -> SizedMonoidSyn.e * x ~=~ x
rightId : Model SizedMonoidThy u =>
          (0 x : u n) -> x * SizedMonoidSyn.e ~=~ x
```

As with opening a syntax, we reuse the names within the theory as the Idris 2-level names, and stick everything in a universe.

### Homogeneous Equality

Instead of using heterogeneous equality, we could use homogeneous equality. This would require proving equality of sorts at theory declaration time. For example, defining associativity of sized monoids would require associativity of natural number addition. On the other hand, we would then have the proof available when constructing models.

There are fewer theories using homogeneous equality than heterogeneous equality. Any theory with homogeneous equality can immediately be interpreted as a theory with heterogeneous equality, but we cannot necessarily do the reverse. For example, with homogeneous equality, we cannot add the axiom $\forall x. x * x = x$ to the theory of sized monoids, as $1 + 1 \neq 1$. I am not aware of any compelling examples that rely on heterogeneous equality.

# Comparison with Interfaces

As we're reinterpreting the meaning of operations, and interfaces reinterpret the meaning of operations, a natural question to ask is: "Can we do this with interfaces?"

Indeed, the `openSyn`{.idr} and `openThy`{.idr} elaboration scripts generate functions that look an awful lot like interface constraints. We could just collect those together as an interface. For example, this would define sized monoid theory like so:

```{.idr file="Interface" .hide}
%hide Prelude.(*)
```

```{.idr file="Interface"}
interface SizedMonoid (0 u : Nat -> Type) where
    e : u 0
    (*) : u n -> u m -> u (n + m)

    assoc : {x : u n} -> {y : u m} -> {z : u (n + m)} ->
        x * (y * z) ~=~ (x * y) * z
    leftId : {x : u n} -> e * x ~=~ x
    rightId : {x : u n} -> x * e ~=~ x
```

A model of sized monoid theory is then an implementation of this interface. But our approach gives us more.

Using interfaces does not give us a representation for terms in the syntax, and when we make a representation, consideration must be given to variable binding. Our approach gives users a type for terms, and automatic variable introduction.

Another issue with interfaces is that there is additional structure not captured. Not only are sized monoids expressible with an interface, it is an algebra. Not only is it an algebra, it is a first-order many-sorted algebra. As we haven't captured that additional structure, we cannot take advantage of it. With our approach that additional structure is available to the user. This links in with universal algebra [@universal-algebra], and allows us to prove properties across multiple algebras simultaneously.

Using interfaces also causes issue in compound expressions, like `x * y * z`{.idr file="Interface" type="SizedMonoid u => {x : u n} -> {y : u m} -> {z : u p} -> u (n + m + p)"}. The two `*`s might refer to different instances of the interface. But when we work in an algebra, there is only one `*` it can refer to --- the one in the algebra. Our approach enforces that terms stay in one universe.

This "universe homogeneity" has another benefit. If the universe we're working in has a complicated type, manual disambiguation may be needed. If we use interfaces, we will need to disambiguate every operation in a term, which can get quite verbose. In this situation, our approach requires disambiguating only once.

## Automatic Variable Introduction

Let us now compare variables in terms with interfaces and our approach more deeply.

The underlying data representation for terms in the library uses well-scoped, well-sorted de Bruijn indices. This enforces a lot of type safety, but can be quite verbose. For example, consider the term $x * y$, in sized monoid syntax. We can write the representation of this term in full as:

```{.idr namespace="T1"}
t : Term SizedMonoidSyn `[x : 1; y : 2] 3
t = Operation (MkOp Here) [<1, 2] [<
    Var (There Here),
    Var Here
  ]
```

Using interfaces, we can pull the `Operation`{.idr type="(op : Op syn) -> (i : Env op.index Prelude.id) -> Env (anonCtx op.arity i) (Term syn ctx) -> Term syn ctx (op.result i)"} into a `(*)`{.idr type="Interp SizedMonoidSyn u => {n : _} -> {m : _} -> u n -> u m -> u (n + m)"}, and write it as:

```{.idr namespace="T2"}
t : Term SizedMonoidSyn `[x : 1; y : 2] 3
t = do
    let x = Var (There Here)
    let y = Var Here
    x * y
```

But we still need to bind `x`{.idr type="{x : Nat} -> ?"} and `y`{.idr type="{y : Nat} -> ?"}. It is very easy to bind these variables incorrectly, misleading the user. Perhaps the user doesn't know that we're counting from the right, instead of the left. Perhaps the user added a new variable to the context, and forgot to update the variable bindings.

This issue can be partially mitigated by annotating the variables with their sorts.

```{.idr namespace="T3"}
t : Term SizedMonoidSyn `[x : 1; y : 2] 3
t = do
    let x : Term _ _ 1 = Var (There Here)
    let y : Term _ _ 2 = Var Here
    x * y
```

If the variables in the context all have different sorts, then this ensures that the variable referred to is the one the user intended. Of course, there could still be an issue if multiple variables in the context have the same sort.

Instead, our library automatically introduces the variables in context:

```{.idr namespace="T4"}
t : Term SizedMonoidSyn `[x : 1; y : 2] 3
t = `(x * y)
```

This is more concise than any of the interface approaches. Further, the user doesn't need to know our internal representation for variables, and any changes to the context are immediately reflected at the term level.

## Universal Algebra

Defining an algebra as an interface allows proving things for that algebra. But it does not allow for proving things across multiple algebras at once, as interfaces allow for much more than algebras. For that, we need a description of what sort of algebras we are working with.

Our library provides representations for syntax and theories of single- and many-sorted algebras. This allows us to, for example, define a property across all many-sorted theories simultaneously. This is not possible using interfaces.

```{.idr file="SingleSorted" .hide}
import Data.Vect.Pointwise
```

We demonstrate a few examples of universal algebra in the library. We define two notions of variable use --- one for single-sorted syntax, and one for single-sorted theories --- and classify for which theories they coincide.

We also define the `Pointwise`{.idr file="SingleSorted" type="{syn : Syntax} -> {n : Nat} -> {thy : Theory syn} -> Model thy a => Model thy (Vect n a)"} model, which takes a model of any single-sorted theory on a type `a`{.idr type="{a : Type} -> ?"}, and lifts it to a model on type `Vect n a`{.idr type="{n : Nat} -> {a : Type} -> ?"} for the same theory.

## Universe Homogeneity

In examples with more complicated universes, Idris 2 cannot always infer which universe we are working in. This requires us to manually disambiguate it. If we were to use interfaces, we would need to disambiguate every operation in an expression. But while the sorts of subexpressions in a term may vary, we generally intend all subexpressions to live in the same universe.

For example, if we define sized monoids as an interface, the expression `e * x`{.idr type="{x : Term SizedMonoidSyn [<\"x\" :! 1] 1} -> ?"} in universe `u`{.idr type="{u : Type -> Type} -> ?"} would need to be written as something like `(*) {u} (e {u}) x`{.idr type="Interp SizedMonoidSyn u => {n : _} ->{x : u n} -> u n"}. With our approach, we would instead write this expression by evaluating `` `(e * x) ``{.idr type="Term SizedMonoidSyn [<\"x\" :! 1] 1"}. This way we only need to specify the universe once. This effect gets more pronounced as expressions get longer.

Idris 2 does provide `with` disambiguation, but unfortunately that does not help in this case. `with` disambiguation allows programmers to provide hints to the compiler about what name to use. For example, the expression `[1, 2, 3]`{.idr type="List Nat"} could be a `List Nat`{.idr}, or it could be a `Vect 3 Nat`{.idr}. Normally, Idris 2 disambiguates based on types, but that is not always possible. For example, `sum [1, 2, 3]`[^no-highlighting] works for both `List Nat`{.idr} and `Vect 3 Nat`{.idr}, as both are `Foldable`{.idr}.

As list literals desugar to a combination of `(::)`{.idr type="a -> List a -> List a"}s and `Nil`{.idr type="List a"}s, we can disambiguate the expression by disambiguating `Nil`{.idr type="List a"}. The `Nil`{.idr type="List a"} for `List`{.idr} is in `Prelude`, while the `Nil`{.idr type="Vect 0 a"} for `Vect`{.idr} is in `Data.Vect`, so writing `with Prelude.Nil sum [1, 2, 3]`{.idr type="Nat"} clarifies that we mean the `[1, 2, 3]`{.idr type="List Nat"} to be a `List Nat`{.idr}.

[^no-highlighting]: Note absence of syntax highlighting, as my highlighter requires snippets type-check.

Disambiguating sized monoid operations when defined as an interface requires disambiguating implicits, and interfaces. But `with` disambiguation works with names, not implicits or interfaces. An alternative to universe homogeneity would be to extend `with` disambiguation to work with these cases.

# Internal Algebra

## Evaluation in a Category

We will now link "standard" evaluation of terms, as defined in section \ref{interpretation}, with evaluation of terms internally to a category. This requires us first define what evaluation of terms internally to a category is.

Given a many-sorted syntax $S$, and a Cartesian category $C$, we will interpret the syntax in $C$. First, for each sort $a$ in $S$, we choose an object $\sem{a}$ in $C$. Then we can interpret a context $\Gamma = [x_1 : a_1, \ldots, x_n : a_n]$, as the object $\sem{\Gamma} = \sem{a_1} \times \cdots \times \sem{a_n}$ of $C$. Now, for each operation $\mathrm{op}$ of $S$, of signature $([a_1, \ldots, a_n], b)$, we choose an arrow $\sem{\mathrm{op}} : \sem{a_1} \times \cdots \times \sem{a_n} \rightarrow \sem{b}$.

We can now interpret terms in $S$ as arrows in $C$. Given a term $t$ in context $\Gamma$ of sort $a$, we interpret $t$ as an arrow $\sem{\Gamma} \rightarrow \sem{a}$. We proceed inductively. A variable is interpreted as the corresponding projection map. An application is interpreted by composing the arrow for the operation with the pairing of the interpretations of the recursively defined operands.
$$
\sem{ \mathrm{op}(t_1, \ldots, t_n) } = \sem{\mathrm{op}} \circ \langle \sem{t_1}, \ldots, \sem{t_n} \rangle
$$

## Example: Linear Algebra

For example, consider the syntax of linear algebra. For our category, let us take the natural numbers as our objects, and $m \times n$ matrices as our arrows $n \rightarrow m$, with matrix product as composition. The product of objects in this category is addition, the product of arrows is stacking matrices on top of each other, and our projection maps are slices of the identity matrix $I_{n + m}$.

Concretely, suppose we have a $1 \times 4$ matrix $A$, and a $2 \times 4$ matrix $B$, with

\begin{center}

\begin{tabular}{c c}

$A = \begin{pmatrix} 1 & 2 & 3 & 4 \end{pmatrix}$, &

$B = \begin{pmatrix} 5 & 6 & 7 & 8 \\ 9 & 10 & 11 & 12 \end{pmatrix}$.

\end{tabular}

\end{center}

Then $A$ and $B$ are arrows $4 \rightarrow 1$ and $4 \rightarrow 2$, respectively, and their product $\langle A, B \rangle$ is an arrow $4 \rightarrow 3$. Specifically,
$$
\langle A, B \rangle = \begin{pmatrix} 1 & 2 & 3 & 4 \\ 5 & 6 & 7 & 8 \\ 9 & 10 & 11 & 12 \end{pmatrix}
$$
The projection maps $\pi_1 : 3 \rightarrow 1$, $\pi_2 : 3 \rightarrow 2$ extract the corresponding rows of this larger matrix.

\begin{center}

\begin{tabular}{c c}

$\pi_1 = \begin{pmatrix} 1 & 0 & 0 \end{pmatrix}$, &

$\pi_2 = \begin{pmatrix} 0 & 1 & 0 \\ 0 & 0 & 1 \end{pmatrix}$.

\end{tabular}

\end{center}

We can think of the projection maps either as slices of the larger identity matrix, or as smaller identity matrices, padded by $0$s. The former perspective comes from the result that the product of projection maps is the identity map, so stacking the projection matrices on top of each other should give us the identity matrix. The latter perspective comes from thinking about the projections directly --- we want some rows from our product matrix unchanged, and none of the other rows.

For our syntax, let us take the natural numbers as our sorts. As our operations, consider $(+_n) : n \rightarrow n \rightarrow n$, and $({++}_{n, m}) : n \rightarrow m \rightarrow n + m$. We think of our sorts as $n$-dimensional vector spaces, with $+$ as addition, and $++$ as concatenation.

We interpret our sorts as themselves. Our operations we need to interpret as arrows $\sem{+_n} : n + n \rightarrow n$, and $\sem{{++}_{n, m}} : n + m \rightarrow n + m$. For this, we choose the matrices

\begin{center}

\begin{tabular}{c c}

$\sem{+_n} = \left(\begin{array}{c|c} I_n & I_n \end{array}\right)$, &

$\sem{{++}_{n, m}} = I_{n + m}$.

\end{tabular}

\end{center}

Now let us evaluate a term in this category. Consider the context $\Gamma = [x : 1, y : 1, z : 2]$, and the term $t = ((x + y + x) {++} y) + z$. As $\sem{\Gamma} = 1 + 1 + 2 = 4$, and $\Gamma \vdash t : 2$, then $t$ is interpreted as an arrow $4 \rightarrow 2$. That is, a $2 \times 4$ matrix. We will build up to the evaluation of $t$. First, in this context, our variables are evaluated as

\begin{center}

\begin{tabular}{c c c}

$\sem{x} = \begin{psmallmatrix} 1 & 0 & 0 & 0 \end{psmallmatrix}$, &

$\sem{y} = \begin{psmallmatrix} 0 & 1 & 0 & 0 \end{psmallmatrix}$, &

$\sem{z} = \begin{psmallmatrix} 0 & 0 & 1 & 0 \\ 0 & 0 & 0 & 1 \end{psmallmatrix}$.

\end{tabular}

\end{center}

If we work through the definition of operation application with the matrices for $+$ and $++$, we see that $+$ corresponds to adding matrices, while $++$ corresponds to concatenation, as desired. Thus, we can build up the evaluation of $t$ as so:
$$
\sem{x + y + x} = \begin{pmatrix} 2 & 1 & 0 & 0 \end{pmatrix}
$$


$$
\sem{(x + y + x) {++} y} = \begin{pmatrix} 2 & 1 & 0 & 0 \\ 0 & 1 & 0 & 0 \end{pmatrix}
$$


$$
\sem{t} = \sem{((x + y + x) {++} y) + z} = \begin{pmatrix} 2 & 1 & 1 & 0 \\ 0 & 1 & 0 & 1 \end{pmatrix}
$$

## Equivalence of Standard and Categorical Evaluation

It turns out that evaluation in locally-small categories is equivalent to standard evaluation. So we can evaluate terms categorically using our approach, via this equivalence. This follows from Lawvere's account of algebraic theories. In one direction we specialize by choosing the category $\mathrm{Set}$. In the other we use the Yoneda embedding.

First, let us show that categorical evaluation gives us standard evaluation. So suppose we have a universe of values, a function for each operation, of appropriate arity; and the ability to perform categorical evaluation.

Well, let us evaluate categorically in $\mathrm{Set}$. For each sort in our syntax, we need an object in $\mathrm{Set}$; and for each operator, an arrow. But the objects in $\mathrm{Set}$ are precisely sets of values; and the arrows, functions of appropriate signature. This is exactly the information we have been given.

So we evaluate a term categorically with this data. This gives us a function from environments to values. Expanding the definitions, we see that it is precisely the function for standard evaluation of that term in those environments. So categorical evaluation gives us standard evaluation.

Now let us go in the other direction. Suppose we have standard evaluation, and we want to do categorical evaluation in a locally small Cartesian category $C$. So for each sort $a$, we have some object $\sem{a}$, and for each operation $\mathrm{op}$, an arrow $\sem{\mathrm{op}}$.

To use standard evaluation, we need to choose a set for each sort $a$. First, let us fix an object $B$. Then we take our set to be the Yoneda embedding $Y \sem{a} B = C(B, \sem{a})$, the collection of $C$ arrows $B \rightarrow \sem{a}$. This is a set as $C$ is locally small.

We now need to choose an operation for each operator $\mathrm{op}$ of signature $([a_1, \ldots, a_n], b)$. We take the function $(g_1, \ldots, g_n) \mapsto \sem{\mathrm{op}} \circ \langle g_1, \ldots, g_n \rangle$. Note that each $g_i$ is an arrow $B \rightarrow \sem{a_i}$, and $\sem{\mathrm{op}}$ is an arrow $\sem{a_1} \times \cdots \times \sem{a_n} \rightarrow \sem{b}$, so the result is an arrow $B \rightarrow \sem{b}$, as required.

Now take some term $\Gamma \vdash t : a$. Choose $B = \sem{\Gamma}$, and standardly evaluate $t$ with the above data, in the projections environment $[x_1 = \pi_{x_1}, \ldots, x_n = \pi_{x_n}]$. This gives us an arrow $\sem{\Gamma} \rightarrow \sem{a}$. Working through the definitions, it is precisely the categorical evaluation of $t$ in $C$. So standard evaluation gives us categorical evaluation.

# Library Internals

In the previous sections, we talked about our library from the perspective of a user. In this section we will go into the implementation details of the library, and the features of Idris 2 used to make this possible.

For the syntactic sugar, we used Idris 2's literal overloading. For compile-time evaluation we used Idris 2's elaborator reflection. For well-scoped, well-sorted data structures, we used Idris 2's dependent types.

## Literal Overloading

Back in section \ref{term}, we wrote `` `(x * y) ``{.idr type="Term SizedMonoidSyn [<\"x\" :! 1, \"y\" :! 2] 3"} for a term in sized monoid syntax. But `` `(x * y) ``{.idr type="TTImp"} is a quoted expression literal, not some custom data type. So how does Idris 2 know what we intended?

Idris 2 provides a way to overload the interpretation of literals. During elaboration, literals are wrapped in a call with a fixed name. Which name depends on what sort of literal it is. This name is then further elaborated as usual, to some function or macro in scope, in a type-directed way. To overload our literals we need only write a function or macro of the appropriate name.

For example, the quoted expression `` `(x * y) ``{.idr} elaborates to `` fromTTImp `(x * y) ``{.idr}. Our library defines a macro `fromTTImp : TTImp -> Elab (Term syn ctx a)`{.idr .decl namespace="Literal"}, which is then run by the elaborator to produce the actual term value.

Similarly, Idris 2 provides literal overloading for strings, integers, quoted declarations, and quoted names. For example, the string `"Hello, world"`{.idr} elaborates to `fromString "Hello, world"`{.idr}, the integer `1`{.idr} to `fromInteger 1`{.idr type="Integer"}, the quoted declaration `` `[e : 0] ``{.idr type="List Decl"} to `` fromDecls `[e : 0] ``{.idr type="List Decl"}, and the quoted name `` `{x} ``{.idr} to `` fromName `{x} ``{.idr}.

The eagle-eyed reader may have noticed the appearance of `fromInteger`{.idr type="Integer -> Integer"} back in section \ref{elaboration}. This is why it's there.

In this library, we overload quoted expressions and declarations for multiple purposes, as described throughout section \ref{first-class-algebraic-presentations}. While we use quoted names, we do not overload them. We use quoted names as themselves, without modification.

## Elaboration Scripts

We use Idris 2's elaboration scripts for compile-time evaluation. For example, to parse terms in our custom algebras. This parsing has no runtime overhead. Idris 2 elaboration scripts are written as regular Idris 2 functions, which return values in the `Elab`{.idr} monad. The `Elab`{.idr} monad provides access to the Idris 2 elaborator.

We can run elaboration scripts in two ways. The first is to use the `%runElab` directive, which runs the passed elaboration script. The second is to tag the function with the `%macro` directive, which makes the function a macro. Any call to a macro will run its script. The former is more composable, as we can refer to other elaboration scripts without immediately running them, but has additional syntactic overhead to run. The latter is more convenient for users, but is potentially confusing, as running a macro looks like a regular function call, but isn't. Further, macros cannot be run at the top-level, while `%runElab` can.

In our library, we used `%runElab` for `openSyn`{.idr} and `openThy`{.idr}, and `%macro` for overloading literals. Using `%runElab` for `openSyn`{.idr} and `openThy`{.idr} is necessary, as they are run at the top-level. We use `%macro` for overloading literals to avoid additional syntactic overhead. We avoid potential confusion with function calls here, as the literal overloading hides the macro call from the user entirely.

## Underlying Data Structure

For basic use, the user doesn't need to know about the underlying data structure. We have, thus far, mostly managed to avoid mentioning it, with the exception of considering alternate approaches in section \ref{automatic-variable-introduction}. But it does become relevant when the user wants to do universal algebra. We use dependent types to enforce well-scoped, well-sorted terms, the signatures of interpretations, and the laws of models.

First of all, our variables are well-scoped, well-sorted de Bruijn indices. At compile-time, variables are represented in unary, for proofs. But at runtime, Idris 2 optimizes `Nat`{.idr}-like types to the more efficient big integers. So we get the best of both worlds. We don't optimize down to a fixed width integer, as theoretically you could have arbitrarily many variables (though I would be surprised if you managed to use over $2^{64}$).

Our terms are indexed by their syntax, context, and sort. They have two constructors, `Var`{.idr type="Var ctx a -> Term syn ctx a"} and `Operation`{.idr type="(op : Op syn) -> (i : Env op.index Prelude.id) -> Env (anonCtx op.arity i) (Term syn ctx) -> Term syn ctx (op.result i)"}. The former enforces that the context and sort of the term match the context and sort of the variable. The latter enforces that the right number of operands are passed to the operation, in the same context, and the correctness of the sorts.

Similarly, environments are indexed by their context, interpretations by their syntax, and models by their theory. All three are indexed by their universes. For environments, we enforce the right length, and that the types of the values match the corresponding sorts in its universe. For interpretations, we enforce the signatures of the operation functions matches the operation arity, in its universe. For theories, we enforce interpretations satisfy the given axioms.

## Elaborator

Back in section \ref{elaborator-reflection}, we mentioned how we didn't write our own type-checker, instead reusing bits of Idris 2 through the elaborator. So let us now give more detail on what bits we reused, and how. In contrast to the previous sections, this section isn't about what we have done, but what we haven't.

The main "trick" we use is to construct a term in Idris 2 surface syntax, and then pass that term to the Idris 2 type-checker. By using sufficiently strong types in our underlying data types, our constructed term only type-checks when it is well-formed, in whatever sense we are using.

For example, consider the term $x * y$ in sized monoid syntax. We start with the Idris 2 surface syntax term `` `(x * y) ``{.idr}. We lookup the names `x`, `y`, and `*`, and find `x` and `y` are in our context, and `*` is an operation in our syntax. From this we construct the Idris 2 surface syntax term:

```{.idr type="TTImp"}
`(Operation (MkOp Here) [<?, ?] [<
    Var (There Here),
    Var Here
])
```

We build this term syntactically, regardless of whether it's well-sorted or not. We then pass this term to the Idris 2 type-checker. Our use of dependent types in the underlying data structures means that type-checking this constructed Idris 2 term corresponds to checking the original term is well-sorted.

Note the two `?`s in the constructed term, and recall that the operation $*$ is not just one operation, but a family of operations $*_{n, m}$. These `?`s correspond to the indexes for $*$. Our use of `?` tells the Idris 2 elaborator that it should be able to fill those values in itself, by unification. In this way, we use Idris 2's unifier for sort inference of our subterms.

Similarly, we did not write our own variable lookup. Instead of manually searching the context, we can ask the elaborator to use proof search to prove the variable is in the context. If it succeeds, we can use the proof to safely construct our variable representation. If not, we know the variable is not in scope, and can raise a compile-time error.

The elaborator can be used to raise custom compile-time error messages, using the `fail`{.idr type="String -> Elab a"} function. For example, if we ask the elaborator to prove a variable is in context, and it fails, we can either let Idris 2 raise the default "Can't find an implementation" error, or replace it with our own custom error message.

## Composition of Elaboration Scripts

Sometimes it is useful to use previously written elaboration scripts in other elaboration scripts. For example, parsing the axiom `` `[leftId : e * x = x] ``{.idr} requires parsing the term `` `(e * x) ``{.idr}.

But while macros are most convenient for the end user, that they are immediately run when called makes them unsuitable for composition. For this reason, we have two copies of every elaboration script --- one for internal use, and another which wraps the former as a macro. This minor overhead allows for composition internally, while presenting a clean interface to the user.

Another issue which impacts composition of elaboration scripts is metavariables. With one script, we build up a term in the surface syntax of Idris 2, and then `check`{.idr type="TTImp -> Elab a"} it, converting it into an actual object of that type. When composing scripts, we need the first object back in the surface syntax, to construct our larger term.

But if we do this with `quote`{.idr type="(0 _ : a) -> Elab TTImp"}, then any currently unsolved metavariables become holes, as the surface syntax does not have any way of expressing metavariables. Worse, those holes don't get linked back to the original metavariables when re`check`{.idr type="TTImp -> Elab a"}ed. This results in our script generating an incomplete program.

Worse still, this is fragile, as it depends on the internal implementation of the elaborator. In some cases, something as simple as applying the identity function was enough to get the elaborator to solve previously unsolved metavariables. Sometimes, it was not, with no clear reason one way or another. This leaks implementation details of the elaborator.

Instead, what we did was keep the internal copy of the elaboration script returning the surface syntax of Idris 2, and only `check`{.idr type="TTImp -> Elab a"}ing it into an actual object in the user-facing macro. This way we only `check`{.idr type="TTImp -> Elab a"} each term once, and avoid the issue of repeatedly `check`{.idr type="TTImp -> Elab a"}ing and `quote`{.idr type="(0 _ : a) -> Elab TTImp"}ing.

This does lose typing information, as the return type of the internal copy of the elaboration script is `TTImp`{.idr}, the type of Idris 2 surface syntax; instead of the type of the object we're constructing. But that typing information is still captured in the user-facing copy.

# Future Work

## Second-order Syntax

A natural extension of this work is to reinterpret ever larger fragments of Idris 2 syntax. For example, second-order syntax, where we reinterpret binding. Our approach allows using the host language binding syntax, without using the host language binding semantics.

This is in contrast with Higher Order Abstract Syntax (HOAS) [@hoas], which reuses both the host language's binding syntax *and* semantics. Reusing the host binding semantics causes issues, such as "junk" terms [@phoas]. While there has been work on removing junk terms, such as with Parameterized Higher Order Abstract Syntax [@phoas], their non-existence of junk terms relies on parametricity. As Idris 2 doesn't have internalized parametricity, we cannot prove the non-existence of junk terms within Idris 2 itself.

Further, even if the issue of junk terms is resolved, HOAS requires requires the semantics of the host and embedded language's semantics match. This causes issues if the binding semantics of the embedded language you want to write diverge from the host language. For example, if our embedded language uses differentiable functions, while our host language is general purpose. Our approach decouples the semantics of the host and embedded languages, allowing for more diverse embedded binding semantics.

## Models in Categories

In this work we have shown that evaluation in categories is equivalent to standard evaluation. One option to extend this work is to show not just equivalence of interpretation, but also equivalence of models. This would require showing our translation preserves equations.

Another option would be to integrate categorical evaluation more deeply in our library. With this, we could explore which problems are most ergonomically solved with categoric evaluation. Combined with second-order syntax, this links in with the work in Compiling to Categories [@compiling-to-categories]. We will then be able to evaluate the simply typed lambda calculus fragment of Idris 2 syntax in an arbitrary cartesian closed category.

# References {.unnumbered}

::: {#refs}
:::
