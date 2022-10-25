---
title: Idris Type-Driven Data Science
subtitle: Sorry Python, I'll miss you
author:
  - Robert Wright
  - Ohad Kammar
  - Michel Steuwer
institute:
  - University of Edinburgh, School of Informatics
  - "Supported by: National Cyber Security Centre (NCSC)"
abstract: |
  Traditionally, most data science is done in dynamically typed languages. If you're manipulating a table of data, and make a typo in a column name, or ask for the wrong column, then you don't find out until runtime - after which significant computation may have taken place. So a small mistake could lose a large amount of time.

  With dependant types, we can express the schemas of tables at compile-time. This allows us to check, at compile-time, that our columns exist, and are of the correct types.

  We will go through a simple machine learning example, and show the whole workflow, to demonstrate how dependant types can help the process.
references:
  - type: article-journal
    id: b2t2
    author:
      - family: Lu
        given: Kuang-Chen
      - family: Greenman
        given: Ben 
      - family: Krishnamurthi
        given: Shriram 
    issued:
      - - 2020
        - 11
        - 15
    title: "Types for Tables: A Language Design Benchmark"
    container-title: The Art, Science, and Engineering of Programming
    volume: 6
pandoc-latex-fontsize:
  - classes: [idris]
    size: scriptsize
header-includes:
  - \addtobeamertemplate{navigation symbols}{}{\usebeamerfont{footline} \usebeamercolor[fg]{footline} \insertframenumber}
---

# Type-Driven Data Science

Why should we care about type-systems in data science?

<!--

After all, in industry, data science is mostly done with dynamically typed languages.

Well, they're useful for the same reasons type-systems are useful elsewhere, to help make correct programs.

The problem is that most mainstream typing-systems aren't powerful enough to express the sorts of constraints that are useful in data science.

For example, if you want the column of a table - does the column exist in the table? What type does that column have?

We've got all of the downsides, having to write in the types; and none of the upsides, the compiler helping us write our program.

So of course industry will go for dynamically typed languages.

This talk will mostly be about tables, as they're pretty fundamental to most data-science work.

-->

# Type-Systems for Tables

<!--

What is a table?

-->

Name    Age Favorite Color
------- --- --------------
"Bob"   12  "blue"
"Alice" 17  "green"
"Eve"   13  "red"

<!--

Roughly speaking:

Has some number of rows.

Has some number of named columns, where each column has a type.



These heterogeneous columns cause issues with mainstream typing-systems.

If you fix the number of columns, you could use Generics, with one positional argument for each column. But we want to add columns, remove columns, and perform all sorts of operations which might change the tables schema, so that doesn't really work.

We could use a custom table typing-system. This way we can get a type system that does exactly what we need it to, maybe even with custom error messages to make debugging easier.

But this is a lot of work, and you can end up with a lot of custom typing rules. So it's easy for a subtle error to slip through somewhere.

The approach we took was to use dependant types. This gives type safety, and can build off the correctness of a relatively small core.

On top of that, dependant types allows programmers to encode their own custom constraints. When writing code, there are often a lot of implicit assumptions about the structure, not just of tables, but also the code-base more generally. For the long-term maintenance of your code-base, and the sanity of the programmer who comes after you, it would be better if these assumptions could be not just included in the source code, but also enforced.

-->

# B2T2

<!--

This work is in response to the Brown Benchmark for Table Types (B2T2) paper, from 2020.

-->

The Brown Benchmark for Table Types [@b2t2].

<!--

They describe what they want out of a table typing-system.

They focused on dynamically typed languages, to avoid assuming what the type-system should look like.

In this paper they provide:

-->

- Definition of a Table
- Example Tables
- Table API
- Example Programs

<!--

We're taking it as inspiration, rather than a strict formula to follow.

We've diverged from it in a couple of places, either for convenience, or to take advantage of the compile-time/runtime split; which doesn't exist in dynamically typed languages.

-->

# The `Data.Table` module

<!--

This is what we've done.

-->

A table library, written in pure Idris 2.

Can rely on the correctness of Idris, to prove the correctness of the library.

<!--

At this point, we've implemented all of the B2T2 Example Tables, all of the B2T2 Example Programs, and most of the B2T2 Table API.

-->

We're focusing on the interface, not performance.

<!--

Though we should still be the right order of magnitude.

Later work could use the same interface, but wrapped over a more efficient representation.

-->

# Demo

<!--

Let's look at the library from an end-user's perspective.

- `students`

  - Show what tables look like

  - Explain the type of the table

  - If you're familiar with Idris notation, you may notice we're using `SnocList` notation here.

    It's common practise in industry to start with a simple dataset, and then modify it - by adding new rows on the end, and new columns to the right.

    We're using a `SnocList` style representation, as this representation easily allows these operations.

  - We can refer to fields by either name or index.

  - If we use either a name, or an index, that is not available, then we get a compile-time error.

  - To explain this error message a little - to convert a `String` to a `Field`, Idris uses proof search to find which `Field`.

    It knows the schema and field name to use from context. It gets the schema from the table, and the name from the `String` literal.

    It doesn't know what type the field will have, hence the hole.

    Unfortunately, it cannot find any such `Field` in the `Schema` (as it doesn't exist), which it reports to us by saying that it "can't find an implementation".

    If we were using a custom type-system, we could provide a more descriptive error message here.

    Alternatively, we could use something like Idris 1's error reflection.

  - `Field` overloads both `String` and `Integer` notation, to allow this.

- `DotProduct`

  We can constrain our column types.

  So here we have two columns, of the same numeric type, and we're returning a value of the same type.

- `QuizScoreFilter`

  - `buildColumn` adds a column to a table, changing the schema.

    We do type-level computation (admittedly, not much in this example) to work out the schema of the result.

  - `quizAverage` takes a "gradebook" table, and takes the average of all columns whose names start with "quiz".

    So we define a custom data type, describing the constraint that all columns whose names start with "quiz" are of the desired type.

- `PHacking`

  Skip if short on time.

  We're wanting to compare all of the columns of a table with a particular column, to see whether there's a correlation between eating jellybeans of a particular colour, and getting acne.

  `AllColumns` requires all columns of a `Schema` be of that type.

  This is one way of allowing type-safe iteration over the columns of a table.

  `jellyAnon` has all columns of that type.

  `jellyNamed` does not, but after dropping "name", it does - and the type-system can cope with this.

-->

# Table Schemas

<!--

Now let's look under the hood a little.

Here's our definition of a table:

-->

```idris
data Table : Schema -> Type where
    Lin : Table schema
    (:<) : Table schema -> Record schema -> Table schema
```

<!--

We need to index our `Table` type by its `Schema` to ensure that each row is in the same format, and also so that we can only use valid operations.

`Lin` and `:<` allow sugaring using `SnocList` notation. So `Table`s are essentially lists of `Record`s.



We could also index our `Table`s by the number of rows in the table, but we chose not to. We don't consider the number of rows of a table to be central to its definition. Instead we tack it on later, as a proof type.



The column names are not included in the table at runtime.

For most purposes, we don't actually need the column names at runtime, so this allows us to exclude them.

We can still include the column names at runtime, if we want to.

-->

# Compile-time/Runtime

<!--

B2T2 doesn't distinguish between compile-time and runtime properties of our data.

This makes sense as B2T2 considers only dynamically-typed languages, where everything is runtime.

But in Idris, we've got dependant types, which allow us to check things at compile-time; and we've also got quantitative types, which allow us to erase values at runtime.

We can use dependant types for all of our proofs - that fields exist, their types, etc. - so that they are checked at compile-time.

And we can use quantitative types to erase anything we don't need at runtime, for efficiency purposes.

I've got two examples here. One where we don't need the schema at runtime, and one where we do.

In the one where we don't need the schema at runtime, it is erased, so it wont occur in the final program.

In the one where we do need the schema at runtime, we keep it, and so it's available for the final program to use.

-->

## Schema Erased at Runtime

<!--

Field accessors get compiled down to indexes at runtime.

-->

```idris
column : Field schema name type
      -> Table schema
      -> SnocList type
```

<!--

So, perhaps surprisingly, getting a column from the table doesn't require that column's name appear anywhere in the compiled program.

-->

## Schema Kept at Runtime

<!--

This is the type-signature an implementation of one of B2T2's Example Programs.

In this program, we need the names of the columns, as we're going to be printing them out.

In the first line, by explicitly including `schema` as an argument, we are telling Idris that we want it at runtime.

-->

```idris
pHacking : {schema : Schema}
        -> AllColumns schema Bool
        => {baseCol : String}
        -> Field schema baseCol Bool
        -> Table schema
        -> IO ()
```

# Field Notation Overloading

<!--

Combined with Idris's `String` notation overloading, you can write code that looks like

-->

```idris
column "name" students
```

<!--

And the `String` "name" doesn't occur anywhere in the compiled program.

Furthermore, if you misspell the name, this is a compile-time error.

You don't have to wait for three hours of discarded data-processing to find it.

-->

```idris
column 0 students
```

<!--

`Field`s also have `fromInteger` overrides, so you can also refer to columns by index.

Again, that column is in the `Schema` is checked at compile-time.

-->

# Schema Proof Types

<!--

If we have more complicated constraints, such as the `quizAverage` example earlier, we can create a custom proof type on the schema.

For the `quizAverage` example, we required that all columns starting with "quiz" have the same type:

-->

```idris
data GradebookColumn : String -> Type -> Type -> Type where
    QuizCol : So (isPrefixOf "quiz" name)
           -> GradebookColumn name a a
    NoQuizCol : So (not $ isPrefixOf "quiz" name)
             -> GradebookColumn name type a

data GradebookSchema : (schema : Schema) -> (a : Type) -> Type where
    [search schema]
    Lin : GradebookSchema [<] a
    (:<) : GradebookSchema schema a
        -> GradebookColumn name type a
        -> GradebookSchema (schema :< (name :! type)) a
```

<!--

Note that string operations occur at compile-time.

The `So` type is a unit at runtime, and so can be erased.

So `GradebookColumn` is a `Bool` at runtime, and `GradebookSchema` is a `List Bool` at runtime.

In this example, the column names are not needed at runtime.

-->

# Table Proof Types

<!--

As well as proofs on `Schema`s, we can have proofs on particular `Table`s.

-->

```idris
row : (tbl : Table schema)
   -> HasRows tbl n
   => Fin n
   -> Record schema
```

<!--

In this example, `HasRows` is a proof type that the table has that many rows.

We can use it to get a row by row number, with a compile-time error if we go out of bounds.

We could actually think of `HasRows` as a very simple database index for the `Table`, as it provides information to help lookup `Record`s within the `Table`.

-->

# Database Indexing

<!--

Indeed, as proof types can also contain runtime information, we could write a database index in the same way.

This isn't something we have yet, but is something that we could do.

Say we wanted to write a database index on a column of a table. We would do this by defining a proof type, call it `FieldIndex`, which, at runtime, contains the indexing information required to quickly lookup records in that table.

Then the lookup function would look something like this:

-->

```idris
lookup : (fld : Field schema name type)
      -> (tbl : Table schema)
      -> FieldIndex tbl fld
      => type
      -> Maybe (Record schema)
```

<!--

Similarly, you could write more complicated indices that work with multiple columns, or tables formatted in a certain way.

-->

# Possible Future Directions

- FFIs

  <!--

  This library is written in pure Idris.

  We could write a more efficient version in another language, say C, and then FFI into it from Idris, with the same interface.

  -->

- Database Indexing

  <!--

  We could write a database index as a proof type on the table.

  Alternatively, it would likely be more efficient, to change the internal runtime structure of our table.

  -->

- Schema structure

  <!--

  We allow duplicate column names, and the ordering of columns is important.

  But we didn't need to - this was just what was easiest to implement.

  Instead of making the schema a list of column schemas, we could make it a fresh list.

  This would disallow duplicate column names, and make column ordering unimportant.

  This does make proofs more complicated, as we need to prove that we're not overwriting existing columns.

  

  We also assumed a flat schema structure for this work, but, again, we didn't need to.
  
  We could have a tree, allowing essentially ad-hoc data types.
  
  -->

# Summary

- Dependant types are powerful enough to express table typing-systems

- We can do a lot at compile-time

- Idris `table` library

  <https://github.com/madman-bob/idris2-table>

- Possible future work
