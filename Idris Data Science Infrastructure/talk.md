---
title: Idris Data Science Infrastructure
subtitle: Because sometimes we have to consider the real world
author: Robert Wright
institute:
  - University of Edinburgh, School of Informatics
  - "Supported by: National Cyber Security Centre (NCSC)"
abstract: |
  An introduction to a number of tools supporting data science work in Idris. We will talk about tools for extracting data from businesses, presenting results to stakeholders, and other tools that were developed in the course of developing these tools.

  This is the surrounding work, we wont directly do any data science.
links:
  - title: Idris Community Discord Recording, December 2021
    url: https://youtu.be/4jDlYJf9_34
  - title: Idris2-Python GitHub
    url: https://github.com/madman-bob/idris2-python
  - title: Idris 2 Jupyter Kernel
    url: https://github.com/madman-bob/idris2-jupyter
  - title: Jupyter Kernel VegaLite plugin
    url: https://github.com/madman-bob/idris2-jupyter-vega
  - title: JSON Schema to Idris types
    url: https://github.com/madman-bob/idris2-json-schema
  - title: XML library
    url: https://github.com/madman-bob/idris2-xml
  - title: LibreOffice file manipulation library
    url: https://github.com/madman-bob/idris2-odf
date-meta: 2021-12-07
header-includes:
  - \addtobeamertemplate{navigation symbols}{}{\usebeamerfont{footline} \usebeamercolor[fg]{footline} \insertframenumber}
---

# Data Science in Idris

<!--

This is a bit of an egocentric talk - I'm going to be talking about what I've done for the last 8 months.

This isn't about how to do data science directly, it's about the infrastructure - the surrounding work so that we can start thinking about data science.

This is about how we talk with the business.



How do we want to do data science in Idris?

It's nice to imagine that we get given our data in a beautifully structured format, but in practise it's rarely that nice.

Here's one possible workflow.

-->

Receive data from business in spreadsheet.

<!--

It is most likely going to be a spreadsheet.

Most businesses use spreadsheets all over the place, and lots of systems have spreadsheet exports.

For convenience, we're going to assume a LibreOffice spreadsheet.

-->

Parse the spreadsheet in Idris.

Do our data analysis in Idris.

Present our results in an interactive and pretty way.

<!-- Jupyter Notebook, with graphs -->

Report our results in a rich text file.

<!-- Again, we'll be using LibreOffice for this -->

# Idris Data Science Infrastructure

<!--

Three main points:

-->

Idris Jupyter kernel

Jupyter kernel VegaLite graph plugin

LibreOffice file reading/writing

# Demo

<!--

Demo Jupyter kernel.

Jupyter sessions more closely resemble REPLs than files, so we need to use the REPL syntax.

We can define functions, and use other REPL features, like `:doc`.

We can import modules, including local modules, if an appropriate `.ipkg` file is defined.

-->

# Jupyter Kernel

<!--

There are multiple ways to make a Jupyter kernel.

We wanted to do something in Idris.

This means that if someone wants to contribute, you only need to know Idris.

Mention that it's a stub - want to make it into a proper client

eg. Semantic highlighting.

We decided to go with making a Python backend, and use an existing Python library, for making Jupyter kernels, from Idris.

-->

Python backend + FFI into a Python Jupyter kernel library.

<!--

We went with creating a Python backend as it would be useful more generally.

A lot of data science stuff is written in Python, and that compatibility is nice.

-->

# Python Backend

<!--

So, we went and made a Python backend.

In the Python backend, we can call Python functions using the FFI.

-->

```idris
%foreign "python: abs"
abs : Int -> Int
```

<!--

We also provided a library of bindings for some of the Python standard library - things like Python's `print` and Python lists.

-->

```idris
import Python

main : IO ()
main = Python.print PythonList.empty
```

<!--

Rather than creating a Python backend from scratch, we -

-->

Based the Python backend on the RefC backend.

<!--

The idea was to speed up development, however -

-->

# RefC

<!-- there were a few problems with RefC -->

- Add support for non-`PrimIO` FFIs, and using closures in FFIs

- Fix a few segfaults (`believe_me`, some `cast`s)

  <!--

  As `believe_me` is used in quite a few FFIs, this caused quite a few segfaults down the line.

  -->

- Support for `[external]` types

- Add some missing `%foreign` function implementations

  <!--

  String support, math support

  `Buffer` support, in entirety

  `Clock` support, in entirety

  `StringIterator` support, in entirety

  Fetching the command line arguments

  -->

  - `fastConcat`/`Pack`/`Unpack`
  - `sin`/`cos`/`tan`/...
  - `Buffer`s, `Clock`s, `StringIterator`
  - `getArgs`

- `Integer`s

- Pattern matching on `Char`s

- Ability to extend RefC, to make a new backend

  (see "Extending RefC")

<!--

This was sufficient for the Jupyter kernel.

I believe that if the RefC tail recursion bug is fixed, then we will be fully self hosting in both C and Python.

We could then host Idris on PyPI, so anyone can install it using pip, making it easy to install anywhere.

-->

# VegaLite Graph Plugin

<!--

This is a plugin for the Jupyter kernel.

-->

Extends the Jupyter kernel to display VegaLite graphs.

VegaLite uses JSON to define its graphs.

<!--

JSON is a bit fuzzy for our purposes.

We're programming in Idris, we want our types to be a little bit more meaningful.

Thankfully, VegaLite has a JSON schema, so we're not just manipulating raw JSON.

But JSON schemas are written in JSON, and we want our code to be in Idris.

So, I wrote a JSON schema parser, which generates Idris types that match that specification.

This is a step towards semantic types for graphs, but there are other restrictions on them that can't be expressed in a JSON schema.

For example, that you should have the same number of data points, as you do labels for those data points.

Show `myBarChart` definition:

- We could probably do with adding smart constructors - to get rid of most of these `Nothing`s.

  Thankfully `default` values for records were added to Idris this morning, so I plan to add that relatively soon.

- Note the dependency in the type signature.

  We're enforcing more at the type level than JSON specifications can.

-->

\scriptsize

```json
{
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "description": "A simple bar chart with embedded data.",
  "data": {
    "values": [
      {"a": "A", "b": 28}, {"a": "B", "b": 55}, {"a": "C", "b": 43},
      {"a": "D", "b": 91}, {"a": "E", "b": 81}, {"a": "F", "b": 53},
      {"a": "G", "b": 19}, {"a": "H", "b": 87}, {"a": "I", "b": 52}
    ]
  },
  "mark": "bar",
  "encoding": {
    "x": {"field": "a", "type": "nominal", "axis": {"labelAngle": 0}},
    "y": {"field": "b", "type": "quantitative"}
  }
}
```

\normalsize

# JSON Schema

<!--

Here's an example of a JSON schema, for a list of films; and the Idris code that we generate from that.

The JSON Schema tool can also change the name of the type generated, I called it `MyFilms` here; and also the name of the module it generates.

-->

\scriptsize

::: columns

:::: column

```json
{
  "type": "array",
  "items": {"$ref": "#/$defs/film"},
  "$defs": {
    "film": {
      "type": "object",
      "required": ["title"],
      "properties": {
        "title": {"type": "string"},
        "year": {"type": "number"}
      }
    }
  }
}
```

::::

:::: column

```idris
public export
record Film where
    constructor MkFilm
    title : String
    year : Maybe Double

public export
MyFilms : Type
MyFilms = List Film
```

::::

:::

\normalsize

# LibreOffice

LibreOffice uses the ODF format.

ODF files are zipped XML under the hood.

<!-- So we went and implemented an XML library -->

# XML

<!--

This is a basic library to parse and manipulate XML types.

It's based on the `Odd` alternating list type, which we're viewing as a free monoidal extension.

So we can use the induced monad similarly to the `List` monad.

Actually, `Odd` induces two monads, but we're not going to go into that here.

-->

\scriptsize

```idris
  let Right (someXMLDoc, _) = parse xmlDocument
          """
          <?xml version="1.0" encoding="UTF-8" standalone="no"?>
          <!DOCTYPE html>
          <!-- Some comment -->
          <p class="article">
          Lorem ipsum, dolor <em>sit</em> amet
          </p>
          """
      | err => printLn err

  pure $ mapContent deEmphasize someXMLDoc
where
  deEmphasize : Element -> Element
  deEmphasize = mapContent $ \content => Snd.do
      elem <- map deEmphasize content
      case show elem.name of
          "em" => elem.content
          _ => pure elem
```

\normalsize

# Reading Spreadsheets

<!--

Explain `readODS`, `findSheet`, and `slice`.

Show `parseSomeData` definition:

- I'm hardcoding in the format and size of the data here - you'll probably want to do something more clever.
- `slice` returns a `Vect` of `Vect`.
- Still some parsing, but less parsing

-->

```idris
Right ods <- readODS fileName
    | Left err => pure Nothing
let Just sheet = findSheet "Sheet1" ods
    | Nothing => pure Nothing
let cells = slice cellRange sheet
```

# Render Rich Text Templates

<!--

The `readODTTemplate` returns a dependant pair of the free variables in the template, and a `ODTTemplate` object, indexed by those variables.

So if our `spam.odt` file doesn't contain exactly the `fromName` and `toName` variables, then the pattern in the first line will fail (at runtime), and we'll instead get the second branch.

While this is a runtime check, it is earlier than most templating systems, which either check the variables at render time, or don't check them at all.

We could move this check to compile-time with type-providers, but Idris doesn't have type-providers yet.

Moving to the middle block. You can use the `render` function to convert an `ODTTemplate` into an `ODT` object, by providing the required substitutions.

If we miss one out, or misspell one of them, then this bit will be caught at compile time.

We could also use the `substitute` and `done` functions - though I've not included them in this example - which substitute the variables one at a time, and then convert a completed `ODTTemplate` into an `ODT` object.

We then use `updateODT` to do an in-place update of our template document, with our substitutions.

The values to be substituted are XML values. So you can substitute rich text, if you happen to know what their representation in XML is.

-->

\scriptsize

```idris
Right (["fromName", "toName"] ** template) <- readODTTemplate "spam.odt"
    | Right (vars ** template) =>
        putStrLn "Unexpected template vars: \{show vars}"
    | Left err => printLn err

let spam = render template [
    "fromName" ::= ["Mr. Jerry Smith"],
    "toName" ::= ["Sir/Madam"]
  ]

Right () <- updateODT "spam.odt" spam
    | Left err => printLn err
```

\normalsize

# Summary

<!--

All the things added as part of this project, in roughly chronological order:

-->

\small

- Significant work on RefC

- Python backend

  <https://github.com/madman-bob/idris2-python>

- Jupyter Kernel

  <https://github.com/madman-bob/idris2-jupyter>

- Jupyter Kernel VegaLite plugin

  <https://github.com/madman-bob/idris2-jupyter-vega>

- JSON Schema to Idris types

  <https://github.com/madman-bob/idris2-json-schema>

- XML library

  <https://github.com/madman-bob/idris2-xml>

- LibreOffice file manipulation library

  <https://github.com/madman-bob/idris2-odf>

\normalsize
