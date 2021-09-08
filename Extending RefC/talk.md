---
title: Extending RefC
subtitle: Making Idris 2 backends while avoiding most of the work
author: Robert Wright
institute:
  - University of Edinburgh, School of Informatics
  - "Supported by: National Cyber Security Centre (NCSC)"
abstract: |
  Writing backends is time-consuming, and most languages have C FFIs, so wouldn't it be nice if we could just use the C backend instead? In this talk I show how to extend RefC to create Idris 2 backends for more languages, by a high-level overview of the Python backend.

  No Python knowledge will be required, we will be staying mostly in Idris code. This talk should be useful both if you want to get involved in the Python backend, or if you want to create another RefC-based backend.
header-includes:
  - \addtobeamertemplate{navigation symbols}{}{\usebeamerfont{footline} \usebeamercolor[fg]{footline} \insertframenumber}
fontsize: 10pt
---

# Idris 2 Backends

<!--

To make sure we're on the same page, I'll start with a couple of definitions.

-->

Idris 2 can compile to many languages.

Each method of compiling Idris 2 to a target language is called a "backend".

~~~ {.dot}
digraph {
  node [shape=record];

  idr [label="Idris 2"];
  scheme [label="Scheme"];
  c [label="C"];
  py [label="Python"];
  js [label="Javascript"];
  jvm [label="JVM"];
  more [label="..."];
  idr -> {scheme, c, js, jvm, more};
  c -> py;
}
~~~

<!--

Each of these arrows represents a backend.

There are actually multiple Scheme and JavaScript backends, but for simplicity I've only drawn one arrow.

I believe there's now a JVM backend.

-->

RefC is the standard C backend for Idris 2.

<!--

I've been working on both the Python backend, and also RefC. So they're a lot more feature complete than they were six months ago.

-->

# Demo

<!--

Let's have a look at what's possible at the moment in the Python backend.

Everything you see will be mentioned in this talk. 

This is the level of integration we can get to, and this is the lazy way to get there.

Mentally replace "Python" with your language.

`ctrl + L` to clear screen

`numpy` is a Python library for numerical analysis

```bash
cd ~/talk
ls
vim Example1.idr
cd ~/idris2-python
./build/exec/idris2-python --build ~/talk/example1.ipkg
./build/exec/idris2-python --build ~/talk/example2.ipkg
cd ~/talk
PYTHONPATH=build/exec/ python -m example1
vim Example2.idr
PYTHONPATH=build/exec/ python -m example2
```

-->

# Talk Structure

- Basic RefC extension

  <!--

  This is the absolute minimum required to have a backend that runs at all.

  This allows you to run programs that are able to run through RefC.

  -->

- Add FFI support

  <!--

  This is adding `[external]` types, and `%foreign` functions for your language.

  Now your backend can do *more* than RefC, as it also supports `%foreign` functions in *your* language, on top of C `%foreign` functions.

  -->

- Add default bindings library

  <!--

  This is a convenience for your users, so they don't have to write `%foreign` functions for your language's standard library.

  In the Python backend, this is `import Python`, the `PythonObject` type, the `Python.print` function, etc.

  -->

# Extending RefC

<!--

I've shown you what's possible with backends.

Nothing I've shown you is specific to RefC - you can do this however you make your backend.

So why would you want to make a backend by extending RefC?

-->

Making new backends is time-consuming.

If our language supports C, why not use RefC, instead of creating it from scratch?

<!--

I'll be going over how we do it in the Python backend, but I expect other languages will be very similar.

-->

# RefC Compilation Pipeline

<!--

Here's a high level overview of how RefC compiles Idris 2 code into an executable.

-->

~~~ {.dot .Grankdir:LR}
digraph {
  node [shape=record];

  idr [label=".idr"];
  c [label=".c"];
  o [label=".o"];
  exe [label="exe"];
  idr -> c -> o -> exe;
}
~~~

<!--

- Idris code is converted to C code (`.c`).
- C code is compiled, with some C header files, to an object file (`.o`).
- Object file is linked, with the Idris 2 support library, to create executable.

But we don't need to worry about the details.

From the `Idris2` repository, file `src/Compiler/RefC/RefC.idr`, function `compileExpr`:

-->

From [`src/Compiler/RefC/RefC.idr`](https://github.com/idris-lang/Idris2/blob/main/src/Compiler/RefC/RefC.idr):

<!-- There are three provocatively named function calls. -->

```idris
generateCSourceFile defs cSourceFile
compileCObjectFile cSourceFile cObjectFile
compileCFile cObjectFile cSharedObjectFile
```

<!--

Indeed the `compileExpr` function itself is quite short. Including function signature, about 20 lines.

It is mostly just a wrapper around these function calls.

-->

# Python Backend Compilation Pipeline

<!--

The Python backend is very similar.

This is what happened under the hood during the demo.

-->

~~~ {.dot .Grankdir:LR}
digraph {
  node [shape=record];

  idr [label=".idr"];
  c [label=".c"];
  o [label=".o"];
  ol [label=".o"];
  so [label=".so"];
  exe [label="exe"];
  py [label=".py"];
  idr -> c;
  c -> o -> exe;
  c -> ol -> so -> py;
}
~~~

<!--

- Idris code is converted to C code (`.c`)
- C code is compiled to an object file (`.o`) (as a library)
- Object file is linked to create shared object file (`.so`)
- Shared object file is imported into Python

With the `idris2-api` installed, we can import the `Compiler.RefC` module. We just use the functions used in RefC for the first three stages.

This bit is documented in the Idris 2 docs - [Extending RefC](https://idris2.readthedocs.io/en/latest/backends/refc.html#extending-refc).

For the last stage, we copy in a template Python module. This template module uses the Python `ctypes` module for importing and using the `.so` file.

We then append any program-specific tweaks. (I'll talk about this later)

From the `idris2-python` repository, file `Idris2Python/Idris2Python.idr`, function `compile`:

-->

From [`Idris2Python/Idris2Python.idr`](https://github.com/madman-bob/idris2-python/blob/main/Idris2Python/Idris2Python.idr):

```idris
let additionalFFILangs = ["python"]

generateCSourceFile {additionalFFILangs} defs cSourceFile
compileCObjectFile {asLibrary = True} cSourceFile cObjectFile
compileCFile {asShared = True} cObjectFile cSharedObjectFile

copyDir "Idris2Python/module_template" modulePath
generatePyInitFile modulePath pyInitPath (pythonFFIs defs)
```

<!--

I expect most languages with C support will have some way of importing `.so` files. So this bit should be almost identical for any RefC extension.

-->

# Simple Backend

<!--

At its most simple, all you need to do is import the `.so` file, and call `main(argc, argv)`.

That's it, you're done!

Here's how you can do it in Python:

-->

```python
import ctypes
import pathlib
import sys

cdll = ctypes.CDLL(pathlib.Path(__file__).parent / "main.so")

cdll.main.argtypes = (
    ctypes.c_int, 
    ctypes.POINTER(ctypes.c_char_p)
)

argc = len(sys.argv)
argv = (ctypes.c_char_p * argc)(*map(str.encode, sys.argv))

cdll.main(argc, argv)
```

<!--

If you don't know Python, don't worry about it. All that's happening is we're:

- Importing `main.so` as `cdll`,
- Telling Python what arguments the `cdll.main` function takes,
- Converting the args passed to Python into a form we can pass on to C,
- Calling `cdll.main`.

So now we've got a backend that can do anything that RefC can do. But there are things that RefC can't do.

- FFI support for your language.
- Bindings for your language's standard library.

Which, funnily enough, is what the rest of the talk will be about.

-->

# FFIs

<!-- Suppose I want to call the Python `abs` built-in function, from within Idris. -->

```idris
%foreign "python: abs"
abs : Int -> Int
```

<!-- I need a way of converting Python functions to Idris functions. -->

~~~ {.dot .Grankdir:LR}
digraph {
  node [shape=record];

  idrArg [label="Idris\n-5"];
  idrRes [label="Idris\n5"];

  subgraph cluster_ffi {
    label = "FFI";

    pyArg [label="Python\n-5"];
    pyRes [label="Python\n5"];

    pyArg -> pyRes [label="Python\n`abs`"];
  }

  idrArg -> pyArg;
  pyRes -> idrRes;
}
~~~

<!--

For any arguments, I'll need a way of converting Idris objects to Python objects.

For any return values, I'll need a way of converting Python objects to Idris objects.

By "Idris objects", I mean the RefC representations of Idris objects.

-->

# External Types

<!--

For primitive types, the conversion's relatively simple.

For more complicated types, the conversion may be more complicated, or even impossible. It's often convenient to have an Idris representation of Python objects.

-->

```idris
data PythonObject : Type where [external]
```

<!--

Types represented as RefC types.

Most likely use `Value_GCPointer`.

Need to manage garbage collection for both Python, and RefC.

From the `idris2-python` repository, file `Idris2Python/module_template/idris2/foreign_python.py`, function `to_idris_obj`:

-->

From [`Idris2Python/module_template/idris2/foreign_python.py`](https://github.com/madman-bob/idris2-python/blob/main/Idris2Python/module_template/idris2/foreign_python.py):

```python
pythonapi.Py_IncRef(py_obj)
return cdll.makeGCPointer(
    py_object(py_obj),
    cdll.makeClosureFromArglist(
        on_collect_idris_obj, 
        cdll.newArglist(2, 2)
    )
)
```

<!--

`Py_IncRef` increments the reference count to the Python object.

In `on_collect_idris_obj`, we decrement the reference count again.

-->

# FFI Callback Arguments

<!--

For functions that take callbacks, we'll need a way of converting Idris functions to Python functions.

-->

```idris
%foreign "python: map"
map : (PythonObject -> PythonObject)
   -> PythonIterable
   -> PythonIterator
```

~~~ {.dot .Grankdir:LR}
digraph {
  node [shape=record];

  idrArg1 [label="Idris\nPythonObject &rarr; PythonObject"];
  idrArg2 [label="Idris\nPythonIterable"];
  idrRes [label="Idris\nPythonIterator"];

  subgraph cluster_ffi {
    label = "FFI";

    pyArg1 [label="Python\nfunction"];
    pyArg [label="", fixedsize="false", width=0, height=0, shape=none];
    pyArg2 [label="Python\nIterable"];
    pyRes [label="Python\nIterator"];

    {pyArg1, pyArg2} -> pyArg [arrowhead=none];
    pyArg -> pyRes [label="Python\n`map`"];
  }

  idrArg1 -> pyArg1;
  idrArg2 -> pyArg2;
  pyRes -> idrRes;
}
~~~

# FFI Targets

<!--

So I've talked about what needs to be done for FFI support, but how do we actually do it?

This is some of the code that was on the Python Backend Pipeline slide.

When generating the C code, we tell it to allow additional FFI languages.

You'll want to add your target language.

If you want to, you can add any other FFI targets your language supports.

-->

Allow additional FFI languages:

```idris
let additionalFFILangs = ["python"]
generateCSourceFile {additionalFFILangs} defs cSourceFile
```

<!--

This will generate a number of function stubs in the C code.

You'll need to backpatch/monkey-patch stubs. In C code, it's generally called "backpatching", while in Python, it's generally called "monkey-patching".

A "stub" is a variable that is defined, but isn't set. It's up to us to make sure it's set before the program is run.

You'll need to generate at least a little bit of code, per `%foreign` function definition.

-->

Backpatch stubs:

<!-- Here's a simple example of backpatching the Python `abs` function -->

```python
idris_abs = CFUNCTYPE(c_int64, c_int64)(abs)
c_void_p.in_dll(cdll, "prim__py_abs").value = (
    cast(idris_abs, c_void_p).value
)
```

# Default Bindings

```idris
import Python

main : IO ()
main = Python.print !PythonList.empty
```

<!--

How does it work behind the scenes?

Well, it's just an Idris library, with FFI bindings for your langauge.

-->

From [`PythonBindings/Python.idr`](https://github.com/madman-bob/idris2-python/blob/main/PythonBindings/Python.idr):

```idris
%foreign "python: print"
prim__py_print : PythonObject -> PrimIO ()

export
print : PythonType io a => a -> io ()
print x = primIO $ prim__py_print !(toPy x)
```

<!--

It's convenient, for your users, to provide FFI bindings for your language's standard library.

Then they don't need to bind it themselves.

-->

Convenient for users.

# Summary

- How `idris2-python` works
- How to extend RefC <!-- to make a fully fledged Idris backend -->
- Convenient to extend RefC

<https://github.com/idris-lang/Idris2> <!-- Where RefC lives -->

<https://github.com/madman-bob/idris2-python>

<https://idris2.readthedocs.io/en/latest/backends/refc.html#extending-refc>
