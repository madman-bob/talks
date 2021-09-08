module Example1

import Data.List1

import Python

export
data NumpyArray : List1 Nat -> Type where [external]

export
PrimPythonType (NumpyArray dims) where

%foreign "python: arange, numpy"
prim__np_arange : Int -> PrimIO PythonObject

export
arange : HasIO io => (n : Nat) -> io $ NumpyArray (singleton n)
arange n = map believe_me $ primIO $ prim__np_arange $ cast n

main : IO ()
main = do
    x <- arange 15
    Python.print x
