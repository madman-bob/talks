module Example2

import Data.List1

import Python

import Example1

prod : List1 Nat -> Nat
prod xs = foldr (*) 1 xs

%foreign "python: reshape and (lambda *args: args.__getitem__(0).reshape(*args.__getitem__(1))), numpy"
prim__np_reshape : PythonObject -> PythonList -> PrimIO PythonObject

reshape : HasIO io
       => NumpyArray dims
       -> (newDims : List1 Nat)
       -> prod dims = prod newDims
       => io $ NumpyArray newDims
reshape arr newDims = do
    pyArr <- toPy arr
    pyDims <- toPyList $ map (cast {to=Int}) $ forget newDims
    newArr <- primIO $ prim__np_reshape pyArr pyDims
    pure $ believe_me newArr

main : IO ()
main = do
    x <- arange 15
    y <- reshape x (3 ::: [5])
    Python.print y
