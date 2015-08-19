jetski
======

> Floats on the sea.

![jetski](img/jetski.jpg)


Overview
--------

`jetski` is a library which allows for runtime compilation and execution of C code.


Example
-------

```haskell
source :: Text
source = T.unlines [ "double mean(double sum, double count) {"
                   , "    return sum / count;"
                   , "}"
                   , ""
                   , "double max(double x, double y) {"
                   , "    return x > y ? x : y;"
                   , "}"

mean :: Double -> Double -> IO Double
mean sum count = do
    program <- compile source
    call "mean" program [argDouble sum, argDouble count] retDouble
```

![barrel-flip](img/barrel-flip.jpg]
