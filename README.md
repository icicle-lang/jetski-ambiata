jetski
======

> Floats on the sea.

![jetski](img/jetski.jpg)


Overview
--------

`jetski` is a library which allows for runtime compilation and execution of C code.

![takeoff](img/takeoff.jpg)


Example
-------

```haskell
source :: Text
source =
  T.unlines [
      "double mean(double sum, double count) {"
    , "    return sum / count;"
    , "}"
    , ""
    , "double max(double x, double y) {"
    , "    return x > y ? x : y;"
    , "}"
    ]

mean :: Double -> Double -> IO (Either JetskiError Double)
mean sum count =
  runEitherT $ withLibrary [] source $ \library -> do
    mean <- function library "mean" retDouble
    mean [argDouble sum, argDouble count]
```

![barrel-flip](img/barrel-flip.jpg)
