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

![barrel-flip](img/barrel-flip.jpg)


Dependencies
------------

To build this project you'll need `libffi` installed and available to `pkgconfig`.

### OS/X

**Install:**
```bash
brew install libffi
```

**Configure:**
```bash
export BREW_LIBFFI=$(brew --prefix libffi)
export PKG_CONFIG_PATH=$BREW_LIBFFI/lib/pkgconfig
```

### RHEL/CentOS

**Install:**
```bash
yum install libffi-devel
```

![takeoff](img/takeoff.jpg)
