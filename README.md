# Readspss [![Build Status](https://travis-ci.org/JanMarvin/readspss.svg?branch=master)](https://travis-ci.org/JanMarvin/readspss) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/JanMarvin/readspss?branch=master&svg=true)](https://ci.appveyor.com/project/JanMarvin/readspss)

Experimental R package using Rcpp to parse a sav-file into a data.frame().
Currently `read.sav` is the main function and feature of this package.

It works. Testing is welcome though the package is still at an early no longer
experimental stage stage. Its read function is extensively tested on 
approximately 200 sav-files. It provides unit tests to make sure, that commits
do not break previouly working things. The read.sav function imports everything
into a data frame. Including long strings and labels.

Because of the R code wrapped Rcpp-Function the package is pretty fast. The 
R code for factor conversion slows things down a bit, changing encoding a bit
more.
In comparisson to `haven::read_sav` and `foreign::read.spss` this package
preforms pretty well. It reads a few more files than each of its predecessors.
Focus was not so much on winning every benchmark, but reading all features of
a sav-file provided and to be as exactly as possible. So we win some benchmarks
and lose others. It is entirely up to the task. In addition readspss ships some
additional informations like the datalabel, date- and timestamp.

Reading of sav-files is feature complete.

## Installation

master:
```R
devtools::install_git("https://github.com/JanMarvin/readspss.git")
```

## Usage

```R
library("readspsss")
# ?read.sav
dat <- read.sav(file)
```

## Test

```R
fl <- system.file("extdata", "electric.sav", package="readspss")

df_r <- read.sav(fl)

df_f <- foreign::read.spss(fl, to.data.frame = TRUE)

all.equal(df_r, df_f, check.attributes = FALSE)
```

