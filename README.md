# Readspss [![Build Status](https://travis-ci.org/JanMarvin/readspss.svg?branch=master)](https://travis-ci.org/JanMarvin/readspss) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/JanMarvin/readspss?branch=master&svg=true)](https://ci.appveyor.com/project/JanMarvin/readspss)

R package using Rcpp to parse an SPSS file into a data.frame(). Currently 
`read.sav` and `read.por` are the main functions and feature of this package.

It works. Testing is welcome though the package is still at an early stage. Its
read function is extensively tested on approximately 600+ sav-files and a good
dozen of por-files. The code is maturing and is frequently tested. The
read functions imports everything into a data frame. Including long strings
and labels. Various features such as importing of value label or missings are 
tested and are working as intended.

Features of the package is reading of

* sav-files,
* zsav-files,
* encrypted sav-files and
* por-files.

Because of the R code wrapped Rcpp functions the package is pretty fast. The 
R code for factor conversion slows things down a bit, changing the encoding a
bit more.

In comparison to `haven` and `foreign` this package preforms pretty well. It
reads more files than each of its predecessors, some are only readable using
`readspss`and covers a few more cases of missing values.

Focus was not so much on winning every benchmark, but reading all features of
an SPSS file and to be as exactly as possible. So some benchmarks are
won and others are lost. It is entirely up to the task. In addition to the data
itself `readspss` ships additional information provided in the files like the
data label, date and timestamp.

Reading of sav and por files is considered feature complete.

## Installation

master:
```R
devtools::install_git("https://github.com/JanMarvin/readspss.git")
```

## Usage

```R
fls <- system.file("extdata", "electric.sav", package="readspss")
flp <- system.file("extdata", "electric.por", package="readspss")

df_s <- read.sav(fls)
df_p <- read.por(flp)

all.equal(df_s, df_p, check.attributes = FALSE)
```

