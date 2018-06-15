# Readspss [![Build Status](https://travis-ci.org/JanMarvin/readspss.svg?branch=master)](https://travis-ci.org/JanMarvin/readspss) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/JanMarvin/readspss?branch=master&svg=true)](https://ci.appveyor.com/project/JanMarvin/readspss)

Experimental R package using Rcpp to parse a sav-file into a data.frame().
It works. Testing is welcome though the package is still at a experimental
stage and the package can crash. The current package imports everything into
a data frame. Including long strings and labels.

Because of the R code wrapped Rcpp-Function the package is pretty fast. Though
since SPSS files can not be trustet (!) some tasks need to be handled in while()
loops and vectors need to be increased on the fly (generally not the best idea).
The R code for factor conversion slows things down a bit, changing encoding even
more.
In comparisson to `haven:read_sav` and `foreign:read.spss` this package reads a
few more files and reads them correctly, but it is still in early stages and 
only(tested with minimal examples).
In addition readspss ships some additional informations like the datalabel,
date- and timestamp.

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

Another difference, some characters are returend as factors in foreign. 
Currently I have no clue why. Nevertheless this appears to affect only variables
with identical values (e.g. a variable containing the dataset name "Data" can be
a factor in foreign and a character in readspss).
As of today, I have no clue what this is supposed to mean, but I leave this here
as a reminder.


```R
dd <- read.sav(file, convert.factors = TRUE, use.missings = FALSE)

dx <- read.spss(file, to.data.frame = TRUE, use.missings = FALSE)

for (i in names(dd) ){
  cat(i, ": ", all.equal(dd[i], dx[i]), "\n")
}
```

