# Readspss

Experimental R package using Rcpp to parse a sav-file into a data.frame().
It may work. Testing is welcome though the package is still at a experimental
stage and the package can crash. The current package imports numerics and 
strings into a data frame. Long strings are currently not implemented.

Because of the R code wrapped around the Rcpp-Function the package is way slower
than havens read_sav, but it is still a bit faster than foreigns read.spss 
(tested with a minimal example).
In contrast to the other packages readspss ships some additional informations 
like the datalabel, date- and timestamp.

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

Currently readspss does not convert internal NAs to R NAs. Meaning, if a value
label is defined as NA (e.g. 9 is missing), it will be returned as number, not
as <NA>. This explaines some differences. readspss will have more factor levels
than foreign because of this. Missings are parsed form the sav file, but the
implementation is missing. Pull requests are welcome.

Another difference, some characters are returend as factors in foreign. 
Currently I have no clue why. Nevertheless this appears to affect only variables
with identical values (e.g. a variable containing the dataset name "Data" can be
a factor in foreign and a character in readspss). 


```R
dd <- read.sav(file, encoding = "UTF-8", convert.factors = TRUE)

dx <- read.spss(file, to.data.frame = TRUE, use.missings = FALSE)

for (i in names(dd) ){
  cat(i, ": ", all.equal(dd[i], dx[i]), "\n")
}
```

