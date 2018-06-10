
# prepare functions #######################################################

# remove additional information from data.frame
strip <- function(df)data.frame(as.matrix(df), stringsAsFactors = F)

# function for direct data comparison
datacompare <- function(x, y) {

  x <- strip(x)
  y <- strip(y)

  res <- unlist(Map(all.equal, c(x), c(y)))

  # with all(unlist(res)) if not TRUE, a warning is thrown
  res <- all(unlist(lapply(res, isTRUE)))

  res
}


# prepare packages ########################################################

library(readspss)
require(foreign)
require(haven)


# foreign test files ######################################################

# electric

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "electric.sav", package="readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE)

df_f <- read.spss(df, to.data.frame = TRUE, use.value.labels = FALSE,
                  use.missings = FALSE, stringsAsFactors=FALSE)

test_that( "first-test", {
  expect_true (datacompare(df_r, df_f) )
  })


# testdata

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "testdata.sav", package="readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE)

suppressWarnings( # caused by foreign
df_f <- read.spss(df, to.data.frame = TRUE, use.value.labels = FALSE,
                  use.missings = FALSE, stringsAsFactors=FALSE,
                  trim_values = TRUE, trim.factor.names = TRUE)
)

# trim_values does not work? so we trim
chars <- sapply(df_f, is.character)
chars <- names(chars)[chars]

for (char in chars) {
  df_f[[char]] <- trimws(df_f[[char]])
}


test_that( "second-test", {
  expect_true(datacompare(df_r, df_f))
})


# pspp testfiles ##########################################################

# v13

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "v13.sav", package="readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE)

# # foreign cannot read this file
# df_f <- read.spss(df, to.data.frame = TRUE, use.value.labels = FALSE,
#                   use.missings = FALSE, stringsAsFactors=FALSE)

# haven does not return the full string
df_h <- read_sav(df) %>% as.data.frame

test_that( "third-test", {
  expect_false (datacompare(df_r, df_h) )
})


# v14

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "v14.sav", package="readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE)

# # foreign does handle long strings as different variables
# df_f <- read.spss(df, to.data.frame = TRUE, use.value.labels = FALSE,
#                   use.missings = FALSE, stringsAsFactors=FALSE,
#                   trim_values = TRUE)

df_h <- read_sav(df) %>% as.data.frame


test_that( "fourth-test", {
  expect_false (datacompare(df_r, df_h) )
})


# haven testfile ##########################################################

# iris

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "iris.sav", package="readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE)

# # foreign cannot read files with k = 0
# df_f <- read.spss(df, to.data.frame = TRUE, use.value.labels = FALSE,
#                   use.missings = FALSE, stringsAsFactors=FALSE,
#                   trim_values = TRUE)

df_h <- read_sav(df) %>% as.data.frame


test_that( "sixth-test", {
  expect_true (datacompare(df_r, df_h))
})

