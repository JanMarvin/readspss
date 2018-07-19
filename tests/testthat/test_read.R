
# prepare packages #############################################################

library(readspss)
require(foreign)

# foreign test files ###########################################################

# electric

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "electric.sav", package="readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE)

df_f <- read.spss(df, to.data.frame = TRUE, use.value.labels = FALSE,
                  use.missings = FALSE, stringsAsFactors=FALSE)

test_that( "electric", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE) )
})


# testdata

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "testdata.sav", package="readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE,
                 convert.dates = FALSE)

suppressWarnings( # caused by foreign
  df_f <- read.spss(df, to.data.frame = TRUE, use.value.labels = FALSE,
                    use.missings = FALSE, stringsAsFactors=FALSE,
                    trim_values = TRUE, trim.factor.names = TRUE)
)


df_f[["string_500"]] <- paste0(df_f[["string_500"]], df_f[["STRIN0"]])
df_f$STRIN0 <- NULL


# trim_values does not work? so we trim
chars <- sapply(df_f, is.character)
chars <- names(chars)[chars]

for (char in chars) {
  df_f[[char]] <- trimws(df_f[[char]])
}


test_that( "testdata", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})


# pspp testfiles ###############################################################

# v13

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "v13.sav", package="readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE)

res <- data.frame(
  N     = 1:2,
  A255  = c(paste0("a1", paste(rep("A", 253), collapse = "")),
            paste0("a2", paste(rep("X", 253), collapse = ""))),
  A258  = c(paste0("b1", paste(rep("B", 256), collapse = "")),
            paste0("b2", paste(rep("Y", 256), collapse = ""))),
  A2000 = c(paste0("c1", paste(rep("C", 1998), collapse = "")),
            paste0("c2", paste(rep("Z", 1998), collapse = ""))),
  stringsAsFactors = FALSE
)

test_that( "third-test", {
  expect_true(all.equal(df_r, res, check.attributes = FALSE) )
})


# v14

df <- df_r <- df_h <- df_f <- res <- NULL
df <- system.file("extdata", "v14.sav", package="readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE)


res <- data.frame(
  vl255  = c(paste(rep("M", 255), collapse = "")),
  vl256  = c(paste(rep("M", 256), collapse = "")),
  vl1335 = c(paste(rep("M", 1335), collapse = "")),
  vl2000 = c(paste(rep("M", 2000), collapse = "")),
  stringsAsFactors = FALSE
)

test_that( "fourth-test", {
  expect_true( all.equal(res, df_r, check.attributes = FALSE) )
})


# haven testfile ###############################################################

# iris

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "iris.sav", package="readspss")

df_r <- read.sav(df, convert.factors = TRUE, use.missings = FALSE)

data(iris)


test_that( "sixth-test", {
  expect_true (all.equal(df_r, iris, check.attributes = FALSE))
})

# factors ######################################################################

# electric
fl <- system.file("extdata", "electric.sav", package="readspss")

df_r <- read.sav(fl)

df_f <- foreign::read.spss(fl, to.data.frame = TRUE)

test_that("factors", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})

# hotel
fl <- system.file("extdata", "hotel.sav", package="readspss")

df_r <- read.sav(fl)

df_f <- foreign::read.spss(fl, to.data.frame = TRUE)

test_that("factors", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})


# physiology
fl <- system.file("extdata", "physiology.sav", package="readspss")

df_r <- read.sav(fl)

df_f <- foreign::read.spss(fl, to.data.frame = TRUE)

test_that("factors", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})

# repairs
fl <- system.file("extdata", "repairs.sav", package="readspss")

df_r <- read.sav(fl)

df_f <- foreign::read.spss(fl, to.data.frame = TRUE)

test_that("factors", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})


#### zsav test #####

fl <- system.file("extdata", "cars.zsav", package="readspss")

df_r <- read.sav(fl)

test_that("zsav", {
  expect_true(all.equal(df_r, cars, check.attributes = FALSE))
})

#### encryption test #####

flu <- system.file("extdata", "hotel.sav", package="readspss")
fle <- system.file("extdata", "hotel-encrypted.sav", package="readspss")

df_u <- read.sav(flu)
df_e <- read.sav(fle, pass = "pspp")

test_that("encrypted", {
  expect_true(all.equal(df_u, df_e, check.attributes = FALSE))
})
