
# prepare packages #############################################################

require(testthat)
library(readspss)
require(foreign)

# foreign test files ###########################################################

# electric

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "electric.sav", package = "readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE)

df_f <- foreign::read.spss(df, to.data.frame = TRUE, use.value.labels = FALSE,
                           use.missings = FALSE, stringsAsFactors = FALSE)

test_that("electric", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})


# testdata

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "testdata.sav", package = "readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE,
                 convert.dates = FALSE)

suppressWarnings(# caused by foreign
  df_f <- foreign::read.spss(df, to.data.frame = TRUE, use.value.labels = FALSE,
                             use.missings = FALSE, stringsAsFactors = FALSE,
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


test_that("testdata", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})


# pspp testfiles ###############################################################

# v13

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "v13.sav", package = "readspss")

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

test_that("third-test", {
  expect_true(all.equal(df_r, res, check.attributes = FALSE))
})


# v14

df <- df_r <- df_h <- df_f <- res <- NULL
df <- system.file("extdata", "v14.sav", package = "readspss")

df_r <- read.sav(df, convert.factors = FALSE, use.missings = FALSE)


res <- data.frame(
  vl255  = c(paste(rep("M", 255), collapse = "")),
  vl256  = c(paste(rep("M", 256), collapse = "")),
  vl1335 = c(paste(rep("M", 1335), collapse = "")),
  vl2000 = c(paste(rep("M", 2000), collapse = "")),
  stringsAsFactors = FALSE
)

test_that("fourth-test", {
  expect_true(all.equal(res, df_r, check.attributes = FALSE))
})


# haven testfile ###############################################################

# iris

df <- df_r <- df_h <- df_f <- NULL
df <- system.file("extdata", "iris.sav", package = "readspss")

df_r <- read.sav(df, convert.factors = TRUE, use.missings = FALSE)

data(iris)


test_that("sixth-test", {
  expect_true(all.equal(df_r, iris, check.attributes = FALSE))
})

# factors ######################################################################

# electric
fl <- system.file("extdata", "electric.sav", package = "readspss")

df_r <- read.sav(fl)

df_f <- foreign::read.spss(fl, to.data.frame = TRUE)

test_that("factors", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})

# hotel
fl <- system.file("extdata", "hotel.sav", package = "readspss")

df_r <- read.sav(fl)

df_f <- foreign::read.spss(fl, to.data.frame = TRUE)

test_that("factors", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})


# physiology
fl <- system.file("extdata", "physiology.sav", package = "readspss")

df_r <- read.sav(fl)

df_f <- foreign::read.spss(fl, to.data.frame = TRUE)

test_that("factors", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})

# repairs
fl <- system.file("extdata", "repairs.sav", package = "readspss")

df_r <- read.sav(fl)

df_f <- foreign::read.spss(fl, to.data.frame = TRUE)

test_that("factors", {
  expect_true(all.equal(df_r, df_f, check.attributes = FALSE))
})


#### zsav test #####

fl <- system.file("extdata", "cars.zsav", package = "readspss")

df_r <- read.sav(fl)

test_that("zsav", {
  expect_true(all.equal(df_r, cars, check.attributes = FALSE))
})

#### encryption test #####

flu <- system.file("extdata", "hotel.sav", package = "readspss")
fle <- system.file("extdata", "hotel-encrypted.sav", package = "readspss")

df_u <- read.sav(flu)
df_e <- read.sav(fle, pass = "pspp")

test_that("encrypted", {
  expect_true(all.equal(df_u, df_e, check.attributes = FALSE))
})

#### por test ####


f_sav <- system.file("extdata", "electric.sav", package = "readspss")
f_por <- system.file("extdata", "electric.por", package = "readspss")

df_sav <- read.sav(f_sav)
df_por <- read.por(f_por)


test_that("por_vs_sav", {
  expect_true(all.equal(df_sav, df_por, check.attributes = FALSE))
})

#### read.spps test ####

f_sav <- system.file("extdata", "electric.sav", package = "readspss")
f_por <- system.file("extdata", "electric.por", package = "readspss")

df_sav <- readspss::read.spss(f_sav)
df_por <- readspss::read.spss(f_por)


test_that("por_vs_sav", {
  expect_true(all.equal(df_sav, df_por, check.attributes = FALSE))
})

#### time date formats ####

f_sav <- system.file("extdata", "datetimes.sav", package = "readspss")

df_sav <- readspss::read.spss(f_sav)

exp <- structure(
  list(
    d1 = structure(15736, class = "Date"), d2 = structure(15736, class = "Date"),
    a1 = structure(15736, class = "Date"), a2 = structure(15736, class = "Date"),
    e1 = structure(15736, class = "Date"), e2 = structure(15736, class = "Date"),
    j1 = structure(15736, class = "Date"), j2 = structure(15736, class = "Date"),
    s1 = structure(15736, class = "Date"), s2 = structure(15736, class = "Date"),
    q1 = structure(15706, class = "Date"), q2 = structure(15706, class = "Date"),
    m1 = structure(15706, class = "Date"), m2 = structure(15706, class = "Date"),
    w1 = structure(15734, class = "Date"), w2 = structure(15734, class = "Date"),
    dt1 = structure(1359594120, tzone = "GMT", class = c("POSIXct", "POSIXt")),
    dt2 = structure(1359594153, tzone = "GMT", class = c("POSIXct", "POSIXt")),
    dt3 = structure(1359594153.72, tzone = "GMT", class = c("POSIXct", "POSIXt")),
    y1 = structure(1359594120, tzone = "GMT", class = c("POSIXct",  "POSIXt")),
    y2 = structure(1359594153, tzone = "GMT", class = c("POSIXct", "POSIXt")),
    y3 = structure(1359594153.72, tzone = "GMT", class = c("POSIXct", "POSIXt")),
    w3 = 5, w4 = 5, m3 = 1, m4 = 1, mt1 = 105276,
    mt2 = 105276.58, t1 = 105240, t2 = 105276, t3 = 105276.58,
    dt4 = 105240, dt5 = 105276, dt6 = 105276.58),
  row.names = c(NA, 1L),
  datalabel = "SPSS DATA FILE GNU pspp 1.4.1 - x86_64-pc-linux-gnu",
  datestamp = "27 May 22", timestamp = "18:27:45", filelabel = "",
  vtype = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
            0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
  disppar = structure(
    c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
      3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
      8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L,
      8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L,
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
    dim = c(34L, 3L)),
  missings = list(),
  haslabel = list(),
  longstring = "",
  longvarname = c("D1=d1", "D2=d2", "A1=a1", "A2=a2", "E1=e1", "E2=e2", "J1=j1",
                  "J2=j2", "S1=s1", "S2=s2", "Q1=q1", "Q2=q2", "M1=m1", "M2=m2",
                  "W1=w1", "W2=w2", "DT1=dt1", "DT2=dt2", "DT3=dt3", "Y1=y1",
                  "Y2=y2", "Y3=y3", "W3=w3", "W4=w4", "M3=m3", "M4=m4",
                  "MT1=mt1", "MT2=mt2", "T1=t1", "T2=t2", "T3=t3", "DT4=dt4",
                  "DT5=dt5", "DT6=dt6"),
  longmissing = list(), longlabel = list(), cflag = 1L, endian = 2L,
  compression = 1L, doc = list(), charcode = 65001L, encoding = "UTF-8",
  encStr = "UTF-8", ownEnc = "UTF-8", doenc = FALSE, autoenc = FALSE,
  swapit = FALSE, totals = "", dataview = "", extraproduct = "",
  class = "data.frame", label = list(),
  varmatrix = structure(
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0,
      0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 9, 11, 8, 10, 8, 10, 5,
      7, 8, 10, 6, 8, 6, 8, 8, 10, 17, 20, 23, 20, 20, 20,
      3, 9, 3, 9, 5, 8, 5, 8, 11, 9, 12, 15, 20, 20, 23, 23,
      38, 38, 24, 24, 39, 39, 29, 29, 28, 28, 30, 30, 22, 22,
      22, 41, 41, 41, 26, 26, 27, 27, 40, 40, 21, 21, 21, 25,
      25, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0,
      0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 9, 11, 8, 10,
      8, 10, 5, 7, 8, 10, 6, 8, 6, 8, 8, 10, 17, 20, 23, 20,
      20, 20, 3, 9, 3, 9, 5, 8, 5, 8, 11, 9, 12, 15, 20, 20,
      23, 23, 38, 38, 24, 24, 39, 39, 29, 29, 28, 28, 30, 30,
      22, 22, 22, 41, 41, 41, 26, 26, 27, 27, 40, 40, 21, 21,
      21, 25, 25, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0),
    dim = c(34L, 11L)),
  var.label = character(0),
  lmissing = list()
)

# fix for non UTF-8 R (oldrel linux on github atm)
attr(df_sav, "autoenc") <- FALSE
attr(df_sav, "doenc") <- FALSE
attr(df_sav, "ownEnc") <- "UTF-8"

test_that("time dates", {
  expect_equal(exp, df_sav)
})
