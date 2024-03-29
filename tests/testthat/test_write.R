#### test 1 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

data(cars)

write.sav(cars, filepath = "data/cars.sav")

dd <- read.sav("data/cars.sav")

test_that("integer/numerics", {
    expect_true(all.equal(cars, dd, check.attributes = FALSE))
    })

unlink("data", recursive = TRUE)

#### test 2 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

df <- data.frame(V1 = letters, V2 = 1:26, stringsAsFactors = FALSE)

write.sav(df, filepath = "data/df.sav")

dd <- read.sav("data/df.sav")

test_that("character", {
  expect_true(all.equal(dd, df, check.attributes = FALSE))
})

unlink("data", recursive = TRUE)

#### test 3 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

df <- data.frame(V1 = letters, V2 = 1:26, stringsAsFactors = FALSE)
lab <- paste0("lab", seq_along(df))


write.sav(df, filepath = "data/df.sav", label = lab)


dd <- read.sav("data/df.sav")

test_that("character and letter mix", {
  expect_true(all.equal(attr(dd, "var.label"), lab, check.attributes = FALSE))
})

unlink("data", recursive = TRUE)


#### test 4 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

fl <- system.file("extdata", "hotel.sav", package = "readspss")

dd <- read.sav(fl)
write.sav(dd, "data/hotel.sav")
df <- read.sav("data/hotel.sav")



test_that("factor", {
  expect_true(all.equal(dd, df, check.attributes = FALSE))
})

unlink("data", recursive = TRUE)

#### test 5 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

dd <- mtcars

write.por(dd, "data/mtcars.por", add.rownames = TRUE)
df <- read.por("data/mtcars.por", add.rownames = TRUE)



test_that("por", {
  expect_true(all.equal(dd, df, check.attributes = FALSE))
})

unlink("data", recursive = TRUE)


#### test 6 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

dd <- mtcars
dd$am <- factor(x = dd$am, levels = c(0, 1), labels = c("auto", "man"))

write.por(dd, "data/mtcars1.por", convert.factors = TRUE)
df1 <- read.por("data/mtcars1.por", convert.factors = TRUE)

write.por(dd, "data/mtcars2.por", convert.factors = FALSE)
df2 <- read.por("data/mtcars2.por", convert.factors = TRUE)
df2$AM <- df2$AM - 1 # was not stored as factor, but was a factor previous

test_that("por", {
  expect_true(all.equal(dd, df1, check.attributes = FALSE))
  expect_true(all.equal(mtcars, df2, check.attributes = FALSE))
})

unlink("data", recursive = TRUE)


#### test 7 ####

### locale test disabled. this breaks constantly on either windows and/or linux
# if (dir.exists("data"))
#   unlink("data", recursive = TRUE)
#
# dir.create("data")
#
# lab <- c("ümläuts", "ÜMLÄUTS")
# dd <- data.frame(v1 = c("ä","ö","ü"), v2 = c("Ä","Ö","Ü"),
#                  stringsAsFactors = FALSE)
# attr(dd, "label") <- lab
#
# write.por(dd, "data/umlauts.por", toEncoding = "CP1252")
# df1 <- read.por("data/umlauts.por")
# df2 <- read.por("data/umlauts.por", fromEncoding = "CP1252")
#
#
# test_that("umlauts", {
#   # unsure how to test that it might be true (depending on the os's encoding)
#   # expect_false(isTRUE(all.equal(dd, df1, check.attributes = FALSE)))
#   expect_true(all.equal(dd, df2, check.attributes = FALSE))
#   expect_true(identical(lab, attr(df2, "label")))
# })
#
# unlink("data", recursive = TRUE)

#### test 8 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

# create a more complex test with integers, missings missing integers,
# characters and more iris data
dd <- cbind(iris,
            as.integer(seq_len(nrow(iris))),
            NA,
            as.integer(NA),
            "a",
            iris)
names(dd) <- letters[seq_len(ncol(dd))]
write.sav(dd, "data/iris_unc.sav", compress = FALSE)
write.sav(dd, "data/iris_com.sav", compress = TRUE)
df_unc <- read.sav("data/iris_unc.sav")
df_com <- read.sav("data/iris_com.sav")

# this is a known difference in the import and we can ignore it
df_unc$g <- as.logical(df_unc$g)
df_com$g <- as.logical(df_com$g)

# check filesize of both
fs_unc <- file.info("data/iris_unc.sav")[["size"]]
fs_com <- file.info("data/iris_com.sav")[["size"]]


test_that("compression", {
  expect_true(all.equal(dd, df_unc, check.attributes = FALSE))
  expect_true(all.equal(dd, df_com, check.attributes = FALSE))
  expect_true(fs_com < fs_unc)
})

unlink("data", recursive = TRUE)

#### test 9 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

dd <- data.frame(
  N     = 1:2,
  A25  = c(paste0("a1", paste(rep("A", 22), collapse = ""), "a"),
           paste0("a2", paste(rep("X", 10), collapse = ""))),
  A255  = c(paste0("a1", paste(rep("A", 252), collapse = ""), "a"),
            paste0("a2", paste(rep("X", 10), collapse = ""))),
  # A258  = c(paste0("b1", paste(rep("B", 256), collapse = "")),
  #           paste0("b2", paste(rep("Y", 256), collapse = ""))),
  # A2000 = c(paste0("c1", paste(rep("C", 1998), collapse = "")),
  #           paste0("c2", paste(rep("Z", 1998), collapse = ""))),
  stringsAsFactors = FALSE
)

write.sav(dd, "data/dd_u.sav", compress = FALSE)
write.sav(dd, "data/dd_c.sav", compress = TRUE)

write.por(dd, "data/dd_p.por")

df_u <- read.sav("data/dd_u.sav")
df_c <- read.sav("data/dd_c.sav")
df_p <- read.por("data/dd_p.por")


test_that("strings", {
  expect_true(all.equal(dd, df_u, check.attributes = FALSE))
  expect_true(all.equal(dd, df_c, check.attributes = FALSE))
  expect_true(all.equal(dd, df_p, check.attributes = FALSE))
})

unlink("data", recursive = TRUE)

#### dates ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

dd <- data.frame(dat = Sys.Date())

write.sav(dd, "data/dd.sav", compress = TRUE)
write.por(dd, "data/dd.por")
ds <- read.sav("data/dd.sav")
dp <- read.por("data/dd.por")


test_that("dates", {
  expect_true(all.equal(dd, ds, check.attributes = FALSE))
  expect_true(all.equal(dd, dp, check.attributes = FALSE))
})

unlink("data", recursive = TRUE)



#### zsav ####

if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

dd <- iris

write.sav(dd, "data/dd_unc.sav", compress = FALSE, is_zsav = FALSE)
write.sav(dd, "data/dd_com.sav", compress = TRUE, is_zsav = FALSE)
write.sav(dd, "data/dd_unc.zsav", compress = FALSE, is_zsav = TRUE)
write.sav(dd, "data/dd_com.zsav", compress = TRUE, is_zsav = TRUE)

ds_unc <- read.sav("data/dd_unc.sav")
ds_com <- read.sav("data/dd_com.sav")
dz_unc <- read.sav("data/dd_unc.zsav")
dz_com <- read.sav("data/dd_com.zsav")


test_that("zsav", {
  expect_true(all.equal(dd, ds_unc, check.attributes = FALSE))
  expect_true(all.equal(dd, ds_com, check.attributes = FALSE))
  expect_true(all.equal(dd, dz_unc, check.attributes = FALSE))
  expect_true(all.equal(dd, dz_com, check.attributes = FALSE))
})

unlink("data", recursive = TRUE)
