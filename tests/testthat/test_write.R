#### test 1 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

data(cars)

# can not really write anything else
write.sav(cars, filepath = "data/cars.sav")

dd <- read.sav("data/cars.sav")

test_that("write test", {
    expect_true(all.equal(cars, dd, check.attributes = FALSE))
    })

unlink("data", recursive = TRUE)

#### test 2 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

df <- data.frame(V1 = letters, V2 = 1:26, stringsAsFactors = FALSE)
# can not really write anything else
write.sav(df, filepath = "data/df.sav")

dd <- read.sav("data/df.sav")

test_that("write test2", {
  expect_true(all.equal(dd, df, check.attributes = FALSE))
})

unlink("data", recursive = TRUE)

#### test 3 ####
if (dir.exists("data"))
  unlink("data", recursive = TRUE)

dir.create("data")

df <- data.frame(V1 = letters, V2 = 1:26, stringsAsFactors = FALSE)
lab <- paste0("lab", seq_along(df))

# can not really write anything else
write.sav(df, filepath = "data/df.sav", label = lab)


dd <- read.sav("data/df.sav")

test_that("write test2", {
  expect_true(all.equal(attr(dd, "var.label"), lab, check.attributes = FALSE))
})

unlink("data", recursive = TRUE)
