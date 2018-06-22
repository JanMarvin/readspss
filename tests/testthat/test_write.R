
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
