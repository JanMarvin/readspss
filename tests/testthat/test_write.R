
dir.create("data")

data(cars)

write.sav(cars, filepath = "data/cars.sav")

dd <- read.sav("data/,cars.sav")

test_that("write test" {
    expext_true(all.equal(cars, dd, check.attributes = FALSE)
    })
    
unlink("data", recursive = TRUE)
