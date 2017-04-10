context("Reading the data file")
test_that("filename includes requested year", {
    name <- make_filename(2014)
    expect_equal(name, "accident_2014.csv.bz2") 
})