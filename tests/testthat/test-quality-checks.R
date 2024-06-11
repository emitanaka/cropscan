test_that("observational unit", {


  expect_equal(is_observational_unit(agridat::gilmour.serpentine, row, col), TRUE)
})
