test_that("field works", {
  expect_equal(field_dim(field(12, 8, 3)), tibble::tibble(env = factor(1:3),
                                                          n = rep(96L, 3),
                                                          nrow = rep(12L, 3),
                                                          ncol = rep(8L, 3)))
})
