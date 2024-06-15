test_that("multiplication works", {

  dd <- datasearch::datasearch(pkgs = "agridat")
  dd |>
    rowwise() |>
    filter(all(c("year", "row", "col") %in% names)) |>
    pull(data_name)
  expect_equal(2 * 2, 4)
})
