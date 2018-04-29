context("Search efficiently positions")

test_that("Search positions", {
  expect_equal(c("B", "C") %where% LETTERS, c(2, 3))
  expect_equal(c("B", "CPOPO") %where% LETTERS, c(2))
  expect_false(is.null(attr(LETTERS, which = "index", exact = TRUE)))
})
