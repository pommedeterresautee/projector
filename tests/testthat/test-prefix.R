context("test word prefix")

test_that("Search positions", {
  expect_equal(object = add_prefix(c("this is a test", "this is another test"), "#"),
               expected = c("#this #is #a #test", 
                            "#this #is #another #test"))
})

