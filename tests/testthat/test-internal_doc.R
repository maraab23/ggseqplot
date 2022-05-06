
test_that("check if output is character", {
  expect_type(shared_params(), "character")
  expect_type(shared_facet(), "character")
})
