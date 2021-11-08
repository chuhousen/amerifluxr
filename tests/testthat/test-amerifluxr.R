context("test amerifluxr API functionality")

# Simple example
test_that("basic check functions", {
  skip_on_cran()

  vars <- amf_variables()

  # test for correctly returned values
  expect_type(vars, "list")
})
