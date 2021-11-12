context("test amerifluxr ancillary functions")

test_that("check Numextract functions", {

  expect_equal(Numextract("_a12a_"), "12")
  expect_equal(Numextract("55_a12a_"), c("55", "12"))
  expect_length(Numextract("_agga_"), 0)
})

test_that("check endpoint function", {

  expect_length(amf_server("sitemap"), 1)
  expect_null(amf_server("test_not_working"))
  expect_is(amf_server(), "character")
})

