context("test amerifluxr simple api functions")

test_that("Check simple site list return", {
  skip_on_cran()

  amf_sites_out <- amf_sites()
  expect_is(amf_sites_out, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_sites_out))
  expect_true("DATA_POLICY" %in% colnames(amf_sites_out))
  expect_gt(nrow(amf_sites_out), 0)

})

test_that("Check site id check function", {
  skip_on_cran()

  expect_true(amf_check_site_id("US-CRT"))
  expect_true(!amf_check_site_id("US-CrT"))
  expect_true(!amf_check_site_id("us-crt"))
  expect_true(!amf_check_site_id("USCRT"))

  amf_sites_out <- amf_sites()
  expect_equal(sum(amf_check_site_id(amf_sites_out$SITE_ID)),
               nrow(amf_sites_out))

})

test_that("Check data coverage return", {
  skip_on_cran()

  amf_data_coverage_out <- amf_data_coverage()
  expect_is(amf_data_coverage_out, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_data_coverage_out))
  expect_gt(nrow(amf_data_coverage_out), 0)

})

test_that("Check variable list return", {
  skip_on_cran()

  amf_variables_out <- amf_variables()
  expect_is(amf_variables_out, "data.frame")
  expect_true("Name" %in% colnames(amf_variables_out))
  expect_true("Max" %in% colnames(amf_variables_out))
  expect_true("Min" %in% colnames(amf_variables_out))
  expect_gt(nrow(amf_variables_out), 0)

})

test_that("Check site info return", {
  skip_on_cran()

  amf_site_info_out <- amf_site_info()
  expect_is(amf_site_info_out, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_site_info_out))
  expect_true("DATA_POLICY" %in% colnames(amf_site_info_out))
  expect_true("DATA_START" %in% colnames(amf_site_info_out))
  expect_gt(nrow(amf_site_info_out), 0)

})
