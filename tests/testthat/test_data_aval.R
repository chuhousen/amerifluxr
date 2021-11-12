context("test amerifluxr data availability functions")

test_that("check data availibility function", {
  skip_on_cran()

  amf_data_aval_out <- amf_data_aval()
  expect_is(amf_data_aval_out, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_data_aval_out))
  expect_true("VARIABLE" %in% colnames(amf_data_aval_out))
  expect_true("BASENAME" %in% colnames(amf_data_aval_out))
  expect_gt(nrow(amf_data_aval_out), 0)

  ## check error & warning return
  expect_warning(amf_data_aval(site_set = c("us-crt", "US-CRT", "USCRT")))
  expect_error(amf_data_aval(site_set = c("us-crt", "US-crt", "USCRT")))
})

test_that("check metadata availibility function", {
  skip_on_cran()

  amf_metadata_aval_out <- amf_metadata_aval(group_only = TRUE)
  expect_is(amf_metadata_aval_out, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_metadata_aval_out))
  expect_true("GRP_IGBP" %in% colnames(amf_metadata_aval_out))
  expect_true("GRP_LOCATION" %in% colnames(amf_metadata_aval_out))
  expect_gt(nrow(amf_metadata_aval_out), 0)

  ## check error & warning return
  expect_warning(amf_metadata_aval(site_set = c("us-crt", "US-CRT", "USCRT"),
                                   group_only = TRUE))
  expect_error(amf_metadata_aval(site_set = c("us-crt", "US-crt", "USCRT"),
                                 group_only = TRUE))

  amf_metadata_aval_out2 <- amf_metadata_aval(group_only = FALSE)
  expect_is(amf_metadata_aval_out2, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_metadata_aval_out2))
  expect_true("IGBP" %in% colnames(amf_metadata_aval_out2))
  expect_true("LOCATION_LAT" %in% colnames(amf_metadata_aval_out2))
  expect_gt(nrow(amf_metadata_aval_out2), 0)

  ## check error & warning return
  expect_warning(amf_metadata_aval(site_set = c("us-crt", "US-CRT", "USCRT"),
                                   group_only = FALSE))
  expect_error(amf_metadata_aval(site_set = c("us-crt", "US-crt", "USCRT"),
                                 group_only = FALSE))
})

test_that("check variable info function", {
  skip_on_cran()

  # check if the file downloaded
  amf_var_info_out <- amf_var_info(out_dir = tempdir(),
                                   verbose = FALSE)
  expect_gt(length(
    list.files(tempdir(), pattern = "BASE_MeasurementHeight_+[[:digit:]]+.csv")
  ), 0)

  # check returned var info
  expect_is(amf_var_info_out, "data.frame")
  expect_true("Site_ID" %in% colnames(amf_var_info_out))
  expect_true("Variable" %in% colnames(amf_var_info_out))
  expect_true("Height" %in% colnames(amf_var_info_out))
  expect_gt(nrow(amf_var_info_out), 0)
})
