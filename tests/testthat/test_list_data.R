context("test amerifluxr data availability functions")

test_that("check data availibility function", {
  skip_on_cran()

  amf_list_data_out <- amf_list_data()
  expect_is(amf_list_data_out, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_list_data_out))
  expect_true("VARIABLE" %in% colnames(amf_list_data_out))
  expect_true("BASENAME" %in% colnames(amf_list_data_out))
  expect_gt(nrow(amf_list_data_out), 0)

  ## check error & warning return
  expect_warning(amf_list_data(site_set = c("us-crt", "US-CRT", "USCRT")))
  expect_error(amf_list_data(site_set = c("us-crt", "US-crt", "USCRT")))

  ## check error & warning return
  expect_warning(amf_list_data(var_set = c("fch4", "FC", "le")))
  expect_error(amf_list_data(var_set = c("fch3", "fc2", "le2")))

})

test_that("check data summary function", {
  skip_on_cran()

  amf_summarize_data_out <- amf_summarize_data()
  expect_is(amf_summarize_data_out, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_summarize_data_out))
  expect_true("VARIABLE" %in% colnames(amf_summarize_data_out))
  expect_true("BASENAME" %in% colnames(amf_summarize_data_out))
  expect_true("GAP_FILLED" %in% colnames(amf_summarize_data_out))
  expect_gt(nrow(amf_summarize_data_out), 0)

  ## check error & warning return
  expect_warning(amf_summarize_data(site_set = c("us-crt", "US-CRT", "USCRT")))
  expect_error(amf_summarize_data(site_set = c("us-crt", "US-crt", "USCRT")))
  expect_error(amf_summarize_data(var_set = c("fch4", "fc", "le")))
  expect_warning(amf_summarize_data(var_set = c("fch4", "FC", "le")))

})

test_that("check metadata availibility function", {
  skip_on_cran()

  amf_list_metadata_out <- amf_list_metadata(group_only = TRUE)
  expect_is(amf_list_metadata_out, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_list_metadata_out))
  expect_true("GRP_IGBP" %in% colnames(amf_list_metadata_out))
  expect_true("GRP_LOCATION" %in% colnames(amf_list_metadata_out))
  expect_gt(nrow(amf_list_metadata_out), 0)

  ## check error & warning return
  expect_warning(amf_list_metadata(
    site_set = c("us-crt", "US-CRT", "USCRT"),
    group_only = TRUE
  ))
  expect_error(amf_list_metadata(
    site_set = c("us-crt", "US-crt", "USCRT"),
    group_only = TRUE
  ))

  amf_list_metadata_out2 <- amf_list_metadata(group_only = FALSE)
  expect_is(amf_list_metadata_out2, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_list_metadata_out2))
  expect_true("IGBP" %in% colnames(amf_list_metadata_out2))
  expect_true("LOCATION_LAT" %in% colnames(amf_list_metadata_out2))
  expect_gt(nrow(amf_list_metadata_out2), 0)

  ## check error & warning return
  expect_warning(amf_list_metadata(
    site_set = c("us-crt", "US-CRT", "USCRT"),
    group_only = FALSE
  ))
  expect_error(amf_list_metadata(
    site_set = c("us-crt", "US-crt", "USCRT"),
    group_only = FALSE
  ))
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

  # check error return
  expect_error(amf_var_info(out_dir = "test_not_working",
                            verbose = FALSE))
})
