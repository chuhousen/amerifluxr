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
})

test_that("check plot data availibility function", {
  skip_on_cran()

  amf_plot_datayear(site_set = "US-CRT",
                    nonfilled_only = FALSE,
                    save_plot = TRUE,
                    filename_prefix = "TEST1_")
  expect_gt(length(
    list.files(tempdir(), pattern = "TEST1_var_year_available.html")), 0)

  amf_plot_datayear(var_set = "FCH4",
                    nonfilled_only = FALSE,
                    save_plot = TRUE,
                    filename_prefix = "TEST2_")
  expect_gt(length(
    list.files(tempdir(), pattern = "TEST2_var_year_available.html")), 0)

  amf_plot_datayear(var_set = "FCH4",
                    nonfilled_only = FALSE,
                    save_plot = FALSE,
                    filename_prefix = "TEST3_")
  expect_equal(length(
    list.files(tempdir(), pattern = "TEST3_var_year_available.html")), 0)

  ## check error & warning return
  expect_error(amf_plot_datayear())
  expect_error(amf_plot_datayear(site_set = "US-CRT",
                                 out_dir = "test_not_working",
                                 save_plot = TRUE))
  expect_error(amf_plot_datayear(site_set = c("us-crt", "US-crt", "USCRT")))
  expect_warning(amf_plot_datayear(site_set = c("us-crt", "US-CRT", "USCRT")))
  expect_error(amf_plot_datayear(site_set = "US-CRT",
                                 data_aval = system.file("extdata",
                                                         "AMF_US-CRT_BASE_HH_2-5.csv",
                                                         package = "amerifluxr")))
  expect_error(amf_plot_datayear(var_set = c("fch4", "fc", "le")))
  expect_warning(amf_plot_datayear(var_set = c("fch4", "FC", "le")))
  expect_error(amf_plot_datayear(site_set = "US-CRT",
                                 year_set = c(1950:1960)))
  expect_warning(amf_plot_datayear(site_set = "US-CRT",
                                   year_set = c(2009:2012)))
  # test warning bcf too many site-variables
  expect_warning(amf_plot_datayear(var_set = "TS"))

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
  expect_warning(amf_list_metadata(site_set = c("us-crt", "US-CRT", "USCRT"),
                                   group_only = TRUE))
  expect_error(amf_list_metadata(site_set = c("us-crt", "US-crt", "USCRT"),
                                 group_only = TRUE))

  amf_list_metadata_out2 <- amf_list_metadata(group_only = FALSE)
  expect_is(amf_list_metadata_out2, "data.frame")
  expect_true("SITE_ID" %in% colnames(amf_list_metadata_out2))
  expect_true("IGBP" %in% colnames(amf_list_metadata_out2))
  expect_true("LOCATION_LAT" %in% colnames(amf_list_metadata_out2))
  expect_gt(nrow(amf_list_metadata_out2), 0)

  ## check error & warning return
  expect_warning(amf_list_metadata(site_set = c("us-crt", "US-CRT", "USCRT"),
                                   group_only = FALSE))
  expect_error(amf_list_metadata(site_set = c("us-crt", "US-crt", "USCRT"),
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

  # check error return
  expect_error(amf_var_info(out_dir = "test_not_working",
                            verbose = FALSE))
})
