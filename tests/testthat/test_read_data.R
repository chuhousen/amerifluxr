context("test amerifluxr read data functions")

test_that("check read bif function", {
  skip_on_cran()

  bif <- amf_read_bif(file = system.file("extdata",
                                         "AMF_AA-Flx_BIF_20201218.xlsx",
                                         package = "amerifluxr"))
  expect_is(bif, "data.frame")
  expect_true("SITE_ID" %in% colnames(bif))
  expect_true("VARIABLE" %in% colnames(bif))
  expect_true("DATAVALUE" %in% colnames(bif))
  expect_gt(nrow(bif), 0)

  ## check error return
  expect_error(amf_read_bif())
})

test_that("check extract badm function", {
  skip_on_cran()

  bif2 <- amf_read_bif(file = system.file("extdata",
                                         "AMF_AA-Flx_BIF_20201218.xlsx",
                                         package = "amerifluxr"))
  bif_out1 <- amf_extract_badm(bif_data = bif2,
                              select_group = "GRP_LOCATION")
  expect_is(bif_out1, "data.frame")
  expect_true("SITE_ID" %in% colnames(bif_out1))
  expect_gt(nrow(bif_out1), 0)

  ## check error  return
  expect_error(amf_extract_badm())
  expect_error(amf_extract_badm(bif_data = bif2))

})

test_that("check read base function", {
  skip_on_cran()

  base <- amf_read_base(
    file = system.file("extdata",
                       "AMF_US-CRT_BASE-BADM_2-5.zip",
                       package = "amerifluxr"),
    unzip = TRUE,
    parse_timestamp = TRUE
  )

  expect_is(base, "data.frame")
  expect_true("TIMESTAMP_START" %in% colnames(base))
  expect_true("TIMESTAMP_END" %in% colnames(base))
  expect_gt(nrow(base), 0)

  ## check error return
  expect_error(amf_read_base())
  expect_error(amf_read_base(file = "test_not_work.csv"))
  expect_error(amf_read_base(file = system.file("extdata",
                                                "AMF_AA-Flx_BIF_20201218.xlsx",
                                                package = "amerifluxr")))

})

test_that("check filter base function", {
  skip_on_cran()

  base2 <- amf_read_base(
    file = system.file("extdata",
                       "AMF_US-CRT_BASE_HH_2-5.csv",
                       package = "amerifluxr"),
    unzip = FALSE,
    parse_timestamp = FALSE
  )
  base_f <- amf_filter_base(data_in = base2)

  expect_is(base_f, "data.frame")
  expect_equal(dim.data.frame(base2), dim.data.frame(base_f))
  expect_equal(sum(colnames(base2) %in% colnames(base_f)), ncol(base_f))
  expect_gt(nrow(base_f), 0)

  ## check error return
  expect_error(amf_filter_base())
  expect_error(amf_filter_base(data_in = base2,
                               limit_ls = "test_not_work.csv"))
  expect_error(amf_filter_base(data_in = base2,
                               basename_decode = "test_not_work.csv"))
  expect_error(amf_filter_base(data_in = base2,
                               loose_filter = 100))
  expect_error(amf_filter_base(data_in = base2,
                               loose_filter = NA))
  expect_error(amf_filter_base(data_in = base2,
                               loose_filter = -1))

})


test_that("Check basename parse function", {

  ## a list of testing names
  test_name <- c("CO2", "CO2_PI_1", "CO2_1_N", "CO2_1_SD", "CO2_1_1_1",
                 "CO2_1_1_A", "CO2_1_1_A_SD", "CO2_1_1_A_N",
                 "CO2_PI_F", "CO2_PI_F_1", "CO2_PI_F_1_N",
                 "CO2_PI_F_1_SD", "CO2_PI_F_1_1_1", "CO2_PI_F_1_1_A",
                 "CO2_PI_F_1_1_A_SD", "CO2_PI_F_1_1_A_N",
                 "FETCH_80", "FETCH_80_PI_1", "FETCH_80_1_N", "FETCH_80_1_SD",
                 "FETCH_80_1_1_1", "FETCH_80_1_1_A", "FETCH_80_1_1_A_SD",
                 "FETCH_80_1_1_A_N", "FETCH_80_PI_F", "FETCH_80_PI_F_1",
                 "FETCH_80_PI_F_1_N", "FETCH_80_PI_F_1_SD",
                 "FETCH_80_PI_F_1_1_1", "FETCH_80_PI_F_1_1_A",
                 "FETCH_80_PI_F_1_1_A_SD", "FETCH_80_PI_F_1_1_A_N",
                 "FC_SSITC_TEST", "FC_SSITC_TEST_PI_1", "FC_SSITC_TEST_1_N",
                 "FC_SSITC_TEST_1_SD", "FC_SSITC_TEST_1_1_1", "FC_SSITC_TEST_1_1_A",
                 "FC_SSITC_TEST_1_1_A_SD", "FC_SSITC_TEST_1_1_A_N",
                 "FC_SSITC_TEST_PI_F", "FC_SSITC_TEST_PI_F_1",
                 "FC_SSITC_TEST_PI_F_1_N", "FC_SSITC_TEST_PI_F_1_SD",
                 "FC_SSITC_TEST_PI_F_1_1_1", "FC_SSITC_TEST_PI_F_1_1_A",
                 "FC_SSITC_TEST_PI_F_1_1_A_SD", "FC_SSITC_TEST_PI_F_1_1_A_N")

  basename_out <- amf_parse_basename(var_name = test_name)

  expect_is(basename_out, "data.frame")
  expect_equal(nrow(basename_out), length(test_name))
  expect_equal(sum(test_name %in% basename_out$variable_name), length(test_name))
  expect_equal(sum(basename_out$basename == "CO2"), 16)
  expect_equal(sum(basename_out$basename == "FETCH_80"), 16)
  expect_equal(sum(basename_out$basename == "FC_SSITC_TEST"), 16)
  expect_equal(sum(basename_out$is_correct_basename), 48)
  expect_equal(sum(basename_out$is_gapfill), 24)
  expect_equal(sum(basename_out$is_fetch), 16)
  expect_equal(sum(basename_out$is_layer_aggregated), 6)
  expect_equal(sum(basename_out$is_layer_SD), 6)
  expect_equal(sum(basename_out$is_layer_number), 6)
  expect_equal(sum(basename_out$is_replicate_aggregated), 6)
  expect_equal(sum(basename_out$is_replicate_SD), 6)
  expect_equal(sum(basename_out$is_replicate_number), 6)
  expect_equal(sum(basename_out$is_quadruplet), 6)
  expect_equal(sum(basename_out$is_PI_provide), 3)

  ## check error return
  expect_error(amf_parse_basename())
  expect_error(amf_parse_basename(var_name = test_name,
                                  FP_ls = "test_not_working"))

})
