context("test amerifluxr plot data functions")

test_that("check plot data availibility function", {
  skip_on_cran()

  p1 <- amf_plot_datayear(site_set = "US-CRT",
                          nonfilled_only = FALSE)
  expect_is(p1, "plotly")

  p2 <- amf_plot_datayear(var_set = "FCH4",
                          nonfilled_only = FALSE)
  expect_is(p2, "plotly")

  p3 <- amf_plot_datayear(site_set = c("US-CRT", "US-WPT"),
                          nonfilled_only = FALSE)
  expect_is(p3, "plotly")

  p4 <- amf_plot_datayear(var_set = c("FCH4", "WTD"),
                          nonfilled_only = TRUE)
  expect_is(p4, "plotly")

  p5 <- amf_plot_datayear(
    var_set = c("FCH4", "WTD"),
    year_set = c(2015:2018),
    nonfilled_only = TRUE
  )
  expect_is(p5, "plotly")


  ## check error & warning return
  expect_error(amf_plot_datayear())
  expect_error(amf_plot_datayear(site_set = c("us-crt", "US-crt", "USCRT")))
  expect_warning(amf_plot_datayear(site_set = c("us-crt", "US-CRT", "USCRT")))
  expect_error(amf_plot_datayear(
    site_set = "US-CRT",
    data_aval = system.file("extdata",
                            "AMF_US-CRT_BASE_HH_2-5.csv",
                            package = "amerifluxr")
  ))
  expect_error(amf_plot_datayear(var_set = c("fch4", "fc", "le")))
  expect_warning(amf_plot_datayear(var_set = c("fch4", "FC", "le")))
  expect_error(amf_plot_datayear(site_set = "US-CRT",
                                 year_set = c(1950:1960)))
  expect_warning(amf_plot_datayear(site_set = "US-CRT",
                                   year_set = c(2009:2012)))
  # test warning bcf too many site-variables
  expect_warning(amf_plot_datayear(var_set = "TS"))

})

test_that("check plot data summary function", {
  skip_on_cran()

  p1 <- amf_plot_datasummary(site_set = "US-CRT",
                             nonfilled_only = FALSE)
  expect_is(p1, "plotly")

  p2 <- amf_plot_datasummary(var_set = "FCH4",
                             nonfilled_only = FALSE)
  expect_is(p2, "plotly")

  p3 <- amf_plot_datasummary(site_set = c("US-CRT", "US-WPT"),
                             show_cluster = TRUE)
  expect_is(p3, "plotly")

  p4 <- amf_plot_datasummary(var_set = c("FCH4", "WTD"),
                             scale = TRUE)
  expect_is(p4, "plotly")

  ## check error & warning return
  expect_error(amf_plot_datasummary())
  expect_error(amf_plot_datasummary(site_set = c("us-crt", "US-crt", "USCRT")))
  expect_warning(amf_plot_datasummary(site_set = c("us-crt", "US-CRT", "USCRT")))
  expect_error(amf_plot_datasummary(
    site_set = "US-CRT",
    data_sum = system.file("extdata",
                           "AMF_US-CRT_BASE_HH_2-5.csv",
                           package = "amerifluxr")
  ))
  expect_error(amf_plot_datasummary(var_set = c("fch4", "fc", "le")))
  expect_warning(amf_plot_datasummary(var_set = c("fch4", "FC", "le")))

  # test warning bcf too many site-variables
  expect_warning(amf_plot_datasummary(var_set = "TS"))

})
