# Blind functions called at startup

# na.max and na.min functions to return
# max and min values where not Inf is returned
# but NA
na.max <- function(x) {
  ifelse(any(!is.na(x)), max(x, na.rm = TRUE), NA)
}

na.min <- function(x) {
  ifelse(any(!is.na(x)), min(x, na.rm = TRUE), NA)
}


## used to parse numbers within string
Numextract <- function(string) {
  unlist(regmatches(string, gregexpr(
    "[[:digit:]]+\\.*[[:digit:]]*", string
  )))
}

# Return AmeriFlux server endpoints
#
# These functions are not exported and "blind"
# but are accessible through :::

amf_server <- function(endpoint = "sitemap"){

  # base urls
  base_url <- "https://amfcdn.lbl.gov/"
  api_url <- file.path(base_url, "api/v1")
  var_info_url <- "ftp://ftp.fluxdata.org/.ameriflux_downloads/measurement_height/"

  # github link for intermediate data summary
  git_base_url <- "https://raw.githubusercontent.com/chuhousen/amerifluxr/master/data-summary"

  # what to return
  url <- switch(
    endpoint,
    "sitemap" = file.path(api_url, "site_display/AmeriFlux"),
    "site_ccby4" = file.path(api_url, "site_availability/AmeriFlux/BIF/CCBY4.0"),
    "data_year" = file.path(api_url, "data_availability/AmeriFlux"),
    "data_download" = file.path(api_url, "data_download"),
    "variables" = file.path(api_url, "fp_var?limits=True"),
    "var_info" = var_info_url,
    "data_variable" = file.path(
      git_base_url,
      "AMF_AA-Flx_BASE_VARIABLE-AVAILABILITY_LATEST.csv"),
    "data_summary" = file.path(
      git_base_url,
      "AMF_AA-Flx_BASE_VARIABLE-SUMMARY_LATEST.csv"),
    "bif_group" = file.path(
      git_base_url,
      "AMF_AA-Net_BIF_LEGACY_VARIABLE-GROUP-AVAILABILITY_LATEST.csv"),
    "bif_variable" = file.path(
      git_base_url,
      "AMF_AA-Net_BIF_LEGACY_VARIABLE-AVAILABILITY_LATEST.csv")
  )

  # web service hosted on AmeriFlux website
  return(url)
}
