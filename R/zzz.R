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

# Return Ameriflux server endpoints
#
# These functions are not exported and "blind"
# but are accessible through :::

amf_server <- function(endpoint = "sitemap"){

  # base urls
  base_url <- "https://amfcdn.lbl.gov/"
  api_url <- file.path(base_url, "api/v1")
  base_url_old <- "https://ameriflux-data.lbl.gov/"
  api_url_old <- file.path(base_url_old, "AmeriFlux/SiteSearch.svc")
  var_info_url <- "ftp://ftp.fluxdata.org/.ameriflux_downloads/measurement_height/"

  # temporary github link for intermediate data summary, currently point to a temporary public repo
  #  need to update when main repository becomes public
  git_base_url <- "https://raw.githubusercontent.com/chuhousen/amerifluxr/master/data-summary"

  # what to return
  url <- switch(
    endpoint,
    "sitemap" = file.path(api_url, "site_display/AmeriFlux"),
    "data_year" = file.path(api_url, "data_availability/AmeriFlux/"),
    "data_download" = file.path(api_url, "data_download/"),
    "variables" = file.path(api_url_old, "fpinVarLimits"),
    "var_info" = var_info_url,
    "data_variable" = file.path(
      git_base_url,
      "AMF_AA-Flx_BASE_VARIABLE-AVAILABILITY_LATEST.csv"),
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
