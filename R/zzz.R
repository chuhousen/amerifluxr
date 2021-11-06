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
  base_url <- "https://ameriflux-data.lbl.gov"
  api_url <- file.path(base_url,"AmeriFlux/SiteSearch.svc")
  var_info_url <-
    "ftp://ftp.fluxdata.org/.ameriflux_downloads/measurement_height/"

  # temporary github link for intermediate data summary,
  # currently point to a temporary public repo
  # need to update when main repository becomes public
  git_base_url <-
    "https://raw.githubusercontent.com/chuhousen/amerifluxr_data_tmp/main"

  # what to return
  url <- switch(
    endpoint,
    "sitemap" = file.path(api_url, "SiteMapData/AmeriFlux"),
    "data" = file.path(api_url, "PublishYears/AmeriFlux"),
    "info" = file.path(base_url, "BADM/Anc/SiteInfo/"),
    "variables" = file.path(api_url, "fpinVarLimits"),
    "var_info" = var_info_url,
    "data_variable" = file.path(
      git_base_url,
      "AMF_AA-Flx_BASE-VARIABLE-AVAILABILITY_LATEST.csv"),
    "bif_group" = file.path(
      git_base_url,
      "AMF_AA-Net_BIF-VARIABLE-GROUP-AVAILABILITY_LATEST.csv"),
    "bif_variable" = file.path(
      git_base_url,
      "AMF_AA-Net_BIF-VARIABLE-AVAILABILITY_LATEST.csv")
  )

  # web service hosted on AmeriFlux website
  return(url)
}
