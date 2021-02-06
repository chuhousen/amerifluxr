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

# Return Ameriflux server endpoints
#
# These functions are not exported and "blind"
# but are accessible through :::

amf_server <- function(endpoint = "sitemap"){

  # base urls
  base_url <- "https://ameriflux-data.lbl.gov"
  api_url <- file.path(base_url,"AmeriFlux/SiteSearch.svc")
  var_info_url1 <- "ftp://ftp.fluxdata.org/.ameriflux_downloads/measurement_height/"
  var_info_url2 <- "https://ftp.fluxdata.org/.ameriflux_downloads/measurement_height/"

  # what to return
  url <- switch(
    endpoint,
    "sitemap" = file.path(api_url, "SiteMapData/AmeriFlux"),
    "data" = file.path(api_url, "PublishYears/AmeriFlux"),
    "info" = file.path(base_url, "BADM/Anc/SiteInfo/"),
    "variables" = file.path(api_url, "fpinVarLimits"),
    "var_info1" = var_info_url1,
    "var_info2" = var_info_url2
  )

  # web service hosted on AmeriFlux website
  return(url)
}
