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

afx_server <- function(endpoint){

  # base urls
  base_url <- "https://ameriflux-data.lbl.gov"
  api_url <- file.path(base_url,"AmeriFlux/SiteSearch.svc")

  # what to return
  url <- switch(
    endpoint,
    "sitemap" = file.path(api_url, "SiteMapData/AmeriFlux"),
    "data" = file.path(api_url, "PublishYears/AmeriFlux"),
    "info" = file.path(base_url, "BADM/Anc/SiteInfo/")
  )

  # web service hosted on AmeriFlux website
  return(url)
}
