
#' Lists Ameriflux sites
#'
#' Lists available site (names) and meta-data
#'
#' @return
#' @export

afx_sites <- memoise::memoise(function(){
  # web service returning a full site list with basic site general info
  df <- jsonlite::fromJSON(
    afx_server("sitemap"),
    flatten = TRUE,
    simplifyDataFrame = TRUE
  )

  # order by site id
  df <- df[order(df$SITE_ID), ]

  # return data
  return(df)
})


#' Lists all site member info
#'
#' This function actually lists ALL site info
#' of which only a fraction is used during processing
#'
#' @param site_id Ameriflux site id
#'
#' @return
#' @export

afx_member_info <- memoise::memoise(function(site_id){
  # grab member info
  df <- jsonlite::fromJSON(
    paste0(afx_server("info"),
           site_id), flatten = TRUE)

  # return data
  return(df)
})


#' Returns a list of data coverage
#'
#' Ameriflux data coverage statistics
#'
#' @return
#' @export

afx_data_coverage <- memoise::memoise(function(){

  # web service returning a full site list with
  # most-updated data available years in AmeriFlux BASE dataset
  df <- jsonlite::fromJSON(
    afx_server("data"),
    flatten = TRUE
  )

  # order by site id
  df <- df[order(df$SITE_ID), ]

  # return data
  return(df)
})

#' Get FP (Flux-Processing) Standard Variable List
#' @description This function obtains the latest AmeriFlux FP (Flux-Processing) standard variable list.
#'  FP standard defines the variable names and units used for continuously sampled data within the AmeriFlux.
#'  Also see AmeriFlux Data Variables page \url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/} for details.
#' @return A data frame containing the following columns:
#' \itemize{
#'   \item Name - Standard variable name
#'   \item Description - Description of the variable
#'   \item Units - Standard variable unit
#'   \item Min - Expected minimal value
#'   \item Max - Expected maximal value
#'   }
#' @export
#'
#' @examples
#' ## Not run:
#' # download the list of standard variable names and units
#' FP_ls <- afx_variables()
#'
#' ## End(Not run)
afx_variables <- function(){

  # get a list of FP (Flux-Processing) standard variables
  variables <- jsonlite::fromJSON(afx_server("variables"),  flatten=TRUE)

  variables$Min <- as.numeric(as.character(variables$Min))
  variables$Max <- as.numeric(as.character(variables$Max))

  return(variables)
}



