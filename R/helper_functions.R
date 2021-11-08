
#' Lists Ameriflux sites
#'
#' Lists available site (names) and meta-data
#'
#' @return list of ameriflux sites
#' @export

amf_sites <- memoise::memoise(function(){
  # web service returning a full site list with basic site general info
  df <- jsonlite::fromJSON(
    amf_server("sitemap"),
    flatten = TRUE,
    simplifyDataFrame = TRUE
  )

  # order by site id
  df <- df[order(df$SITE_ID), ]

  # return data
  return(df)
})

<<<<<<< HEAD
=======

#' Lists all site member info
#'
#' This function actually lists ALL site info
#' of which only a fraction is used during processing
#'
#' @param site_id Ameriflux site id
#'
#' @return Ameriflux site ids
#' @export

amf_member_info <- memoise::memoise(function(site_id){
  # grab member info
  df <- jsonlite::fromJSON(
    paste0(amf_server("info"),
           site_id), flatten = TRUE)

  # return data
  return(df)
})


>>>>>>> f59a5c1fd15ebef7bc102e1b234d75d7012b48f5
#' Returns a list of data coverage
#'
#' Ameriflux data coverage statistics
#'
<<<<<<< HEAD
#' @param data_product Ameriflux data product. Only "BASE-BADM" data product is supported in current version,
#' and will be expanded for others in the future.
#' @param data_policy Specify data use policy, either "CCBY4.0" and "LEGACY". See AmeriFlux website
#'  \url{https://ameriflux.lbl.gov/data/data-policy/} for details.
#'
#' @return
=======
#' @return Ameriflux data coverage
>>>>>>> f59a5c1fd15ebef7bc102e1b234d75d7012b48f5
#' @export

amf_data_coverage <- memoise::memoise(function(data_product = "BASE-BADM",
                                               data_policy = "CCBY4.0"){

  # web service returning a full site list with
  # most-updated data available years in AmeriFlux BASE dataset
  df <- jsonlite::fromJSON(
    paste0(amf_server("data_year"), "/", data_product, "/", data_policy),
    flatten = TRUE
  )

  # order by site id
  df <- df[order(df$SITE_ID), ]

  # return data
  return(df)
})

#' Get FP (Flux-Processing) Standard Variable List
#'
#' @description This function obtains the latest AmeriFlux FP (Flux-Processing)
#' standard variable list. FP standard defines the variable names and units used
#' for continuously sampled data within the AmeriFlux. Also see AmeriFlux Data
#' Variables page \url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/}
#' for details.
#'
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
#' \dontrun{
#' # download the list of standard variable names and units
#' FP_ls <- amf_variables()
#'}

amf_variables <- function(){

  # get a list of FP (Flux-Processing) standard variables
  variables <- jsonlite::fromJSON(amf_server("variables"),  flatten=TRUE)

  variables <- rbind.data.frame(variables,
                                c("TIMESTAMP_START", "YYYYMMDDHHMM", NA, NA),
                                c("TIMESTAMP_END", "YYYYMMDDHHMM", NA, NA))

  variables$Min <- as.numeric(as.character(variables$Min))
  variables$Max <- as.numeric(as.character(variables$Max))

  return(variables)
}



