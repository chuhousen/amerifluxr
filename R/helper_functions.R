
#' Lists AmeriFlux sites
#'
#' Lists available site (names) and basic meta-data
#'
#' @return A data frame containing the following columns.
#' See AmeriFlux BADM standard \url{https://ameriflux.lbl.gov/data/badm/}
#' for detailed explanation.
#' \itemize{
#'   \item SITE_ID - Six character site identifier (CC-Sss)
#'   \item SITE_NAME - Site name (free text)
#'   \item COUNTRY - Country (free text)
#'   \item STATE - State (free text)
#'   \item IGBP - Vegetation type based on the IGBP definition (character)
#'   \item URL_AMERIFLUX - Site web site URL, maintained by AmeriFlux (URL)
#'   \item TOWER_BEGAN - The starting year of flux measurement (YYYY)
#'   \item TOWER_END - The ending year of flux measurement (YYYY), NA if still active or unspecified
#'   \item LOCATION_LAT - Latitude of the site (decimal deg ref WGS84)
#'   \item LOCATION_LONG - Longitude of the site (decimal deg ref WGS84)
#'   \item LOCATION_ELEV - Elevation of the site above sea level (m)
#'   \item CLIMATE_KOEPPEN - Koppen climate classification (character)
#'   \item MAT - Long-term mean annual average air temperature (degree C)
#'   \item MAP - Long-term mean annual average precipitation (mm)
#'   \item DATA_POLICY - LEGACY / CCBY4.0 (character)
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # download a list of sites and basic info
#' sites <- amf_sites()
#'
#'}
#'
amf_sites <- memoise::memoise(function(){
  # web service returning a full site list with basic site general info
  df <- jsonlite::fromJSON(
    amf_server("sitemap"),
    flatten = TRUE,
    simplifyDataFrame = TRUE
  )

  # merge info of data policy
  site.ccby4 <- jsonlite::fromJSON(
    amf_server("site_ccby4"),
    flatten = TRUE,
    simplifyDataFrame = TRUE
  )[, 1]

  df$DATA_POLICY <- "LEGACY"
  df$DATA_POLICY[which(df$SITE_ID %in% site.ccby4)] <- "CCBY4.0"

  # order by site id
  df <- df[order(df$SITE_ID),]

  # rename columns
  colnames(df)[which(names(df) == "GRP_LOCATION.LOCATION_LAT")] <-
    "LOCATION_LAT"
  colnames(df)[which(names(df) == "GRP_LOCATION.LOCATION_LONG")] <-
    "LOCATION_LONG"
  colnames(df)[which(names(df) == "GRP_LOCATION.LOCATION_ELEV")] <-
    "LOCATION_ELEV"
  colnames(df)[which(names(df) == "GRP_CLIM_AVG.MAT")] <- "MAT"
  colnames(df)[which(names(df) == "GRP_CLIM_AVG.MAP")] <- "MAP"
  colnames(df)[which(names(df) == "GRP_CLIM_AVG.CLIMATE_KOEPPEN")] <-
    "CLIMATE_KOEPPEN"

  # return data
  return(df)
})


#' Check valid AmeriFlux site ID
#'
#' Check if the character is a valid AmeriFlux site ID (CC-Sss)
#'
#' @param x A vector or scalar of characters
#'
#' @return logical vector or scalar
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if valid site ID
#' check_id <- amf_check_site_id(c("US-CRT", "US-crt", "USCRT"))
#'
#'}
#'
amf_check_site_id <- memoise::memoise(function(x){
  # web service returning a full site list with basic site general info
  df <- jsonlite::fromJSON(
    amf_server("sitemap"),
    flatten = TRUE,
    simplifyDataFrame = TRUE
  )[, "SITE_ID"]

  chk_id <- x %in% df

  # return data
  return(chk_id)
})

#' Returns a list of data coverage
#'
#' AmeriFlux data coverage statistics
#'
#' @param data_product A scalar of character specifying the data product
#'  Currently, only "BASE-BADM" is supported.
#' @param data_policy A scalar of character specifying the data policy
#'  Currently, "CCBY4.0" and "LEGACY" are supported. The default is "CCBY4.0".
#'
#' @return AmeriFlux data coverage
#' \itemize{
#'   \item SITE_ID - Six character site identifier (CC-Sss)
#'   \item URL -  Site page link (url)
#'   \item publish_years - List of data available years (YYYY)
#'  }
#' @export
#'
#' @examples
#' \dontrun{
#' # download the variable availability
#' data_year <- amf_data_coverage()
#'
#' # download variable availability for LEGACY policy
#' data_year <- amf_data_coverage(data_policy = "LEGACY")
#' }
#'
amf_data_coverage <- memoise::memoise(
  function(
    data_product = "BASE-BADM",
    data_policy = "CCBY4.0"
    ){

  # web service returning a full site list with
  # most-updated data available years in AmeriFlux BASE data set
    df <-
      jsonlite::fromJSON(paste0(amf_server("data_year"), "/", data_product, "/", data_policy),
                         flatten = TRUE)

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

  variables <- variables[order(variables$Type), ]
  variables <-
    variables[c(which(variables$Type == "TIMEKEEPING"),
                which(variables$Type != "TIMEKEEPING")),
              c("Name", "Description", "Units", "Min", "Max")]

  variables$Min <- as.numeric(as.character(variables$Min))
  variables$Max <- as.numeric(as.character(variables$Max))

  return(variables)
}
