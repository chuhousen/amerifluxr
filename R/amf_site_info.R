#' Get AmeriFlux site general info
#'
#' This combines the information of various other functions.
#'
#' @description This function obtains the latest AmeriFlux site list and
#' sites' general info through the AmeriFlux web service.
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
#'   \item DATA_START - The starting year with published AmeriFlux BASE data (YYYY)
#'   \item DATA_END - The ending year with published AmeriFlux BASE data (YYYY)
#' }
#' @export
#' @examples
#' \dontrun{
#' # obtain the basic general info for all sites
#' site <- amf_site_info()
#'
#' ## End(Not run)
#' }
amf_site_info <- function(){

    # grab meta-data, data is memoised
    # second calls should be fast (as from memory)
    sites <- amf_sites()

    # get data available years
    # legacy policy can cover all sites with data
    data_legacy <- amf_data_coverage(data_policy = "LEGACY")

    # find start and end of data series
    sites$DATA_START <- sapply(
      data_legacy$publish_years,
      na.min
    )

    sites$DATA_END <- sapply(
      data_legacy$publish_years,
      na.max
    )

    # convert strings to numeric
    sites$LOCATION_LAT <-
      as.numeric(sites$LOCATION_LAT)
    sites$LOCATION_LONG <-
      as.numeric(sites$LOCATION_LONG)
    sites$LOCATION_ELEV <-
      as.numeric(sites$LOCATION_ELEV)
    sites$MAT <-
      as.numeric(sites$MAT)
    sites$MAP <-
      as.numeric(sites$MAP)

    # return site info
    return(sites)
}
