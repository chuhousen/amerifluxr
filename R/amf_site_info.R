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
#'   \item URL_AMERIFLUX - Site web site URL, maintained by AmeriFlux (URL)
#'   \item TOWER_BEGAN - The starting year of flux measurement (YYYY)
#'   \item TOWER_END - The ending year of flux measurement (YYYY), NA if still active or unspecified
#'   \item IGBP - Vegetation type based on the IGBP definition (free text)
#'   \item GRP_LOCATION.LOCATION_LAT - Latitude of the site (decimal deg ref WGS84)
#'   \item GRP_LOCATION.LOCATION_LONG - Longitude of the site (decimal deg ref WGS84)
#'   \item GRP_LOCATION.LOCATION_ELEV - Elevation of the site above sea level (m)
#'   \item GRP_CLIM_AVG.MAT - Long-term mean annual average air temperature (degree C)
#'   \item GRP_CLIM_AVG.MAP - Long-term mean annual average precipitation (mm)
#'   \item GRP_CLIM_AVG.CLIMATE_KOEPPEN - Koppen climate classification (free text)
#'   \item DATA_START_CCBY4 - The starting year with published AmeriFlux BASE data under CCBY4.0 policy (YYYY)
#'   \item DATA_END_CCBY4 - The ending year with published AmeriFlux BASE data under CCBY4.0 policy (YYYY)
#'   \item DATA_START_LEGACY - The starting year with published AmeriFlux BASE data under LEGACY policy (YYYY)
#'   \item DATA_END_LEGACY - The ending year with published AmeriFlux BASE data under LEGACY policy (YYYY)
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
#'
amf_site_info <- function(){

    # grab meta-data, data is memoised
    # second calls should be fast (as from memory)
    sites <- amf_sites()
    data_ccby4 <- amf_data_coverage(data_policy = "CCBY4.0")
    data_legacy <- amf_data_coverage(data_policy = "LEGACY")

    # find start and end of data series
    sites$DATA_START_CCBY4 <- sapply(
      data_ccby4$publish_years,
      na.min
      )

    sites$DATA_END_CCBY4 <- sapply(
      data_ccby4$publish_years,
      na.max
      )

    sites$DATA_START_LEGACY <- sapply(
      data_legacy$publish_years,
      na.min
    )

    sites$DATA_END_LEGACY <- sapply(
      data_legacy$publish_years,
      na.max
    )

    sites$DATA_START <- apply(
      sites[, c("DATA_START_CCBY4", "DATA_START_LEGACY")],
      MARGIN = 1,
      FUN = na.min
    )

    sites$DATA_END <- apply(
      sites[, c("DATA_END_CCBY4", "DATA_END_LEGACY")],
      MARGIN = 1,
      FUN = na.max
    )

    # convert strings to numeric
    # NOTE: you might want to rename variables
    sites$GRP_LOCATION.LOCATION_LAT <-
      as.numeric(sites$GRP_LOCATION.LOCATION_LAT)
    sites$GRP_LOCATION.LOCATION_LONG <-
      as.numeric(sites$GRP_LOCATION.LOCATION_LONG)

    # return site info
    return(sites)
}
