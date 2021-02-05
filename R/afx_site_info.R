#' Get AmeriFlux site general info
#'
#' This combines the information of various other functions.
#'
#' @param contact_info include contact info (TRUE / FALSE)
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
#'   \item DATA_START - The starting year with published AmeriFlux data (YYYY)
#'   \item DATA_END - The ending year with published AmeriFlux data (YYYY)
#'   \item TEAM_MEMBER_PI1 - Site principal investigator #1 name (First/Given Last/Family)
#'   \item TEAM_MEMBER_PI1_EMAIL - Site principal investigator #1 email (free text)
#'   \item TEAM_MEMBER_PI2 - Site principal investigator #2 name (First/Given Last/Family)
#'   \item TEAM_MEMBER_PI2_EMAIL - Site principal investigator #2 email (free text)
#'   \item TEAM_MEMBER_PI3 - Site principal investigator #3 name (First/Given Last/Family)
#'   \item TEAM_MEMBER_PI3_EMAIL - Site principal investigator #3 email (free text)
#' }
#' @export

afx_site_info <- function(contact_info = FALSE){

    # grab meta-data, data is memoised
    # second calls should be fast (as from memory)
    sites <- afx_sites()
    data <- afx_data_coverage()

    # find start and end of data series
    sites$DATA_START <- sapply(
      data$publish_years,
      na.min
      )

    sites$DATA_END <- sapply(
      data$publish_years,
      na.max
      )

    # convert strings to numeric
    # NOTE: you might want to rename variables
    sites$GRP_LOCATION.LOCATION_LAT <- as.numeric(sites$GRP_LOCATION.LOCATION_LAT)
    sites$GRP_LOCATION.LOCATION_LONG <- as.numeric(sites$GRP_LOCATION.LOCATION_LONG)

    # grab contact info
    # THIS IS SLOOOOW, from this side of the ocean anyway
    # generally to be avoided (made optional)
    if(contact_info){
      member_info <- apply(sites, 1, function(x){

          # web service returning complete site general
          # info for a single site
          member <- afx_member_info(site_id = x['SITE_ID'])

          # PROCESSING TBD
      })
    }

    # return site info
    return(sites)
}
