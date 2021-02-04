#' Get AmeriFlux site general info
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

afx_sites <- function() {

    # web service returning a full site list with basic site general info
    sgi <- memoise::memoise(
        jsonlite::fromJSON(
          afx_server("sitemap"),
          flatten = TRUE,
          simplifyDataFrame = TRUE
          )
        )

    # web service returning a full site list with
    # most-updated data available years in AmeriFlux BASE dataset
    data <- memoise::memoise(
        jsonlite::fromJSON(
          afx_server("data"),
          flatten = TRUE
          )
        )

    # order the data
    sgi <- sgi[order(sgi$SITE_ID), ]
    data <- data[order(data$SITE_ID), ]

    # find start and end of data series
    sgi$DATA_START <- sapply(
      data$publish_years,
      na.min
      )

    sgi$DATA_END <- sapply(
      data$publish_years,
      na.max
      )

    # convert strings to numeric
    sgi$GRP_LOCATION.LOCATION_LAT <- as.numeric(sgi$GRP_LOCATION.LOCATION_LAT)
    sgi$GRP_LOCATION.LOCATION_LONG <- as.numeric(sgi$GRP_LOCATION.LOCATION_LONG)

    member_info <- apply(sgi, 1, function(x){

        # web service returning complete site general
        # info for a single site
        member <- memoise::memoise(jsonlite::fromJSON(
          paste0(afx_server("info"),
                 x['SITE_ID']), flatten = TRUE))

        # get number of team members
        member_role <-
          lapply(member$values$GRP_TEAM_MEMBER, function(x){

            if(x['TEAM_MEMBER_ROLE'] == 'PI'){

              # return data frame
              return(
                data.frame(
                  TEAM_MEMBER_NAME = x['TEAM_MEMBER_NAME'],
                  TEAM_MEMBER_EMAIL = x['TEAM_MEMBER_EMAIL']
                )
              )
            } else {
              return(NULL)
            }
          })

      # return member role
      return(member_role)

    })

    return(sgi)
}
