#' Get AmeriFlux site general info
#' @description This function obtains the latest AmeriFlux site list and sites' general info through the AmeriFlux web service.
#' @return A data frame containing the following columns. See AmeriFlux BADM standard \url{https://ameriflux.lbl.gov/data/badm/} for detailed explanation.
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
#' @examples
#' ## Not run:
#' # download the site list with general info
#' sgi <- get_ameriflux_sgi()
#'
#' ## End(Not run)
get_ameriflux_sgi <- function() {

    # web service hosted on AmeriFlux website
    ameriflux.ws.sitemap <- "https://ameriflux-data.lbl.gov/AmeriFlux/SiteSearch.svc/SiteMapData/AmeriFlux"
    ameriflux.ws.datayear <- "https://ameriflux-data.lbl.gov/AmeriFlux/SiteSearch.svc/PublishYears/AmeriFlux"
    ameriflux.ws.siteinfo <- "https://ameriflux-data.lbl.gov/BADM/Anc/SiteInfo/"

    na.max <- function(x) {
        ifelse(sum(!is.na(x)) > 0, max(x, na.rm = TRUE), NA)
    }
    na.min <- function(x) {
        ifelse(sum(!is.na(x)) > 0, min(x, na.rm = TRUE), NA)
    }

    ## web service returning a full site list with basic site general info
    sgi.ameriflux <- jsonlite::fromJSON(httr::content(httr::POST(ameriflux.ws.sitemap), as = "text"), flatten = TRUE)

    ## web service returning a full site list with most-updated data available years in AmeriFlux BASE dataset
    data.yr.ameriflux <- jsonlite::fromJSON(httr::content(httr::POST(ameriflux.ws.datayear), as = "text"), flatten = TRUE)

    sgi.ameriflux <- sgi.ameriflux[order(sgi.ameriflux$SITE_ID), ]
    data.yr.ameriflux <- data.yr.ameriflux[order(data.yr.ameriflux$SITE_ID), ]

    sgi.ameriflux$DATA_START <- sapply(data.yr.ameriflux$publish_years, na.min)
    sgi.ameriflux$DATA_END <- sapply(data.yr.ameriflux$publish_years, na.max)

    sgi.ameriflux$GRP_LOCATION.LOCATION_LAT <- as.numeric(sgi.ameriflux$GRP_LOCATION.LOCATION_LAT)
    sgi.ameriflux$GRP_LOCATION.LOCATION_LONG <- as.numeric(sgi.ameriflux$GRP_LOCATION.LOCATION_LONG)

    sgi.ameriflux$TEAM_MEMBER_PI1 <- NA
    sgi.ameriflux$TEAM_MEMBER_PI1_EMAIL <- NA
    sgi.ameriflux$TEAM_MEMBER_PI2 <- NA
    sgi.ameriflux$TEAM_MEMBER_PI2_EMAIL <- NA
    sgi.ameriflux$TEAM_MEMBER_PI3 <- NA
    sgi.ameriflux$TEAM_MEMBER_PI3_EMAIL <- NA

    for (j1 in seq_len(nrow(sgi.ameriflux))) {

        target.site <- sgi.ameriflux$SITE_ID[j1]

        ## web service returning complete site general info for a single site
        sgi.site.member <- jsonlite::fromJSON(httr::content(httr::GET(paste(ameriflux.ws.siteinfo,
                                                                            target.site, sep = "")), as = "text"), flatten = T)[[2]][2][[1]]

        get.pi <- NULL
        for (j2 in seq_len(length(sgi.site.member))) {
            if (length(which(names(sgi.site.member[[j2]]) == "TEAM_MEMBER_ROLE")) > 0) {
                if (sgi.site.member[[j2]]$TEAM_MEMBER_ROLE == "PI")
                  get.pi <- c(get.pi, j2)
            }
        }

        if (!is.null(get.pi)){
          sgi.ameriflux$TEAM_MEMBER_PI1[j1] <- sgi.site.member[[get.pi[1]]]$TEAM_MEMBER_NAME
          sgi.ameriflux$TEAM_MEMBER_PI1_EMAIL[j1] <- sgi.site.member[[get.pi[1]]]$TEAM_MEMBER_EMAIL
        }

        if (length(get.pi) > 1){
          sgi.ameriflux$TEAM_MEMBER_PI2[j1] <- sgi.site.member[[get.pi[2]]]$TEAM_MEMBER_NAME
          sgi.ameriflux$TEAM_MEMBER_PI2_EMAIL[j1] <- sgi.site.member[[get.pi[2]]]$TEAM_MEMBER_EMAIL
        }

        if (length(get.pi) > 2){
          sgi.ameriflux$TEAM_MEMBER_PI3[j1] <- sgi.site.member[[get.pi[3]]]$TEAM_MEMBER_NAME
          sgi.ameriflux$TEAM_MEMBER_PI3_EMAIL[j1] <- sgi.site.member[[get.pi[3]]]$TEAM_MEMBER_EMAIL
        }

    }
    return(sgi.ameriflux)
}
