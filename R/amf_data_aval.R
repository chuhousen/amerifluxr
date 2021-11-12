#' Get BASE data variable availability
#'
#' @description This function obtains the BASE data availability for all or
#' selected AmeriFlux sites. See AmeriFlux page
#' \url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/} for details
#' about the variable naming.
#'
#' @param site_set a scalar or vector of character specifying the target
#' AmeriFlux Site ID (CC-Sss). If not specified, it returns all sites.
#'
#' @return A data frame of variable-specific data availability (per year)
#'  for selected AmeriFlux sites.
#' \itemize{
#'   \item Site_ID - Six character site identifier (CC-Sss)
#'   \item VARIABLE - Variable name of the data included in the BASE file
#'   \item BASENAME - Variable base name of the data included in the BASE file.
#'   \item GAP_FILLED - Whether a variable is a gap-filled variable (TRUE/FALSE)
#'   \item Y1990 - Percentage of data availability in the year 1990 (0-1).
#'   \item Y1991 - Percentage of data availability in the year 1991 (0-1).
#'   \item Y1992 - Percentage of data availability in the year 1992 (0-1).
#'   \item ...
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # obtain the data variable availability for all sites
#' data_aval <- amf_data_aval()
#'
#' # obtain the data variable availability for selected sites
#' data_aval <- amf_data_aval(site_set = c("US-CRT","US-WPT"))
#'
#' }

amf_data_aval <- function(site_set = NULL) {
  # check if the file exists
  if (httr::HEAD(amf_server("data_variable"))$status_code == 200) {
    # get latest data variable availability
    data_aval <- utils::read.csv(
      amf_server("data_variable"),
      header = T,
      skip = 1,
      stringsAsFactors = FALSE
    )

    # subset interested sites
    if (!is.null(site_set)) {
      check_id <- amf_check_site_id(site_set)

      # check if any or all site_set not valid site ID
      if (any(!check_id) & !all(!check_id)) {
        warning(paste(
          paste(site_set[which(!check_id)], collapse = ", "),
          "not valid AmeriFlux Site ID"
        ))
        site_set <- site_set[which(check_id)]
        data_aval <- data_aval[data_aval$SITE_ID %in% site_set, ]

      } else if (!any(!check_id)) {
        data_aval <- data_aval[data_aval$SITE_ID %in% site_set, ]

      } else{
        stop("Download failed, no valid Site ID in site_set")

        data_aval <- NULL
      }
    }

  } else{
    stop("Download failed, timeout or server error...")

    data_aval <- NULL

  }
  return(data_aval)
}
