#' Get BASE data summary
#'
#' @description This function obtains the BASE data summary for all or
#' selected AmeriFlux sites. See AmeriFlux page
#' \url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/} for details
#' about the variable naming.
#'
#' @param site_set A scalar or vector of character specifying the target
#' AmeriFlux Site ID (CC-Sss). If not specified, it returns all sites.
#' @param var_set A scalar or vector of character specifying the target
#' variables as in basename. See AmeriFlux
#' page\url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/#base}
#' for a list of variable names. If not specified, it returns all variables.
#'
#' @return A data frame of site-specific variable summary statistics (selected
#' percentiles) for selected AmeriFlux sites.
#' \itemize{
#'   \item Site_ID - Six character site identifier (CC-Sss)
#'   \item VARIABLE - Variable name of the data included in the BASE file
#'   \item BASENAME - Variable base name of the data included in the BASE file.
#'   \item GAP_FILLED - Whether a variable is a gap-filled variable (TRUE/FALSE)
#'   \item DATA_RECORD - Number of supposed data record (counts)
#'   \item DATA_MISSING - Number of missing data record (counts)
#'   \item Q01 - 1th percentile of the data
#'   \item Q05 - 5th percentile of the data
#'   \item ...
#'   \item Q95 - 95th percentile of the data
#'   \item Q99 - 99th percentile of the data
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # obtain the data variable availability for all sites & variables
#' data_sum <- amf_summarize_data()
#'
#' # obtain the data variable availability for selected sites, all variables
#' data_sum <- amf_summarize_data(site_set = c("US-CRT","US-WPT"))
#'
#' # obtain the data variable availability for selected sites & variables
#' data_sum <- amf_summarize_data(site_set = c("US-CRT","US-WPT"),
#'                                var_set = c("FC", "LE", "H"))
#' }

amf_summarize_data <- function(site_set = NULL,
                               var_set = NULL) {
  # check if the file exists
  if (httr::HEAD(amf_server("data_summary"))$status_code == 200) {
    # get latest data summary
    data_sum <- utils::read.csv(
      amf_server("data_summary"),
      header = TRUE,
      skip = 1,
      stringsAsFactors = FALSE
    )

    # subset interested sites
    all_site <- amerifluxr::amf_sites()[, "SITE_ID"]
    if (is.null(site_set)) {
      site_set <- all_site
    } else {
      check_id <- amf_check_site_id(site_set)
      # check if any or all site_set not valid site ID
      if (any(!check_id) & !all(!check_id)) {
        warning(paste(
          paste(site_set[which(!check_id)], collapse = ", "),
          "not valid AmeriFlux Site ID"
        ))
        site_set <- site_set[which(check_id)]
      } else if (all(!check_id)) {
        stop("Download failed, no valid Site ID in site_set")
      }
    }

    # Check var_set through amf_variables()
    FP_var <- amerifluxr::amf_variables()[, "Name"]
    if (is.null(var_set)) {
      var_set <- FP_var
    } else{
      # check if var_set are valid variable names
      check_var <- var_set %in% FP_var
      if (all(!check_var)) {
        stop("No valid variable in var_set...")
      } else if (any(!check_var) & !all(!check_var)) {
        warning(paste(paste(var_set[which(!check_var)], collapse = ", "),
                      "not valid variable names"))
        var_set <- var_set[which(check_var)]
      }
    }

    data_sum <- data_sum[data_sum$SITE_ID %in% site_set, ]
    data_sum <- data_sum[data_sum$BASENAME %in% var_set, ]

  } else{
    stop("Download failed, timeout or server error...")

    data_sum <- NULL

  }
  return(data_sum)
}
