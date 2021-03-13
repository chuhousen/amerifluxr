#' Get metadata availability
#'
#' @description This function obtains the metadata (i.e., BADM) availability for all or selected AmeriFlux sites.
#' See AmeriFlux page \url{https://ameriflux.lbl.gov/data/badm/} for details about the BADM.
#'
#' @param site_set a vector of character specifying the target AmeriFlux Site ID (CC-Sss). If not specified, it returns all sites.
#' @param group_only logical. Should it return availability for BADM variable groups or variables?
#'
#' @return A data frame of data variable availability (per year) for selected AmeriFlux sites. The first column contains the full site list.
#' The remaining columns contains the number of entries for a variable or a variable group, with column names specifying the variable or
#' group names.
#' \itemize{
#'   \item Site_ID - Six character site identifier (CC-Sss)
#'   \item ...
#'   }
#' @export
#'
#' @examples
#' #' ## Not run:
#' # obtain the metadata availability for all sites, at variable group levels
#' metadata_aval <- amf_metadata_aval()
#'
#' # obtain the metadata availability for selected sites, at variable levels
#' metadata_aval <- amf_metadata_aval(site_set = c("US-CRT","US-WPT"), group_only = FALSE)
#'
#' ## End(Not run)
#'
amf_metadata_aval <- function(site_set = NULL, group_only = TRUE){

  # determine the level of granularity
  target_level <- ifelse(group_only, "bif_group", "bif_variable")

  # check if the file exists
  if(httr::HEAD(amf_server())$status_code == 200){

    # get latest data variable availability
    metadata_aval <- utils::read.csv(amf_server(target_level),
                                 header = T,
                                 skip = 1)

    # subset interested sites
    if(!is.null(site_set)){

      metadata_aval <- metadata_aval[metadata_aval$SITE_ID %in% site_set,]

    }

  }else{

    stop("Download failed, timeout or server error...")

    metadata_aval <- NULL

  }
  return(metadata_aval)
}
