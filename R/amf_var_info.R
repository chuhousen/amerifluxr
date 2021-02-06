#' Get variable information
#'
#' @description This function obtains the measurement height metadata for the AmeriFlux BASE data product.
#' See AmeriFlux page \url{https://ameriflux.lbl.gov/data/measurement-height/} for details.
#'
#' @param out_dir output directory  (default = tempdir())
#'
#' @return A data frame of measurement height data for all vaiable AmeriFlux sites
#' #' \itemize{
#'   \item Site_ID - Six character site identifier (CC-Sss)
#'   \item Variable - Variable name of the data included in the BASE file
#'   \item Start_Date - Date when the information first applies
#'   \item Height - Distance above the ground surface in meters
#'   \item Instrument_Model - Instrument model used to collect the data variable
#'   \item Instrument_Model2 - A second instrument model used to collect the data variable
#'   \item Comment - Additional information provided by the site team
#'   \item BASE_Version - The most recent BASE data product version number for which the information applies
#'   }
#' @export
#' @examples
#' ## Not run:
#' # download the measurement height data for all sites
#' var_info <- amf_var_info()
#'
#' ## End(Not run)

amf_var_info <- function(out_dir = tempdir()) {

  # Endpoint of measurement height data
  url1 <- "ftp://ftp.fluxdata.org/.ameriflux_downloads/measurement_height/"
  url2 <- "https://ftp.fluxdata.org/.ameriflux_downloads/measurement_height/"

  # Get a list of hosted data files
  filenames <- RCurl::getURL(url1, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- paste0(strsplit(filenames, "\r*\n")[[1]])
  filenames <- filenames[which(nchar(filenames) == 35)]

  if (length(filenames) > 1) {

    # get the latest version of measurement height data
    time_ls <- as.numeric(substr(filenames, start = 24, stop = 31))
    var_info_ver <- time_ls[which(time_ls == max(time_ls))]

    latest <- filenames[which(time_ls == max(time_ls))]

    # download data to designated output directory
    utils::download.file(paste0(url2, "/", latest), file.path(out_dir, latest))

    var_info <- utils::read.csv(file.path(out_dir, latest), header = T, na.strings = c(""))
    var_info$Height <- as.numeric(as.character(var_info$Height))

    ## clean for a bug in earlier version
    var_info$Variable <- gsub("_PI_PI", "_PI", var_info$Variable)

  } else {

    stop("Download failed, timeout or server error...")

    var_info <- NULL

  }

  return(var_info)
}
