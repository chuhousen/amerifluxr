#' Read AmeriFlux BASE data product
#'
#' @description This function read in the BASE data file downloaded
#' from AmeriFlux. See AmeriFlux web page
#' \url{https://ameriflux.lbl.gov/data/data-processing-pipelines/base-publish/}
#' for details about BASE data product. Use \code{\link{amf_variables}}
#' to get a list of standard variable names and units.
#'
#' @param file a BASE data file, either in a zipped file or a comma-separate
#'  value (csv) file
#' @param unzip whether to unzip. Set TRUE if reading from a zipped file
#' @param parse_timestamp whether to parse the timestamp. Set TRUE to parse
#'  and add timekeeping columns.
#'
#' @return A data frame containing data. See AmeriFlux website
#'  \url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/}
#' for details about file format, variable definition, units, and convention.
#' If parse_timestamp = TRUE, the following six time-keeping columns are
#' added in the returned data frame:
#' \itemize{
#'   \item YEAR - Year (YYYY)
#'   \item MONTH - Month (MM)
#'   \item DAY - Day of the month (DD)
#'   \item DOY - Day of the year (DDD)
#'   \item HOUR - Hour of the day (HH), based on the middle time of the interval
#'   \item MINUTE - Minute of the hour (mm), based on the middle time of the interval
#' }
#' @seealso amf_variables
#' @export
#'
#' @examples
#' \dontrun{
#' # read the BASE from a zip file, using the example data file
#' base <- amf_read_base(file = system.file("extdata",
#'                                          "AMF_US-CRT_BASE-BADM_2-5.zip",
#'                                           package = "amerifluxr"),
#'                       unzip = TRUE,
#'                       parse_timestamp = TRUE)
#'
#' # read the BASE from a csv file
#' base <- amf_read_base(file = system.file("extdata",
#'                                          "AMF_US-CRT_BASE_HH_2-5.csv",
#'                                           package = "amerifluxr"),
#'                       unzip = FALSE,
#'                       parse_timestamp = FALSE)
#'}

amf_read_base <- function(file,
                          unzip = TRUE,
                          parse_timestamp = FALSE) {
  # stop if missing file parameter
  if (missing(file)) {
    stop('File not specified...')
  }

  # check if the file exists
  if (!file.exists(file)) {
    stop('File not found...')
  }

  if (unzip) {
    # check if file extension valid
    if (tools::file_ext(file) != "zip") {
      stop('File extention not valid...')
    }

    ## specify the BASE files under zip to be grabbed
    file_grab <- unzip(file, list = TRUE)[, "Name"]
    case_ls <-
      file_grab[which(substr(file_grab, start = 12, stop = 15) == "BASE")]

    if (length(case_ls) != 1) {
      stop('Can not find BASE file...')

    } else{
      # get file resolution
      res <- substr(case_ls, start = 17, stop = 18)

      ## read BASE
      data1 <-
        utils::read.table(
          unz(file,
              case_ls),
          na = c("-9999"),
          header = TRUE,
          sep = ",",
          skip = 2,
          stringsAsFactors = FALSE
        )
    }

  } else{
    # check if file extension valid
    if (tools::file_ext(file) != "csv") {
      stop('File extention not valid...')
    }

    case_ls <-
      basename(file)[which(substr(basename(file) , start = 12, stop = 15) == "BASE")]

    if (length(case_ls) != 1) {
      stop('Can not find BASE file...')

    } else{
      # get file resolution
      res <- substr(case_ls, start = 17, stop = 18)

      ## read BASE
      data1 <-
        utils::read.table(
          file,
          na = c("-9999"),
          header = TRUE,
          sep = ",",
          skip = 2,
          stringsAsFactors = FALSE
        )
    }
  }

  ##############################################################################
  # Parse TIMESTAMP
  if (parse_timestamp) {
    # determine time stamp resolution
    if (res %in% c("HH", "HR")) {
      # minutes per time step
      hr <- ifelse(res == "HH", 30, 60)

      ## create TIMESTAMP based on middle time of each interval
      if (length(which(colnames(data1) == "TIMESTAMP_START")) == 1) {
        TIMESTAMP <-
          strptime(data1$TIMESTAMP_START,
                   format = "%Y%m%d%H%M",
                   tz = "UTC")
        TIMESTAMP <-
          strptime(TIMESTAMP + 0.5 * hr * 60,
                   format = "%Y-%m-%d %H:%M:%S",
                   tz = "UTC")

      } else if (length(which(colnames(data1) == "TIMESTAMP_END")) == 1) {
        TIMESTAMP <-
          strptime(data1$TIMESTAMP_END,
                   format = "%Y%m%d%H%M",
                   tz = "UTC")
        TIMESTAMP <-
          strptime(TIMESTAMP - 0.5 * hr * 60,
                   format = "%Y-%m-%d %H:%M:%S",
                   tz = "UTC")

      } else{
        stop("Can not find TIMESTAMP columns...")
        TIMESTAMP <- NULL

      }

      if (!is.null(TIMESTAMP)) {
        data1 <- data.frame(
          YEAR = TIMESTAMP$year + 1900,
          MONTH = TIMESTAMP$mon + 1,
          DAY = TIMESTAMP$mday,
          DOY = TIMESTAMP$yday + 1,
          HOUR = TIMESTAMP$hour,
          MINUTE = TIMESTAMP$min,
          data1,
          stringsAsFactors = FALSE
        )
      }

    } else{
      stop('Can not parse time stamp...')

    }
  }
  return(data1)
}
