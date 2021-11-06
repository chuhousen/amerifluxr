#' Filter AmeriFlux BASE data product based on physical range
#'
#' @description The function filters BASE data product based on the expected
#' physical ranges specified for each variable. See AmeriFlux web site
#' \url{https://ameriflux.lbl.gov/data/data-processing-pipelines/data-qaqc/physical-range-module/}
#' for description of physical ranges.
#'
#' @param data_in A data frame containing BASE data, e.g.,
#'  import from \code{\link{amf_read_base}}.
#' @param limit_ls A data frame with at least three columns, i.e., Name, Min,
#'  Max, denoting the base name, expected lower and upper ranges of each
#'  variable. If not specified, use \code{\link{amf_variables}} by default.
#' @param basename_decode A data frame with at least two columns, i.e.,
#'  variable_name, basename, denoting the actual variable name and
#'  base name of each variable in data_in. If not specified,
#'  use \code{\link{amf_parse_basename}} by default.
#' @param loose_filter A number in ratio (0-1) used to adjust the physical range
#' for filtering. Set it to 0 if not used. The default is 0.05.
#' @return A data frame similar to data_in, with out-of-range data points
#'  being filtered out
#' @export
#' @seealso amf_read_base, amf_var_info, amf_parse_basename
#' @examples
#' \dontrun{
#' # read the BASE from a csv file
#' base <- amf_read_base(file = system.file("extdata",
#'                                          "AMF_US-CRT_BASE_HH_2-5.csv",
#'                                           package = "amerifluxr"),
#'                       unzip = FALSE,
#'                       parse_timestamp = FALSE)
#'
#' # filter data, using default physical range +/- 5% buffer
#' base_f <- amf_filter_base(data_in = base)
#'
#' # filter data, using default physical range without buffer
#' base_f <- amf_filter_base(data_in = base, loose_filter = 0)
#'}

amf_filter_base <- function(data_in,
                            limit_ls = NULL,
                            basename_decode = NULL,
                            loose_filter = 0.05) {

  # stop if missing data_in parameter
  if (missing(data_in)) {
    stop('data_in not specified...')
  }

  # unless specified, obtain limit_ls through amf_variables()
  if (is.null(limit_ls)) {
    limit_ls <- amerifluxr::amf_variables()
  }

  # check if the default columns exist
  if (sum(c("Name", "Min", "Max") %in% colnames(limit_ls)) != 3) {
    stop('limit_ls format unrecognized...')
  } else if (!is.character(limit_ls$Name) |
             !is.numeric(limit_ls$Min) |
             !is.numeric(limit_ls$Max)) {
    stop('limit_ls format unrecognized...')
  }

  # unless specified, obtain basename_decode using amf_parse_basename()
  if (is.null(basename_decode)) {
    basename_decode <- amerifluxr::amf_parse_basename(
      var_name = colnames(data_in),
      FP_ls = limit_ls$Name)
  }

  # check if default columns exist
  if (sum(c("variable_name","basename") %in% colnames(basename_decode)) != 2) {
    stop('basename_decode format unrecognized...')
  }

  # check loose_filter
  if (!is.numeric(loose_filter) & !is.na(loose_filter)) {
    stop('loose_filter should be numeric...')
  }else if (loose_filter < 0 | loose_filter > 1) {
    stop('loose_filter may be unrealistic...')
  }

  ## ensure data_in match the order in basename_decode
  var.order <- NULL
  for (i in seq_len(nrow(basename_decode))) {
    var.order <-
      c(var.order,
        which(colnames(data_in) == paste(basename_decode$variable_name[i])))
  }
  data_in <- data_in[, var.order]

  ## filter by data_in by limit_ls, based on matching
  ##  basename as provided in basename_decode
  for (l in seq_len(ncol(data_in))) {

    # locate corresponding criteria via the parsed basename
    limit_ls_loc <- which(limit_ls$Name == basename_decode$basename[l])

    if (length(limit_ls_loc) == 1) {

      var.upp <- limit_ls$Max[limit_ls_loc]
      var.low <- limit_ls$Min[limit_ls_loc]

      # adjust for loose filtering
      if (!is.na(var.upp) & !is.na(var.low) & !is.na(loose_filter)) {
        var.upp <- var.upp + loose_filter * abs(var.upp - var.low)
        var.low <- var.low - loose_filter * abs(var.upp - var.low)
      }

      if (!is.na(var.upp)) {
        data_in[which(data_in[, l] > var.upp), l] <- NA
      }

      if (!is.na(var.low)) {
        data_in[which(data_in[, l] < var.low), l] <- NA
      }
    }
  }

  return(data_in)
}
