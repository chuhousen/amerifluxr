#' Plot data availability
#'
#' @description This function visualizes the BASE data availability for
#' selected AmeriFlux sites, variables, and years. This is a wrapper around
#' \code{\link{amf_list_data}}. However, it is strongly advised to subset
#' the sites, variables, and/or years for faster processing and better
#' visualization.
#'
#' @param data_aval A data frame with at least five columns:
#' \itemize{
#'   \item SITE_ID:
#'   \item VARIABLE:
#'   \item BASENAME: variable basename
#'   \item GAP_FILLED
#'   \item Y1990: Percentage of data availability in the year 1990 (0-1).
#'   \item ...
#'  }
#'  If not specified, use \code{\link{amf_list_data}} by default.
#' @param site_set a scalar or vector of character specifying the target
#' AmeriFlux Site ID (CC-Sss). If not specified, it returns all sites.
#' @param var_set a scalar or vector of character specifying the target
#' variables as in basename. See AmeriFlux page
#' \url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/#base}
#' for a list of variable names. If not specified, it returns all variables.
#' @param nonfilled_only logical, whether only showing non-filled variables,
#'  or both non- and gap-filled variables (default = TRUE)
#' @param year_set a scalar or vector of integers. If not specified,
#' it plots only years with any available data in selected sites and variables
#' @param save_plot logical, whether to save the plot as a html file, or
#' return via default R plot viewer.
#' @param out_dir output directory (default = tempdir())
#' @param filename_prefix optional, a scalar of character used as the
#' prefix in the file name of output figure (...var_year_available.html)
#'
#' @seealso amf_list_data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # plot data availability for all variables at a single site
#' #  in all years, don't save figure.
#' amf_plot_datayear(site_set = "US-CRT",
#'                   nonfilled_only = FALSE,
#'                   save_plot = FALSE)
#'
#' # plot data availability for non-filled FCH4 and WTD at all
#' #  sites in all years, save figure.
#' amf_plot_datayear(var_set = c("FCH4", "WTD"),
#'                   nonfilled_only = TRUE)
#'
#' # plot data availability for non-filled FCH4 at all sites
#' #  in 2018-2020, save figure
#' amf_plot_datayear(var_set = "FCH4",
#'                   year_set = c(2018:2020),
#'                   nonfilled_only = TRUE)
#' }
amf_plot_datayear <- function(data_aval = NULL,
                              site_set = NULL,
                              var_set = NULL,
                              nonfilled_only = TRUE,
                              year_set = NULL,
                              save_plot = FALSE,
                              out_dir = tempdir(),
                              filename_prefix = NULL) {

  # check if out_dir reachable
  if(save_plot & !dir.exists(out_dir)){
    stop("out_dir not valid...")
  }

  # subset interested sites
  if (!is.null(site_set)) {
    check_id <- amerifluxr::amf_check_site_id(site_set)

    # check if any or all site_set not valid site ID
    if (any(!check_id) & !all(!check_id)) {
      warning(paste(
        paste(site_set[which(!check_id)], collapse = ", "),
        "not valid AmeriFlux Site ID"
      ))
      site_set <- site_set[which(check_id)]

    } else if (all(!check_id)) {
      stop("No valid Site ID in site_set")

    }
  }

  # If unspecified, obtain data_aval through amf_list_data()
  if (is.null(data_aval)) {
    data_aval <- amerifluxr::amf_list_data(site_set = site_set)
  }

  # check if the default columns exist
  if (!is.data.frame(data_aval)) {
    stop('data_aval format unrecognized...')
  } else if (sum(c("SITE_ID", "VARIABLE", "BASENAME", "GAP_FILLED") %in% colnames(data_aval)) != 4) {
    stop('data_aval format unrecognized...')
  }

  # If unspecified, obtain var_set through amf_variables()
  FP_var <- amerifluxr::amf_variables()[, "Name"]
  if (is.null(var_set)) {
    var_set <- FP_var

  } else{
    # check if var_set are valid variable names
    check_var <- var_set %in% FP_var
    if (any(!check_var)) {
      warning(paste(paste(var_set[which(!check_var)], collapse = ", "),
                    "not valid variable names"))
      var_set <- var_set[which(check_var)]

    }
    if (length(var_set) == 0) {
      stop("No valid variable in var_set...")
    }
  }

  #### subset data_aval
  var_year <- data_aval[data_aval$BASENAME %in% var_set,]
  if (nonfilled_only)
    var_year <- var_year[!var_year$GAP_FILLED,]

  ## If unspecified, obtain year_set from all available years
  var_year_viz <- var_year[, 5:ncol(var_year)]
  row.names(var_year_viz) <-
    paste(var_year$SITE_ID, var_year$VARIABLE)

  #  return years with any available data
  year_ava <-
    as.numeric(gsub("Y", "", colnames(var_year_viz[, apply(var_year_viz, 2, sum) > 0])))

  if (is.null(year_set)) {
    year_set <- year_ava
  } else if (!is.numeric(year_set)) {
    stop("No valid year in year_set...")
  } else{
    # check if year_set are valid years
    check_year <- year_set %in% year_ava
    if (any(!check_year)) {
      warning(paste(paste(year_set[which(!check_year)], collapse = ", "),
                    "have no data..."))
      year_set <- year_set[which(check_year)]

    }
    if (length(year_set) == 0) {
      stop("No valid year in year_set...")
    }
  }
  # subset years
  var_year_viz <-
    var_year_viz[, which(colnames(var_year_viz) %in% paste0("Y", year_set))]
  var_year_viz <-
    var_year_viz[apply(var_year_viz, 1, sum) > 0, ]

  if(nrow(var_year_viz) > 500)
    warning(
      "Too many site-variables, consider subseting either..."
    )

  ## prepare data for heatmap
  var_year_viz <- (as.matrix(var_year_viz))
  var_year_viz[which(var_year_viz == 0)] <- NA

  p <- heatmaply::heatmaply(
    var_year_viz,
    dendrogram = "none",
    xlab = "",
    ylab = "",
    main = "",
    scale = "none",
    margins = c(60, 200, 20, 20),
    grid_color = "grey90",
    grid_width = 0.00001,
    titleX = FALSE,
    hide_colorbar = FALSE,
    label_names = c("Variable", "Year", "Percentage"),
    fontsize_row = 10,
    fontsize_col = 10,
    labCol = colnames(var_year_viz),
    labRow = rownames(var_year_viz)
  )

  if(save_plot){
    htmlwidgets::saveWidget(p,
                            file = paste0(out_dir,
                                          filename_prefix,
                                          "var_year_available.html"))
  }else{
    p
  }


}
