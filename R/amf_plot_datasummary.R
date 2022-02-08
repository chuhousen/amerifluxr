#' Plot data summary
#'
#' @description This function visualizes the BASE data summary for
#' selected AmeriFlux sites and variables. This is a wrapper around
#' \code{\link{amf_summarize_data}}. However, it is strongly advised to
#' subset either sites or variables for faster processing and better
#' visualization.
#'
#' @param data_sum A data frame with following columns:
#' \itemize{
#'   \item Site_ID - Six character site identifier (CC-Sss)
#'   \item VARIABLE - Variable name of the data included in the BASE file
#'   \item BASENAME - Variable base name of the data included in the BASE file.
#'   \item GAP_FILLED - Whether a variable is a gap-filled variable (TRUE/FALSE)
#'   \item Any statistics, e.g., P01, P05... output from
#'   \code{\link{amf_summarize_data}}
#'  }
#'  If not specified, use \code{\link{amf_summarize_data}} by default.
#' @param site_set A scalar or vector of character specifying the target
#' AmeriFlux Site ID (CC-Sss). If not specified, it returns all sites.
#' @param var_set A scalar or vector of character specifying the target
#' variables as in basename. See AmeriFlux
#' page\url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/#base}
#' for a list of variable names. If not specified, it returns all variables.
#' @param nonfilled_only Logical, whether only showing non-filled variables,
#'  or both non- and gap-filled variables. The default is TRUE.
#' @param show_cluster Logical, whether showing clustering (dendrogram) of
#'  site-variables. The default is FALSE.
#' @param scale Logical, whether the values should be centered and scaled
#' among site-variables. The default is FALSE.
#' @return An object of class 'plotly' from \code{\link[heatmaply]{heatmaply}}
#' @seealso \code{\link{amf_summarize_data}}, \code{\link[heatmaply]{heatmaply}}
#' @export
#'
#' @examples
#' \dontrun{
#' # plot data summary for selected variables at two sites
#' amf_plot_datasummary(site_set = c("US-CRT", "US-WPT"),
#'                      var_set = c("H", "LE", "NETRAD"))
#'
#' # plot data summary for FCH4 at all sites, and show
#' #  clustering among sites
#' amf_plot_datasummary(var_set = "FCH4",
#'                      show_cluster = TRUE)
#'
#' # plot data summary for TA at all grassland sites,
#' #  and show clustering among sites
#' sites <- amf_site_info()
#' sites <- subset(sites, IGBP == "CRO")
#' amf_plot_datasummary(site_set = sites$SITE_ID,
#'                      var_set = "TA",
#'                      show_cluster = TRUE)
#'
#' # normalize TA among sites
#' amf_plot_datasummary(site_set = sites$SITE_ID,
#'                      var_set = "TA",
#'                      show_cluster = TRUE,
#'                      scale = TRUE)
#' }
amf_plot_datasummary <- function(data_sum = NULL,
                                 site_set = NULL,
                                 var_set = NULL,
                                 nonfilled_only = TRUE,
                                 show_cluster = FALSE,
                                 scale = FALSE) {
  # check either site_set or var_set exist
  if (is.null(site_set) & is.null(var_set)) {
    stop("Specify either site_set or var_set...")
  }

  # subset interested sites
  all_site <- amerifluxr::amf_sites()[, "SITE_ID"]
  if (is.null(site_set)) {
    site_set <- all_site
  } else {
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

  # If unspecified, obtain var_set through amf_variables()
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

  #### subset data_sum
  if (is.null(data_sum)) {
    data_sum <- amerifluxr::amf_summarize_data(site_set = site_set,
                                               var_set = var_set)
    data_sum <- data_sum[, -c(which(colnames(data_sum) == "DATA_RECORD"),
                              which(colnames(data_sum) == "DATA_MISSING"))]
  }

  # check if the default columns exist
  if (!is.data.frame(data_sum)) {
    stop("data_sum format unrecognized...")
  } else if (sum(c("SITE_ID", "VARIABLE", "BASENAME", "GAP_FILLED") %in%
                 colnames(data_sum)) != 4) {
    stop("data_sum format unrecognized...")
  }

  # subset gap-/non-filled
  if (nonfilled_only)
    data_sum <- data_sum[!data_sum$GAP_FILLED, ]

  ## If unspecified, obtain year_set from all available years
  data_sum_viz <- data_sum[, 5:ncol(data_sum)]
  row.names(data_sum_viz) <-
    paste(data_sum$SITE_ID, data_sum$VARIABLE)

  if (nrow(data_sum_viz) > 500)
    warning("Too many site-variables, consider subseting either...")

  ## prepare data for heatmap
  data_sum_viz <- (as.matrix(data_sum_viz))

  p <- heatmaply::heatmaply(
    data_sum_viz,
    dendrogram = ifelse(show_cluster, "row", "none"),
    xlab = "",
    ylab = "",
    main = "",
    scale = ifelse(scale, "row", "none"),
    margins = c(60, 200, 20, 20),
    titleX = FALSE,
    plot_method = "plotly",
    hide_colorbar = FALSE,
    label_names = c("Variable", "Statistics", "Value"),
    fontsize_row = 10,
    fontsize_col = 10,
    labCol = colnames(data_sum_viz),
    labRow = rownames(data_sum_viz)
  )

  return(p)
}
