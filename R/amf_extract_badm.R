#' Extract BADM data of a specific BADM group
#'
#' @description This function extracts BADM data of a specific BADM group
#' from the imported BADM (BIF) file. Use function \code{\link{amf_read_bif}}
#' to import BADM (BIF) file.
#'
#' @param bif_data A data frame consists of 5 columns: SITE_ID, GROUP_ID,
#' VARIABLE_GROUP, VARIABLE, DATAVALUE, imported from function
#' \code{\link{amf_read_bif}}.
#' @param select_group A string (character), selected from VARIABLE_GROUP
#'  in the \code{bif_data}
#'
#' @seealso \code{\link{amf_read_bif}}
#'
#' @return A data frame of re-structured BADM data with the following columns:
#' \itemize{
#'   \item GROUP_ID - A unique identifier for data belonging to the same instance of a reported variable group
#'   \item SITE_ID - Six character site identifier (CC-Sss)
#'   \item VALUE - Values for all available VARIABLES in the selected group
#'   \item ...
#' }
#' @export
#'
#' @examples
#' # read the BADM BIF file, using an example data file
#' bif <- amf_read_bif(file = system.file("extdata",
#'                                        "AMF_AA-Flx_BIF_CCBY4_20201218.xlsx",
#'                                         package = "amerifluxr"))
#'
#' # get a list of valid VARIALBE_GROUP
#' unique(bif$VARIABLE_GROUP)
#'
#' # extract the selected VARIALBE_GROUP
#' amf_extract_badm(bif_data = bif, select_group = "GRP_FLUX_MEASUREMENTS")
#' amf_extract_badm(bif_data = bif, select_group = "GRP_IGBP")

amf_extract_badm <- function(bif_data,
                             select_group) {
  # stop if missing bif_data parameter
  if (missing(bif_data)) {
    stop('bif_data not specified...')
  }

  # stop if missing bif_data parameter
  if (missing(select_group)) {
    stop('select_group not specified...')
  }

  # check if the default columns exist
  if (sum(
    c(
      "SITE_ID",
      "GROUP_ID",
      "VARIABLE_GROUP",
      "VARIABLE",
      "DATAVALUE"
    ) %in% colnames(bif_data)
  ) != 5) {
    stop('bif_data format unrecognized...')
  }

  # stop if select_group do not exist
  if (length(which(bif_data$VARIABLE_GROUP == select_group)) == 0) {
    stop("Extraction failed, cannot locate select_group...")

    bif_out <- NULL

  } else{
    # locate VARIALBE_GROUP
    bif_work <-
      bif_data[which(bif_data$VARIABLE_GROUP == select_group),]

    # get a list of VARIALBE under the specific VARIABLE_GROUP
    var_ls <- unique(bif_work$VARIABLE)

    # retrieve a list of GROUP_ID
    entry_ls <- as.character(bif_work$GROUP_ID)
    entry_ls <- unique(bif_work$GROUP_ID)

    # output data frame
    bif_out <- data.frame(
      GROUP_ID = tapply(bif_work$GROUP_ID,
                        bif_work$GROUP_ID,
                        function(x)
                          paste(x[1])),
      SITE_ID = tapply(bif_work$SITE_ID,
                       bif_work$GROUP_ID,
                       function(x)
                         paste(x[1])),
      stringsAsFactors = FALSE
    )

    # re-organize bif_data by unique GROUP_ID
    for (j in seq_len(length(var_ls))) {
      bif_work_tmp <-
        bif_work[bif_work$VARIABLE == paste(var_ls[j]),
                 c("GROUP_ID", "DATAVALUE")]

      bif_out <- merge.data.frame(bif_out,
                                  bif_work_tmp,
                                  by = "GROUP_ID", all = TRUE)

      colnames(bif_out)[ncol(bif_out)] <- paste(var_ls[j])
    }

    bif_out <- bif_out[order(bif_out$SITE_ID),]

  }

  return(bif_out)
}
