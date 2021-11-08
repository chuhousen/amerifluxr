#' Read AmeriFlux BADM data product
#'
#' @description This function read in the BADM data file formatted in BADM Interchange Format (BIF).
#' @param file a BADM data file
#'
#' @return A data frame containing the following 5 columns. See AmeriFlux website
#'  \url{https://ameriflux.lbl.gov/data/aboutdata/badm-data-product/} for details.
#' \itemize{
#'   \item SITE_ID - Six character site identifier (CC-Sss)
#'   \item GROUP_ID - A unique identifier for data belonging to the same instance of a reported variable group
#'   \item VARIABLE_GROUP - A set of variables that are reported together
#'   \item VARIABLE - The variable name
#'   \item DATAVALUE - The reported value of a variable
#' }
#' @export
#'
#' @examples
#' ## Not run:
#' # read the BADM BIF file, using an example data file
#' bif <- amf_read_bif(file = system.file("extdata",
#'                                        "AMF_AA-Flx_BIF_20201218.xlsx",
#'                                         package = "amerifluxr"))
#'
#' ## End(Not run)
#'
amf_read_bif <- function(file) {

  # stop if missing file parameter
  if (missing(file)) {
    stop('File not specified...')
  }

  # check if the file exists
  if (!file.exists(file)) {
    stop('File not found...')
  }

  # read in BIF excel file
  bif_data <- readxl::read_excel(file,
                                 sheet = 1,
                                 col_names = T,
                                 na = "-9999")

  bif_data$GROUP_ID <- as.character(bif_data$GROUP_ID)
  bif_data$VARIABLE_GROUP <- as.character(bif_data$VARIABLE_GROUP)
  bif_data$DATAVALUE <- as.character(bif_data$DATAVALUE)
  bif_data$SITE_ID <- as.character(bif_data$SITE_ID)

  return(bif_data)
}
