#' Get FP (Flux-Processing) Standard Variable List
#' @description This function obtains the latest AmeriFlux FP (Flux-Processing) standard variable list.
#'  FP standard defines the variable names and units used for continuously sampled data within the AmeriFlux.
#'  Also see AmeriFlux Data Variables page \url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/} for details.
#' @return A data frame containing the following columns:
#' \itemize{
#'   \item Name - Standard variable name
#'   \item Description - Description of the variable
#'   \item Units - Standard variable unit
#'   \item Min - Expected minimal value
#'   \item Max - Expected maximal value
#'   }
#' @export
#'
#' @examples
#' ## Not run:
#' # download the list of standard variable names and units
#' FP_ls <- get_FP_var()
#'
#' ## End(Not run)
get_FP_var <- function(){

  # web service hosted on AmeriFlux website
  FP.ls.ws <- "http://ameriflux-data.lbl.gov/AmeriFlux/SiteSearch.svc/fpinVarLimits"

  # get a list of FP (Flux-Processing) standard variables
  FP.ls <- jsonlite::fromJSON(httr::content(httr::POST(FP.ls.ws),
                                            as="text"),  flatten=TRUE)

  FP.ls$Min <- as.numeric(as.character(FP.ls$Min))
  FP.ls$Max <- as.numeric(as.character(FP.ls$Max))

  return(FP.ls)
}
