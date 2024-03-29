#' Download AmeriFlux BADM data product
#'
#' @description This function downloads all AmeriFlux sites' BADM
#' data files in a single file. Note: Access to AmeriFlux data requires
#' creating an AmeriFlux account first.
#' Register an account through the link
#' \url{https://ameriflux-data.lbl.gov/Pages/RequestAccount.aspx}.
#'
#' For details about BADM data files, see AmeriFlux web page
#' \url{https://ameriflux.lbl.gov/data/aboutdata/badm-data-product/}.
#'
#' @param user_id AmeriFlux account username (character)
#' @param user_email AmeriFlux account user email (character)
#' @param data_policy "CCBY4.0" or "LEGACY". AmeriFlux data are shared under
#'  two tiers of licenses as chosen by site's PI. See
#'  \url{https://ameriflux.lbl.gov/data/data-policy/#data-use} for data use
#'  guidelines under each license. Note: Data use policy
#'  selected affects which sites’ data are available for download.
#' @param agree_policy Acknowledge you read and agree to the AmeriFlux
#' Data use policy (TRUE/FALSE)
#' @param intended_use The intended use category. Currently, it needs to be one
#'  of the followings:
#'  \itemize{
#'    \item "synthesis" (i.e., Multi-site synthesis)
#'    \item "model" (i.e., Land model/Earth system model)
#'    \item "remote_sensing" (i.e., Remote sensing research)
#'    \item "other_research" (i.e., Other research)
#'   "\item "education" (i.e., Education (Teacher or Student))
#'    \item "other"
#'  }
#' @param intended_use_text Enter a brief description of intended use.
#' This will be recorded in the data download log and emailed to
#' site's PI (character).
#' @param out_dir Output directory for downloaded data, default tempdir()
#' @param verbose Show feedback on download progress (TRUE/FALSE)
#' @param site_w_data Logical, download all registered sites (FALSE)
#' or only sites with available BASE data (TRUE)
#'
#' @return A vector of download file names on the local drive
#' @export
#'
#' @seealso \code{\link{amf_download_base}}
#'
#' @examples
#'
#' \dontrun{
#' ## Download all sites with BASE data, under CCBY4.0 policy
#' amf_download_bif(user_id = "test",
#'   user_email = "test@@mail.com",
#'   data_policy = "CCBY4.0",
#'   agree_policy = TRUE,
#'   intended_use = "other",
#'   intended_use_text = "testing download",
#'   out_dir = tempdir(),
#'   site_w_data = TRUE)
#'
#' ## Download all registered sites, under LEGACY policy
#' amf_download_bif(user_id = "test",
#'   user_email = "test@@mail.com",
#'   data_policy = "LEGACY",
#'   agree_policy = TRUE,
#'   intended_use = "other",
#'   intended_use_text = "testing download",
#'   out_dir = tempdir(),
#'   site_w_data = FALSE)
#'
#'}
#'
amf_download_bif <- function(user_id,
                             user_email,
                             data_policy,
                             agree_policy,
                             intended_use,
                             intended_use_text,
                             out_dir = tempdir(),
                             verbose = TRUE,
                             site_w_data = FALSE) {

  output_zip_file <- amf_download_base(
    user_id = user_id,
    user_email = user_email,
    site_id = ifelse(site_w_data, "AA-Flx", "AA-Net"),
    data_product = "BASE-BADM",
    data_policy = data_policy,
    agree_policy = agree_policy,
    intended_use = intended_use,
    intended_use_text = intended_use_text,
    out_dir = out_dir,
    verbose = verbose
  )

  return(output_zip_file)

}
