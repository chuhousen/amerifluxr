#' Download AmeriFlux BASE data product through web service
#'
#' @description This function allows downloading AmeriFlux BASE and BADM data files. Note: Access to AmeriFlux data requires creating
#' an AmeriFlux account first. Register an account through the link \url{https://ameriflux-data.lbl.gov/Pages/RequestAccount.aspx}.
#' For details about BASE and BADM data files, see AmeriFlux web pages \url{https://ameriflux.lbl.gov/data/data-processing-pipelines/base-publish/}
#' and \url{https://ameriflux.lbl.gov/data/aboutdata/badm-data-product/}.
#'
#' @param user_id AmeriFlux account username (character)
#' @param user_email AmeriFlux account user email (character)
#' @param site_id A vector of character specifying the target AmeriFlux Site ID (CC-Sss)
#' @param data_product AmeriFlux data product. Currently, only "BASE-BADM" is supported and used as default.
#' @param data_policy "CCBY4.0" or "LEGACY". AmeriFlux data are shared under two tiers of licenses as chosen by site's PI.
#' See \url{https://ameriflux.lbl.gov/data/data-policy/#data-use} for data use guidelines under each license. Note: Data use policy
#' selected affects which sites’ data are available for download.
#' @param intended_use The intended use category. Currently, it needs to be one of the followings: "synthesis", "model", "remote_sensing",
#' "other_research", "education", or "other"
#' @param intended_use_text Enter a brief description of intended use. This will be recorded in the data download log and emailed to
#' site's PI (free text).
#' @param out_dir Output directory for downloaded data
#' @param verbose Show feedback on download progress (TRUE/FALSE)
#'
#' @return A vector of download filenames on the local drive
#'
#' @export
#' @examples
#'
#' \dontrun{
#' ## Download a single site, under CCBY4.0 policy
#' amf_download_base(user_id = "test",
#'  user_email = "test@mail.com",
#'  site_id = "US-CRT",
#'  data_product = "BASE-BADM",
#'  data_policy = "CCBY4.0",
#'  intended_use = "other",
#'  intended_use_text = "testing download",
#'  out_dir = tempdir())
#'
#' ## Download several sites, under LEGACY data policy
#' #  When finished, return a list of downloaded files
#' #  in your local drive.
#' file.ls <- amf_download_base(user_id = "test",
#'  user_email = "test@mail.com",
#'  site_id = c("US-CRT", "US-WPT", "US-Oho"),
#'  data_product = "BASE-BADM",
#'  data_policy = "LEGACY",
#'  intended_use = "other",
#'  intended_use_text = "testing download",
#'  out_dir = tempdir())
#'}
#'
amf_download_base <- function(user_id,
                              user_email,
                              site_id,
                              data_product = "BASE-BADM",
                              data_policy,
                              intended_use,
                              intended_use_text,
                              out_dir = tempdir(),
                              verbose = TRUE) {
  ## obtain formal intended use category
  intended_use_cat <- function(intended_use) {
    intended_use_verbse <- switch(
      intended_use,
      synthesis = "Research - Multi-site synthesis",
      remote_sensing = "Research - Remotesensing",
      model = "Research - Land model/Earth system model",
      other_research = "Research - Other",
      education = "Education (Teacher or Student)",
      other = "Other"
    )
    return(intended_use_verbse)
  }

  if (data_policy == "CCBY4.0") {
    cat("Data shared under the AmeriFlux Legacy Data Policy follow the attribution guidelines:\n")
    cat("(1) Provide a citation to each site’s data product that includes the data-product DOI and/or recommended publication.\n")
    cat("(2) Acknowledge funding for site support if it was provided in the data download information.\n")
    cat("(3) Acknowledge funding for supporting AmeriFlux data portal: U.S. Department of Energy Office of Science.\n")
    cat("Please acknowledge you read and agree to the AmeriFlux CC-BY-4.0 Data Policy (https://ameriflux.lbl.gov/data/data-policy/#data-use).")
    agree_policy <- readline(prompt =
                               "[Yes/No]")

  } else if (data_policy == "LEGACY") {
    cat("Data shared under the AmeriFlux CC-BY-4.0 License follow the attribution guidelines:\n")
    cat("(1) Provide a citation to each site’s data product that includes the data-product DOI.\n")
    cat("(2) Acknowledge funding for supporting AmeriFlux data portal: U.S. Department of Energy Office of Science.\n")
    cat("Please acknowledge that you read and agree to the AmeriFlux Legacy Data Policy (https://ameriflux.lbl.gov/data/data-policy/#data-use).")
    agree_policy <- readline(prompt =
                               "[Yes/No]")

  } else {
    stop("Need to specify a data policy before proceed")
  }

  if (agree_policy %in% c("Yes", "yes", "YES", "y", "Y")) {

    ## set it for download testing
    #  this is used only while code developments, to be removed
    is_test <- TRUE

    ## prepare a list of site id for json query
    if (length(site_id) > 1) {
      site_id_txt <- paste0(site_id, collapse = "\", \"")
    } else{
      site_id_txt <- site_id
    }

    ## payload for download web service
    json_query <-
      paste0(
        "{\"user_id\":\"",
        user_id,
        "\",\"user_email\":\"",
        user_email,
        "\",\"data_product\":\"",
        data_product,
        "\",\"data_policy\":\"",
        data_policy,
        "\",\"site_ids\":[\"",
        site_id_txt,
        "\"],\"intended_use\":\"",
        intended_use_cat(intended_use = intended_use),
        "\",\"description\":\"",
        paste0("[amerifluxr download] ", intended_use_text),
        "\",\"is_test\":\"",
        ifelse(is_test, "true", ""),
        "\"}"
      )

    result <-
      httr::POST(
        amf_server("data_download"),
        body = json_query,
        encode = "json",
        httr::add_headers(`Content-Type` = "application/json")
      )

    # check if FTP returns correctly
    if (result$status_code == 200) {
      ## get a list of fpt url
      link <- httr::content(result)
      ftplink <- NULL
      if (length(link$data_urls) > 0) {
        for (i in 1:length(link$data_urls)) {
          ftplink <- c(ftplink,
                       link$data_urls[[i]]$url)
        }
      }

      ## check if any site_id has no data
      if (is.null(ftplink)) {
        stop(paste0("Cannot find data from ", site_id))
      }

      # get zip file names
      outfname <- strsplit(ftplink, c("/"))
      outfname <- sapply(outfname,  utils::tail, n = 1)
      outfname <- substr(outfname, 1, sapply(outfname, regexpr, pattern = "?=", fixed = T) - 1)

      ## check if any site_id has no data
      if (length(outfname) < length(site_id)) {
        miss_site_id <-
          site_id[which(!site_id %in% substr(outfname, 5, 10))]
        warning(paste0("Cannot find data from ", miss_site_id))
      }

      ## download sequentially
      output_zip_file <- file.path(out_dir, outfname)
      for (ii in 1:length(ftplink)) {
        utils::download.file(ftplink[ii],
                             output_zip_file[ii],
                             mode = "wb",
                             quiet = !verbose)
      }

      ## check if downloaded files exist
      miss_download <- which(!sapply(output_zip_file, file.exists))
      if (length(miss_download) > 0) {
        warning(paste("Cannot download", output_zip_file[miss_download], "from", ftplink[miss_download]))
      }

      message(paste())

    } else{
      stop("Data download fails, timeout or server error...")

      output_zip_file <- NULL
    }

  } else {
    stop("Need to acknowledge data policy before proceed")

  }
  return(output_zip_file)
}
