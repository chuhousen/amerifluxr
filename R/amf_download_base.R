#' Download AmeriFlux BASE data product
#'
#' @description This function downloads AmeriFlux BASE and BADM data files.
#'  Note: Access to AmeriFlux data requires creating an AmeriFlux account
#'  first. Register an account through the link
#'   \url{https://ameriflux-data.lbl.gov/Pages/RequestAccount.aspx}.
#'
#' For details about BASE and BADM data files, see AmeriFlux web pages
#'  \url{https://ameriflux.lbl.gov/data/data-processing-pipelines/base-publish/}
#' and \url{https://ameriflux.lbl.gov/data/aboutdata/badm-data-product/}.
#'
#' @param user_id AmeriFlux account username (character)
#' @param user_email AmeriFlux account user email (character)
#' @param site_id A scalar or vector of character specifying the AmeriFlux
#'  Site ID (CC-Sss)
#' @param data_product AmeriFlux data product. Currently, only "BASE-BADM" is
#' supported and used as default. (character)
#' @param data_policy "CCBY4.0" or "LEGACY" (character). AmeriFlux data
#'  are shared under two tiers of licenses as chosen by site's  PI. See
#' \url{https://ameriflux.lbl.gov/data/data-policy/#data-use} for data use
#' guidelines under each license. Note: Data use policy
#' selected affects which sites’ data are available for download.
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
#' @param intended_use_text Enter a brief description of intended use. This will
#' be recorded in the data download log and emailed to
#' site's PI (free text).
#' @param out_dir Output directory for downloaded data, default tempdir()
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
#'  user_email = "test _at_ mail.com",
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
#'  user_email = "test _at_ mail.com",
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

  ## check all inputs valid
  if (!is.character(user_id) |
      length(user_id) != 1) {
    stop('user_id should be a string...')
  }

  if (!is.character(user_email) |
      length(user_email) != 1 |
      !grepl("@", user_email)) {
    stop('user_email not a valid email...')
  }

  # check if site_id are valid site ID
  check_id <- amf_check_site_id(site_id)
  ## for multiple site ids
  if(length(site_id) > 1){
    if (any(!check_id)) {
      warning(paste(
        paste(site_id[which(!check_id)], collapse = ", "),
        "not valid AmeriFlux Site ID"
      ))
      site_id <- site_id[which(check_id)]

    }
  } else if (length(site_id) == 1){
    ## for single site id, need to work exception for AA-Flx, AA-Net
    if (check_id | site_id == "AA-Flx" | site_id == "AA-Net") {
      site_id <- site_id

    }else{
      site_id <- NULL

    }
  }
  if(length(site_id) == 0){
    stop("No valid Site ID in site_id...")
  }

  ## obtain formal intended use category
  intended_use_cat <- function(intended_use) {
    intended_use_verbse <- switch(
      intended_use,
      synthesis = "Research - Multi-site synthesis",
      remote_sensing = "Research - Remote sensing",
      model = "Research - Land model/Earth system model",
      other_research = "Research - Other",
      education = "Education (Teacher or Student)",
      other = "Other"
    )
    return(intended_use_verbse)
  }

  if(is.null(intended_use_cat(intended_use = intended_use))){
    stop("Invalid intended_use input...")
  }

  # check if out_dir reachable
  if(!dir.exists(out_dir)){
    stop("out_dir not valid...")
  }


  if (data_policy == "CCBY4.0") {
    cat("Data use guidelines for AmeriFlux CC-BY-4.0 Data Policy:\n",
        fill = TRUE)
    cat(
      paste0(
        "Data user is free to Share (copy and redistribute ",
        "the material in any medium or format) and/or Adapt ",
        "(remix, transform, and build upon the material) ",
        "for any purpose."
      ),
      fill = TRUE,
      labels = "(1)"
    )
    cat(
      paste0(
        "Provide a citation to each site data product ",
        "that includes the data-product DOI and/or recommended ",
        "publication."
      ),
      fill = TRUE,
      labels = "(2)"
    )
    cat(
      paste0(
        "Acknowledge funding for supporting AmeriFlux ",
        "data portal: U.S. Department of Energy Office ",
        "of Science.\n"
      ),
      fill = TRUE,
      labels = "(3)"
    )
    prompt <- paste0(
      "Please acknowledge you read and agree to the ",
      "AmeriFlux CC-BY-4.0 Data Policy."
    )
    agree_policy <- utils::menu(c("YES", "NO"), title = prompt) == 1

  } else if (data_policy == "LEGACY") {
    cat("Data use guidelines for AmeriFlux LEGACY License:\n",
        fill = TRUE)
    cat(
      paste0(
        "When you start in-depth analysis that may ",
        "result in a publication, contact the data ",
        "contributors directly, so that they have the ",
        "opportunity to contribute substantively and ",
        "become a co-author."
      ),
      fill = TRUE,
      labels = "(1)"
    )
    cat(
      paste0(
        "Provide a citation to each site data product that",
        " includes the data-product DOI."
      ),
      fill = TRUE,
      labels = "(2)"
    )
    cat(
      paste0(
        "Acknowledge funding for site support if it was ",
        "provided in the data download information."
      ),
      fill = TRUE,
      labels = "(3)"
    )
    cat(
      paste0(
        "Acknowledge funding for supporting AmeriFlux ",
        "data portal: U.S. Department of Energy Office ",
        "of Science.\n"
      ),
      fill = TRUE,
      labels = "(4)"
    )
    prompt <- paste0(
      "Please acknowledge you read and agree to the ",
      "AmeriFlux LEGACY Data Policy."
    )
    agree_policy <- utils::menu(c("YES", "NO"), title = prompt) == 1

  } else {
    stop("Need to specify a valid data policy before proceed...")

  }

  if (agree_policy) {

    #############################################################
    #  inform API this is a download test
    #  this is used only while code developments
    is_test <- FALSE

    #############################################################

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
        paste0(intended_use_text, " [amerifluxr download]"),
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
        for (i in seq_len(length(link$data_urls))) {
          ftplink <- c(ftplink,
                       link$data_urls[[i]]$url)
        }
      }

      ## check if any site_id has no data
      if (is.null(ftplink)) {
        stop(paste0("Cannot find data from ", site_id))
      }

      # avoid downloading fluxnet_bif for now
      if (length(site_id) == 1) {
        if (site_id == "AA-Flx" &
            data_policy == "CCBY4.0" &
            length(ftplink) > 1) {
          ftplink <- ftplink[-grep("FLUXNET-BIF", ftplink)]
        }
      }

      # get zip file names
      outfname <- strsplit(ftplink, c("/"))
      outfname <- sapply(outfname,  utils::tail, n = 1)
      outfname <-
        substr(outfname,
               1,
               sapply(outfname, regexpr, pattern = "?=", fixed = TRUE) - 1)

      ## check if any site_id has no data
      if (length(outfname) < length(site_id)) {
        miss_site_id <-
          site_id[which(!site_id %in% substr(outfname, 5, 10))]
        warning(paste0("Cannot find data from ", miss_site_id))
      }

      ## download sequentially
      output_zip_file <- file.path(out_dir, outfname)
      for (ii in seq_len(length(ftplink))) {
        utils::download.file(ftplink[ii],
                             output_zip_file[ii],
                             mode = "wb",
                             quiet = !verbose)
      }

      ## check if downloaded files exist
      miss_download <- which(!sapply(output_zip_file, file.exists))
      if (length(miss_download) > 0) {
        warning(paste("Cannot download",
                      output_zip_file[miss_download],
                      "from",
                      ftplink[miss_download]))
      }

    } else{
      stop("Data download fails, timeout or server error...")

      output_zip_file <- NULL
    }

  } else {
    stop("Need to acknowledge data policy before proceed")

    output_zip_file <- NULL
  }
  return(output_zip_file)
}
