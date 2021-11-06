#' Parse variable name and qualifier in the AmeriFlux BASE data product
#'
#' @description This function parse variable names and qualifiers of AmeriFlux BASE data product. See AmeriFlux web page
#' \url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/} about the details of variable naming and qualifiers.
#' @param var_name A vector of variable names (character) to be parsed
#' @param FP_ls A vector of standard variable names. If not specified, use \code{\link{amf_variables}} to get the latest list.
#' @param gapfill_postfix Expected suffixes appended to variable being gap-filled. The default is "_PI_F".
#'
#' @return A data frame containing the parsed results for all variables in var_name:
#' \itemize{
#'   \item variable_name - original variable name
#'   \item basename - associated basename, w/o qualifier
#'   \item qualifier_gf - qualifier associated with gap-filling
#'   \item qualifier_pi - qualifier associated with PI version, excluding gap-filling
#'   \item qualifier_pos - qualifier associated with position
#'   \item qualifier_ag - qualifier associated with layer-aggregation, e.g., _N, _SD
#'   \item layer_index - layer index provided, if any
#'   \item H_index - H index provided, if any
#'   \item V_index - V index provided, if any
#'   \item R_index - R index provided, if any
#'   \item is_correct_basename - is the parsed basename recognized in FP-Standard
#'   \item is_pi_provide - is this a PI provided variable e.g., _PI
#'   \item is_gapfill - is this a gap-filled variable, _PF_F or _F
#'   \item is_fetch - is this a fetch quantile variable, e.g., FETCH_70...FETCH_90
#'   \item is_layer_aggregated - is this a layer-integrated var, i.e., _#
#'   \item is_layer_SD - is this a standard deviation of layer-integrated var, i.e., spatial variability
#'   \item is_layer_number - is this a number of samples of layer-integrated var, i.e., spatial variability
#'   \item is_replicate_aggregated - is this a replicate-averaged var, e.g., _1_1_A
#'   \item is_replicate_SD - is this a standard deviation of replicate-averaged var, e.g., _1_1_A_SD
#'   \item is_replicate_number - is this a number of samples of replicate-averaged var, e.g., _1_1_A_N
#'   \item is_quadruplet - is this a quadruplet, e.g., _1_1_1
#' }
#' @export
#' @seealso amf_read_base, amf_variables, amf_data_aval, amf_var_info
#' @examples
#' ## Not run:
#' # read the BASE from a csv file
#' base <- amf_read_base(file = system.file("extdata",
#'                                          "AMF_US-CRT_BASE_HH_2-5.csv",
#'                                           package = "amerifluxr"),
#'                       unzip = FALSE,
#'                       parse_timestamp = FALSE)
#'
#' # parse variable names/qualifiers
#' basename_decode <- amf_parse_basename(var_name = colnames(base))
#'
#' ## End(Not run)
#'
amf_parse_basename <- function(var_name,
                               FP_ls = NULL,
                               gapfill_postfix = "_PI_F") {

  # stop if missing var_name parameter
  if (missing(var_name)) {
    stop('var_name not specified...')
  }

  # stop if var_name parameter not character
  if (!is.character(var_name)) {
    stop('var_name not recognized...')
  }

  # IF FP_ls not specified, use amf_variables() by default
  if(is.null(FP_ls)){
    FP_ls <- amerifluxr::amf_variables()[, c("Name")]
  }

  # stop if FP_ls parameter not character
  if (!is.character(FP_ls)) {
    stop('FP_ls not recognized...')
  }

  # a data frame for parsing results
  basename_decode <- data.frame(variable_name = var_name,   # original variable name
                                working_names = NA,         # working variable name, dropped in output
                                basename = NA,              # associated basename, w/o qualifier
                                qualifier_gf = NA,          # qualifier associated with gap-filling
                                qualifier_pi = NA,          # qualifier associated with PI version, excluding gap-filling
                                qualifier_pos = NA,         # qualifier associated with position
                                qualifier_ag = NA,          # qualifier associated with layer-aggregation, e.g., _N, _SD
                                layer_index = NA,           # layer index provided, if any
                                H_index = NA,               # H index provided, if any
                                V_index = NA,               # V index provided, if any
                                R_index = NA,               # R index provided, if any
                                is_correct_basename = NA,   # is the parsed basename recognized in FP-Standard
                                is_pi_provide = NA,         # is this a PI provided variable e.g., _PI
                                is_gapfill = NA,            # is this a gap-filled variable, _PF_F or _F
                                is_fetch = NA,              # is this a fetch quantile variable, e.g., FETCH_70...FETCH_90
                                is_layer_aggregated = NA,   # is this a layer-integrated var, i.e., _#
                                is_layer_SD = NA,           # is this a standard deviation of layer-integrated var, i.e., spatial variability
                                is_layer_number = NA,       # is this a number of samples of layer-integrated var, i.e., spatial variability
                                is_replicate_aggregated = NA,  # is this a replicate-averaged var, i.e., _<H>_<V>_A
                                is_replicate_SD = NA,       # is this a standard deviation of replicate-averaged var, i.e., _<H>_<V>_A_SD
                                is_replicate_number = NA,   # is this a number of samples of replicate-averaged var, i.e., _<H>_<V>_A_N
                                is_quadruplet = NA,         # is this a quadruplet, i.e., _<H>_<V>_<R>
                                stringsAsFactors = FALSE)

  #####################################################################
  ## locate gap-filled variables
  basename_decode$is_gapfill <- grepl(paste0("(", gapfill_postfix, "$|", gapfill_postfix, "_)"),
                                      basename_decode$variable_name,
                                      perl = TRUE)
  basename_decode$qualifier_gf <- ifelse(basename_decode$is_gapfill,
                                         gapfill_postfix,
                                         NA)
  basename_decode$working_names <- ifelse(basename_decode$is_gapfill,
                                          sub(gapfill_postfix,
                                              "",
                                              basename_decode$variable_name),
                                          basename_decode$variable_name)

  ## locate PI provided (_PI) variables
  basename_decode$is_PI_provide <- grepl("_PI",
                                         basename_decode$working_names,
                                         perl = TRUE)
  basename_decode$working_names <- sub("_PI",
                                       "",
                                       basename_decode$working_names)
  basename_decode$qualifier_pi <- ifelse(basename_decode$is_PI_provide,
                                         "_PI",
                                         NA)

  ## locate footprint fetch FETCH_70, _80, _90
  basename_decode$is_fetch <- grepl("FETCH_[[:digit:]]+",
                                    basename_decode$working_names,
                                    perl = TRUE)

  #######################################################################
  #### work on quadruplet _H_V_R
  ## locate quadruplet _H_V_R
  basename_decode$is_quadruplet <- (!basename_decode$is_fetch &
                                      grepl("_[[:digit:]]+_[[:digit:]]+_[[:digit:]]+",
                                            basename_decode$working_names,
                                            perl = TRUE)) | grepl(
                                              "FETCH_[[:digit:]]+_[[:digit:]]+_[[:digit:]]+_[[:digit:]]+",
                                              basename_decode$working_names,
                                              perl = TRUE)

  basename_decode$qualifier_pos <- ifelse(basename_decode$is_quadruplet,
                                          substr(basename_decode$working_names,
                                                 start = regexpr("_[[:digit:]]+_[[:digit:]]+_[[:digit:]]+",
                                                                 basename_decode$working_names,
                                                                 perl = TRUE) + ifelse(basename_decode$is_fetch, 3, 0),
                                                 stop = nchar(basename_decode$working_names)),
                                          basename_decode$qualifier_pos)

  # parse position qualifier
  for (i1 in seq_len(nrow(basename_decode))) {
    if (basename_decode$is_quadruplet[i1]) {
      basename_decode[i1, c("H_index", "V_index", "R_index")] <-
        Numextract(basename_decode$qualifier_pos[i1])
    }
  }

  ########################################################################
  #### work on replicate aggregated _H_V_A, _H_V_A_SD, _H_V_A_N
  ## find replicate aggregated SD
  basename_decode$is_replicate_SD <- grepl("_[[:digit:]]+_[[:digit:]]+_A_SD",
                                           basename_decode$working_names,
                                           perl = TRUE)

  basename_decode$qualifier_pos <- ifelse(basename_decode$is_replicate_SD,
                                          substr(basename_decode$working_names,
                                                 start = regexpr("_[[:digit:]]+_[[:digit:]]+_A_SD",
                                                                 basename_decode$working_names,
                                                                 perl = TRUE),
                                                 stop = regexpr("_SD",
                                                                basename_decode$working_names,
                                                                perl = TRUE) - 1),
                                          basename_decode$qualifier_pos)
  # parse position qualifier
  for (i2 in seq_len(nrow(basename_decode))) {
    if (basename_decode$is_replicate_SD[i2]) {
      basename_decode[i2, c("H_index", "V_index", "R_index")] <-
        c(Numextract(basename_decode$qualifier_pos[i2]), "A")
      basename_decode[i2, c("qualifier_ag")] <- c("_SD")
    }
  }

  ## locate replicate aggregated N
  basename_decode$is_replicate_number <- grepl("_[[:digit:]]+_[[:digit:]]+_A_N",
                                               basename_decode$working_names,
                                               perl = TRUE)

  basename_decode$qualifier_pos <- ifelse(basename_decode$is_replicate_number,
                                          substr(basename_decode$working_names,
                                                 start = regexpr("_[[:digit:]]+_[[:digit:]]+_A_N",
                                                                 basename_decode$working_names,
                                                                 perl = TRUE),
                                                 stop = regexpr("_N",
                                                                basename_decode$working_names,
                                                                perl = TRUE) - 1),
                                          basename_decode$qualifier_pos)
  # parse position qualifier
  for (i3 in seq_len(nrow(basename_decode))) {
    if (basename_decode$is_replicate_number[i3]) {
      basename_decode[i3, c("H_index", "V_index", "R_index")] <-
        c(Numextract(basename_decode$qualifier_pos[i3]), "A")
      basename_decode[i3, c("qualifier_ag")] <- c("_N")
    }
  }

  ## find replicate aggregated
  basename_decode$is_replicate_aggregated <- (grepl("_[[:digit:]]+_[[:digit:]]+_A",
                                                 basename_decode$working_names,
                                                 perl = TRUE) &
                                             !basename_decode$is_replicate_number &
                                             !basename_decode$is_replicate_SD)

  basename_decode$qualifier_pos <- ifelse(basename_decode$is_replicate_aggregated,
                                          substr(basename_decode$working_names,
                                                 start = regexpr("_[[:digit:]]+_[[:digit:]]+_A",
                                                                 basename_decode$working_names,
                                                                 perl = TRUE),
                                                 stop = nchar(basename_decode$working_names)),
                                          basename_decode$qualifier_pos)
  for (i4 in seq_len(nrow(basename_decode))) {
    if (basename_decode$is_replicate_aggregated[i4]) {
      basename_decode[i4, c("H_index", "V_index", "R_index")] <-
        c(Numextract(basename_decode$qualifier_pos[i4]), "A")
    }
  }

  ##################################################################################################
  #### work on layer aggregated variable, _#, _#_SD, _#_N
  ## locate layer aggregated SD
  basename_decode$is_layer_SD <- grepl("_[[:digit:]]+_SD",
                                       basename_decode$working_names,
                                       perl = TRUE)

  basename_decode$qualifier_pos <- ifelse(basename_decode$is_layer_SD,
                                          substr(basename_decode$working_names,
                                                 start = regexpr("_[[:digit:]]+_SD",
                                                                 basename_decode$working_names,
                                                                 perl = TRUE),
                                                 stop = regexpr("_SD",
                                                                basename_decode$working_names,
                                                                perl = TRUE) - 1),
                                          basename_decode$qualifier_pos)
  # parse qualifier
  for (i5 in seq_len(nrow(basename_decode))) {
    if (basename_decode$is_layer_SD[i5]) {
      basename_decode[i5, c("layer_index")] <-
        c(Numextract(basename_decode$qualifier_pos[i5]))
      basename_decode[i5, c("qualifier_ag")] <- c("_SD")
    }
  }

  ## locate layer aggregated Number
  basename_decode$is_layer_number <- grepl("_[[:digit:]]+_N",
                                           basename_decode$working_names,
                                           perl = TRUE)

  basename_decode$qualifier_pos <- ifelse(basename_decode$is_layer_number,
                                          substr(basename_decode$working_names,
                                                 start = regexpr("_[[:digit:]]+_N",
                                                                 basename_decode$working_names,
                                                                 perl = TRUE),
                                                 stop = regexpr("_N",
                                                                basename_decode$working_names,
                                                                perl = TRUE) - 1),
                                          basename_decode$qualifier_pos)
  # parse position qualifier
  for (i6 in seq_len(nrow(basename_decode))) {
    if (basename_decode$is_layer_number[i6]) {
      basename_decode[i6, c("layer_index")] <-
        c(Numextract(basename_decode$qualifier_pos[i6]))
      basename_decode[i6, c("qualifier_ag")] <- c("_N")
    }
  }

  ## locate layer aggregated variables
  basename_decode$is_layer_aggregated <- (grepl("_[[:digit:]]+",
                                               basename_decode$working_names,
                                               perl = TRUE) &
                                           !basename_decode$is_fetch &
                                           !basename_decode$is_quadruplet &
                                           !basename_decode$is_replicate_aggregated &
                                           !basename_decode$is_replicate_number &
                                           !basename_decode$is_replicate_SD &
                                           !basename_decode$is_layer_SD &
                                           !basename_decode$is_layer_number
  ) | (grepl("FETCH_[[:digit:]]+_[[:digit:]]+",
             basename_decode$working_names,
             perl = TRUE) &
         basename_decode$is_fetch &
         !basename_decode$is_quadruplet &
         !basename_decode$is_replicate_aggregated &
         !basename_decode$is_replicate_number &
         !basename_decode$is_replicate_SD &
         !basename_decode$is_layer_SD &
         !basename_decode$is_layer_number)

  basename_decode$qualifier_pos <- ifelse(basename_decode$is_layer_aggregated,
                                          substr(basename_decode$working_names,
                                                 start = regexpr("_[[:digit:]]+",
                                                                 basename_decode$working_names,
                                                                 perl = TRUE) + ifelse(basename_decode$is_fetch, 3, 0),
                                                 stop = nchar(basename_decode$working_names)),
                                          basename_decode$qualifier_pos)

  # parse position qualifier
  for (i7 in seq_len(nrow(basename_decode))) {
    if (basename_decode$is_layer_aggregated[i7]) {
      basename_decode[i7, c("layer_index")] <-
        c(Numextract(basename_decode$qualifier_pos[i7]))
    }
  }

  ############################################################################################
  ## parse basename, w/o all qualifiers
  basename_decode$basename <- basename_decode$working_names
  for (i8 in seq_len(nrow(basename_decode))) {
    if (!is.na(basename_decode$qualifier_pos[i8])) {
      basename_decode$basename[i8] <- sub(basename_decode$qualifier_pos[i8],
                                          "",
                                          basename_decode$working_names[i8])
    }
    if (!is.na(basename_decode$qualifier_ag[i8])) {
      basename_decode$basename[i8] <- sub(paste0(basename_decode$qualifier_pos[i8],
                                                 basename_decode$qualifier_ag[i8]),
                                          "",
                                          basename_decode$working_names[i8])
    }
  }

  ## check if parsed basename present in FP_ls
  for (i9 in seq_len(nrow(basename_decode))) {
    basename_decode$is_correct_basename[i9] <- ifelse(length(which(
      FP_ls == paste(basename_decode$basename[i9])
    )) == 1,
    TRUE, FALSE)
  }

  ## re-order to follow input order
  basename_decode <- merge.data.frame(x = data.frame(variable_name = var_name,
                                                     stringsAsFactors = FALSE),
                                      y = basename_decode[, -which(colnames(basename_decode) == "working_names")],
                                      by = "variable_name",
                                      sort = FALSE)

  return(basename_decode)
}



