#' BASE data example
#'
#' Continuous flux/met data (i.e., AmeriFlux BASE data product) for the
#' US-CRT site, as an example for demonstration. Also see AmeriFlux webpage
#' \url{https://ameriflux.lbl.gov/data/aboutdata/data-variables/#base} for
#' variable definitions and details.
#'
#' @format A data frame with 52608 rows and 36 variables:
#' \describe{
#'   \item{TIMESTAMP_START}{ISO timestamp start of averaging period (YYYYMMDDHHMM)}
#'   \item{TIMESTAMP_END}{ISO timestamp end of averaging period (YYYYMMDDHHMM)}
#'   \item{CO2}{Carbon Dioxide (CO2) mole fraction in wet air (µmolCO2 mol-1)}
#'   \item{H2O}{Water (H2O) vapor in mole fraction of wet air (mmolH2O mol-1)}
#'   \item{FC}{Carbon Dioxide (CO2) turbulent flux (µmolCO2 m-2 s-1)}
#'   \item{NEE_PI}{Net Ecosystem Exchange (µmolCO2 m-2 s-1)}
#'   \item{CH4}{Methane (CH4) mole fraction in wet air (nmolCH4 mol-1)}
#'   \item{FCH4}{Methane (CH4) turbulent flux (nmolCH4 m-2 s-1)}
#'   \item{H}{Sensible heat turbulent flux (W m-2)}
#'   \item{LE}{Latent heat turbulent flux (W m-2)}
#'   \item{G_1_1_1}{Soil heat flux at horizontal location #1 (W m-2)}
#'   \item{G_2_1_1}{Soil heat flux at horizontal location #2 (W m-2)}
#'   \item{WD}{Wind direction (Decimal degrees)}
#'   \item{WS}{Wind speed (m s-1)}
#'   \item{USTAR}{Friction velocity (m s-1)}
#'   \item{ZL}{data value}
#'   \item{MO_LENGTH}{Monin-Obukhov Stability parameter (nondimensional)}
#'   \item{W_SIGMA}{Standard deviation of vertical velocity fluctuations (m s-1)}
#'   \item{V_SIGMA}{Standard deviation of lateral velocity fluctuations (m s-1)}
#'   \item{U_SIGMA}{Standard deviation of along-wind velocity fluctuations (m s-1)}
#'   \item{T_SONIC}{Sonic temperature (deg C)}
#'   ...
#' }
#' @source \url{https://ameriflux.lbl.gov/}
"amf_base"


#' BADM data example
#'
#' The BADM file provides a description, general background, geo-location,
#' relevant publications and references for the site. The BADM files also
#' list what is measured at the site (this list may include chamber, gradient
#' or other eddy covariance measurements that are a superset of the data
#' available in AmeriFlux). Also see AmeriFlux webpage
#' \url{https://ameriflux.lbl.gov/data/aboutdata/badm-data-product/} for details.
#'
#' @format A data frame with 443 rows and 5 variables:
#' \describe{
#'   \item{SITE_ID}{6 digit AmeriFlux site ID (CC-Sss)}
#'   \item{GROUP_ID}{A unique identifier for data entries belonging to the same instance of a reported variable group}
#'   \item{VARIABLE_GROUP}{Define a set of variables that are reported together}
#'   \item{VARIABLE}{Variable names}
#'   \item{DATAVALUE}{Data value}
#'   ...
#' }
#' @source \url{https://ameriflux.lbl.gov/}
"amf_bif"

