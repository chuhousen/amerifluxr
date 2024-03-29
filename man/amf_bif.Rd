% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{amf_bif}
\alias{amf_bif}
\title{BADM data example}
\format{
A data frame with 443 rows and 5 variables
}
\source{
\url{https://ameriflux.lbl.gov/}
}
\usage{
amf_bif
}
\description{
The BADM file provides a description, general background, geo-location,
relevant publications and references for the site. The BADM files also
list what is measured at the site (this list may include chamber, gradient
or other eddy covariance measurements that are a super-set of the data
available in AmeriFlux). Also see AmeriFlux web page
\url{https://ameriflux.lbl.gov/data/aboutdata/badm-data-product/}
for details.
}
\details{
\itemize{
  \item SITE_ID - 6 digit AmeriFlux site ID (CC-Sss)
  \item GROUP_ID - A unique identifier for data entries belonging to the same
   instance of a reported variable group
  \item VARIABLE_GROUP - Define a set of variables that are reported together
  \item VARIABLE - Variable names
  \item DATAVALUE - Data value
}
}
\keyword{datasets}
