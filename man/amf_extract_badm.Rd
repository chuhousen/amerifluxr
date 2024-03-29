% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amf_extract_badm.R
\name{amf_extract_badm}
\alias{amf_extract_badm}
\title{Extract BADM data of a specific BADM group}
\usage{
amf_extract_badm(bif_data, select_group)
}
\arguments{
\item{bif_data}{A data frame consists of 5 columns: SITE_ID, GROUP_ID,
VARIABLE_GROUP, VARIABLE, DATAVALUE, imported from function
\code{\link{amf_read_bif}}.}

\item{select_group}{A string (character), selected from VARIABLE_GROUP
in the \code{bif_data}}
}
\value{
A data frame of re-structured BADM data with the following columns:
\itemize{
  \item GROUP_ID - A unique identifier for data belonging to the same
  instance of a reported variable group
  \item SITE_ID - Six character site identifier (CC-Sss)
  \item VALUE - Values for all available VARIABLES in the selected group
  \item ...
}
}
\description{
This function extracts BADM data of a specific BADM group
from the imported BADM (BIF) file. Use function \code{\link{amf_read_bif}}
to import BADM (BIF) file.
}
\examples{
# read the BADM BIF file, using an example data file
bif <- amf_read_bif(file = system.file("extdata",
                                       "AMF_AA-Flx_BIF_CCBY4_20201218.xlsx",
                                        package = "amerifluxr"))

# get a list of valid VARIALBE_GROUP
unique(bif$VARIABLE_GROUP)

# extract the selected VARIALBE_GROUP
amf_extract_badm(bif_data = bif, select_group = "GRP_FLUX_MEASUREMENTS")
amf_extract_badm(bif_data = bif, select_group = "GRP_IGBP")
}
\seealso{
\code{\link{amf_read_bif}}
}
