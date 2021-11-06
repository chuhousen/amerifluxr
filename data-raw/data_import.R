# read in multi-site BADM file

amf_bif <- readxl::read_xlsx(file.path(getwd(),"data-original\\","AMF_AA-Flx_BIF_20201218.xlsx"))

amf_base <- utils::read.table(unz(paste0(getwd(), "\\data-original\\", "AMF_US-CRT_BASE-BADM_4-5.zip"), "AMF_US-CRT_BASE_HH_4-5.csv"),
                              sep=",", na=c("-9999"), header=T, skip=2)

usethis::use_data(amf_bif, amf_bif, compress = "xz")
usethis::use_data(amf_base, amf_base, compress = "xz")
