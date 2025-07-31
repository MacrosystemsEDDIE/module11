#' awss3Connect 
#'
#' @description Establishes connection to the S3 bucket and fetches data 
#'
#' @return The return value, if any, from executing the function.
#' @import aws.s3 readr
#' @noRd


library('aws.s3')
library(dplyr)

# To enforce HTTPS, should be set to TRUE
Sys.setenv('USE_HTTPS' = TRUE)

# Set details for bucket origin
Sys.setenv(
  'AWS_DEFAULT_REGION' = '', 
  'AWS_S3_ENDPOINT' = 'projects.pawsey.org.au', 
  'AWS_ACCESS_KEY_ID' = '2f1a9d81bdf24a178b2bd18d530e959b', 
  'AWS_SECRET_ACCESS_KEY' = 'e062073c1faf488cb4209ba8de2eb483'
)


awss3Connect <- function(filename){
  
  # Now set bucket contents as objects
  bucket <- 'scevo-data'

  #filename = 'data-warehouse/dbca/wiski/DBCA_data_export_2023-07-19_1615.csv'
  # fetchedData <- aws.s3::s3read_using(FUN = utils::read.csv,
  #                                     check.names = FALSE,
  #                                     encoding = "UTF-8",
  #                                # show_col_types = FALSE,
  #                                # lazy = FALSE,
  #                                # progress = FALSE,
  #                                object = filename,
  #                                bucket = bucket,
  #                                filename = basename(filename),
  #                                opts = list(
  #                                  base_url = "projects.pawsey.org.au",
  #                                  region = ""))
  
  
  fetchedData <- aws.s3::s3read_using(FUN = utils::read.csv,
                                      check.names = FALSE,
                                      encoding = "UTF-8",
                                      # show_col_types = FALSE,
                                      # lazy = FALSE,
                                      # progress = FALSE,
                                      object = filename,
                                      bucket = bucket,
                                      filename = basename(filename),
                                      opts = list(
                                        base_url = "projects.pawsey.org.au",
                                        region = "",
                                        key = "2f1a9d81bdf24a178b2bd18d530e959b",
                                        secret = "e062073c1faf488cb4209ba8de2eb483"))

 
  return(fetchedData)
}
  

# rawwiski <- awss3Connect(filename = 'data-warehouse/dbca/wiski/wiski_30072025_merged.csv')
# cannsites <- c('BAC','KEN')
# samp_data <- rawwiski %>%  
#   dplyr::filter(`Program Site Ref` %in% cannsites) %>%
#    select(
#     `Program Site Ref`, `Collect Time`, `Collect Date`, `Sample Depth(s) (m)`,
#     `Salinity (ppt)`, `Temperature (deg C)`, `O2-{DO conc} (mg/L)`, `pH (no units)`,
#     `Chlorophyll a (in situ) (ug/L)`,  `Chlorophyll a (by vol) (mg/L)`, `Turbidity (NTU)`, `Wind speed (knot)`,
#     `P (tot) {TP pTP} (ug/L)`, `N (tot) {TN pTN} (ug/L)`,  `PO4-P (sol react) {SRP FRP} (ug/L)`, `NH3-N/NH4-N (sol) (ug/L)`,
#     `C (sol org) {DOC DOC as NPOC} (ug/L)`, `N (sum sol org) {DON} (ug/L)`)
# 

