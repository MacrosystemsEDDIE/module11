# Enable bookmarking
enableBookmarking(store = "url")

# Load required libraries
suppressPackageStartupMessages(library(shiny, quietly = TRUE)); library(shinycssloaders)
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE, warn.conflicts = FALSE))
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE); library(shinyalert, quietly = TRUE, warn.conflicts = FALSE)
library(leaflet); library(htmltools); library(xml2); library(zoo); library(tsibble); library(fable);
suppressPackageStartupMessages(library(sf, quietly = TRUE, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE)); library(plotly, quietly = TRUE, warn.conflicts = FALSE)
library(ncdf4); library(reshape, quietly = TRUE, warn.conflicts = FALSE)
library(sortable)
# remotes::install_github('yonicd/slickR') # removed from CRAN - now only on GitHub
library(slickR); library(tinytex); library(rvest, quietly = TRUE, warn.conflicts = FALSE)
library(rLakeAnalyzer)
library(DT, quietly = TRUE, warn.conflicts = FALSE); library(rintrojs); library(hover)
library(stringr); library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(RColorBrewer); library(ggpubr); library(readr); library(shinyBS); library(httr);
library(tidyverse)

# colors for theme
obj_bg <- "#ccd9e0"
ques_bg <- "#cee3f1"
nav_bg <- "#DDE4E1"
nav_butt <- "#0d3658"
nav_txt <- "#fff" # white = #fff; black = #000000
slider_col <- "#446c84"

# Source required functions
source("./R/data_input_validator.R")

# Load module text
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)

# Read in assessment questions
quest <- read.csv("data/handout_questions.csv", row.names = 1)
answers <- quest
answers[, 1] <- NA

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Slides for slickR
recap_slides <- list.files("www/shiny_slides", full.names = TRUE)
arima_slides <- list.files("www/arima_slides", full.names = TRUE)
uc_slides <- list.files("www/prediction_uncertainty", full.names = TRUE)
ign_slides <- list.files("www/ignorance_score", full.names = TRUE)

# Create case study site dataframe
sites_df <- tibble(SiteID = c("cann"),
                   SiteName = c("Canning River"), 
                   SiteLocation = c("Perth, Australia"),
                   Latitude = c(-32.02115897791225), #currently Kent St Weir coords, needs updating
                   Longitude = c(115.92079680626996)) # currently Kent St Weir coords, needs updating

# Load case study data

# Canning River
cann_data <- read_csv("./data/cann.csv") # joined observational data at weekly timestep
cann_model_data <- read_csv("./data/cann_mod.csv") # interpolated data for model fitting

# Load variable descriptions
site_vars <- read.csv("data/site_variables.csv")


# Icons
siteIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28)
)

# Statistics for exploring data
stats <- list("Minimum" = "Min.", "Maximum" = "Max.", "Mean" = "Mean")

# Statistics table
stat_table <- data.frame(
  Variable = rep(NA, 5),
  Units = rep(NA, 5),
  Mean = rep(NA, 5),
  Min = rep(NA, 5),
  Max = rep(NA, 5)
)

# Variable relationships table
rel_table <- data.frame(
  Target = rep(NA, 5),
  Variable = rep(NA, 5),
  Relationship = rep(NA, 5)
)

# Model coefficients key
model_coeffs_key <- read_csv("./data/model_coeffs_key.csv")

# Data format table
data_format_table <- data.frame(
  a = c("site_id","character","name or abbreviated name of site where data were collected","cann"),
  b = c("datetime","Date","date of data collection in YYYY-MM-DD format","2023-01-23"),
  c = c("variable","character","name of variable with no spaces; recommended to include units","temp_degC"),
  d = c("observation","numeric","observed value of variable","4")
) %>%
  tibble::rownames_to_column("RowName") %>%
  mutate(RowName = c("Column Name","Format","Description","Example"))

# Tab names for updating buttons
tab_names <- read.csv("data/tab_names.csv", fileEncoding = "UTF-8-BOM")

# Add last update time
app_time <- format(file.info("ui.R")$mtime, "%Y-%m-%d")
web_time <- format(file.info("www/usanpn_eab.html")$mtime, "%Y-%m-%d")
app_update_txt <- paste0("This app was last updated on: ", app_time)

