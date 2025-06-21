install.packages("aws.s3")

library(tidyverse)
library(lubridate)

source('fct_awss3Connect_sensorcode.R')
source('fct_awss3Connect.R')
#source('collect_insitu_targets.R')
source('collect_profile_targets.R')
source('keys.R')

sensorcode_df <- read_csv('./sensorcode.csv', show_col_types = FALSE)

# Inflow Targets
print('Generating Inflow Targets')
source('generate_inflow_targets.R')

