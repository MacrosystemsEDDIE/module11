# NEON NEE data workflow
# Author: Mary Lofton
# Date: 01AUG25

library(tidyverse)
library(lubridate)
library(neonUtilities)

url_P1D <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/terrestrial_daily-targets.csv.gz"

terrestrial_targets <- read_csv(url_P1D, show_col_types = FALSE)

terrestrial_focal_sites <- c("BART","KONZ")

terrestrial_targets |> 
  filter(site_id %in% terrestrial_focal_sites & variable == "nee") |> 
  ggplot(aes(x = datetime, y = observation)) +
  geom_point() +
  facet_wrap(facets = vars(site_id), scales = "free_y") +
  theme_bw()

nee_df <- terrestrial_targets %>%
  filter(site_id %in% terrestrial_focal_sites & variable == "nee" & year(datetime) %in% c(2019:2024)) %>%
  select(-c(project_id, duration))
  
#### Air temperature as a covariate (seems reasonable)

########### Air temp 

#Using this data product for air temp: https://data.neonscience.org/data-products/DP1.00003.001

source("./dev_files/NEON_TOKEN.R")

at <- loadByProduct(dpID="DP1.00003.001", site=c("BART","KONZ"),
                    startdate="2019-01", enddate="2024-12", 
                    package="basic",
                    tabl="TAAT_30min",
                    token = NEON_TOKEN,
                    check.size = F)

# unlist the variables and add to the global environment
list2env(at, .GlobalEnv)

#format data
at2 <- TAAT_30min %>%
  select(siteID, startDateTime, tempTripleMean) %>%
  mutate(datetime = date(startDateTime)) %>%
  rename(site_id = siteID) %>%
  group_by(site_id, datetime) %>%
  summarize(observation = mean(tempTripleMean, na.rm = TRUE)) %>%
  add_column(variable = "temp") %>%
  arrange(site_id, datetime)

#plot formatted data
ggplot(data = at2, aes(x = datetime, y = observation))+
  geom_point()+
  facet_wrap(vars(site_id), scales = "free", nrow = 2)+
  theme_bw()

######### Join targets and predictors, write to file as intermediate prior to processing for module
nee <- bind_rows(nee_df, at2)
write.csv(nee, "./dev_files/neon_nee.csv", row.names = FALSE)


####### Make files for module; BART first 

nee <- read_csv("./dev_files/neon_nee.csv")

bart <- nee %>%
  filter(site_id == "BART") %>%
  pivot_wider(names_from = "variable", values_from = "observation") %>%
  select(-site_id)

ggplot(data = bart, aes(x = datetime, y = nee))+
  geom_point()

write.csv(bart, "./app/data/bart.csv", row.names = FALSE)

bart_mod <- bart %>%
  arrange(datetime) %>%
  mutate(datetime = date(datetime)) %>%
  as_tsibble(.) %>%
  tsibble::fill_gaps() %>%
  fill(nee:temp, .direction = "updown")

check <- bart_mod %>%
  filter(!complete.cases(.))

ggplot(data = bart_mod, aes(x = datetime, y = nee))+
  geom_point()

write.csv(bart_mod, "./app/data/bart_mod.csv", row.names = FALSE)