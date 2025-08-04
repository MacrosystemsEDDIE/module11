# NEON NEE data workflow
# Author: Mary Lofton
# Date: 01AUG25

library(tidyverse)
library(lubridate)
library(neonUtilities)
library(tsibble)

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
  dplyr::rename(site_id = siteID) %>%
  group_by(site_id, datetime) %>%
  summarize(observation = mean(tempTripleMean, na.rm = TRUE)) %>%
  add_column(variable = "temp") %>%
  arrange(site_id, datetime)

#plot formatted data
ggplot(data = at2, aes(x = datetime, y = observation))+
  geom_point()+
  facet_wrap(vars(site_id), scales = "free", nrow = 2)+
  theme_bw()

########### Relative humidity (to calculate vapor pressure deficit)

#Using this data product for air temp: https://data.neonscience.org/data-products/DP1.00098.001

source("./dev_files/NEON_TOKEN.R")

rh <- loadByProduct(dpID="DP1.00098.001", site=c("BART","KONZ"),
                    startdate="2019-01", enddate="2024-12", 
                    package="basic",
                    tabl="RH_30min",
                    token = NEON_TOKEN,
                    check.size = F)

# unlist the variables and add to the global environment
list2env(rh, .GlobalEnv)

#format data
rh2 <- RH_30min %>%
  select(siteID, startDateTime, tempRHMean, dewTempMean) %>%
  mutate(es = ifelse(tempRHMean > 0, 0.61078 * exp((17.27 * tempRHMean) / (tempRHMean + 237.3)), 0.61078 * exp((21.875 * tempRHMean) / (tempRHMean + 265.5))),
         ea = ifelse(dewTempMean > 0, 0.61078 * exp((17.27 * dewTempMean) / (dewTempMean + 237.3)), 0.61078 * exp((21.875 * dewTempMean) / (dewTempMean + 265.5)))) %>%
  mutate(vpd = es - ea) %>%
  mutate(datetime = date(startDateTime)) %>%
  dplyr::rename(site_id = siteID) %>%
  group_by(site_id, datetime) %>%
  summarize(observation = mean(vpd, na.rm = TRUE)) %>%
  add_column(variable = "vpd") %>% # believe this is kPa
  arrange(site_id, datetime) %>%
  mutate(observation = ifelse(observation > 1000, NA, observation)) # dealing with KONZ outlier

#plot formatted data
ggplot(data = rh2, aes(x = datetime, y = observation))+
  geom_point()+
  facet_wrap(vars(site_id), scales = "free", nrow = 2)+
  theme_bw()


########### Shortwave 

#Using this data product for shortwave: https://data.neonscience.org/data-products/DP1.00023.001

sw <- loadByProduct(dpID="DP1.00023.001", site=c("BART","KONZ"),
                    startdate="2019-01", enddate="2024-12", 
                    package="basic",
                    tabl="SLRNR_30min",
                    token = NEON_TOKEN,
                    check.size = F)

# unlist the variables and add to the global environment
list2env(sw, .GlobalEnv)

#format data
sw2 <- SLRNR_30min %>%
  select(siteID, startDateTime, inSWMean) %>%
  mutate(datetime = date(startDateTime)) %>%
  dplyr::rename(site_id = siteID) %>%
  group_by(site_id, datetime) %>%
  summarize(observation = mean(inSWMean, na.rm = TRUE)) %>%
  add_column(variable = "sw") %>% 
  arrange(site_id, datetime) %>%
  #inserting some QC here for super-high values
  mutate(observation = ifelse(observation > 400 | (observation > 3*mean(observation, na.rm = TRUE)),NA,observation))

#plot formatted data
ggplot(data = sw2, aes(x = datetime, y = sw))+
  geom_point()+
  facet_wrap(vars(site_id), scales = "free", nrow = 2)+
  theme_bw()




######### Join targets and predictors, write to file as intermediate prior to processing for module
nee <- bind_rows(nee_df, at2) %>%
  bind_rows(., rh2) %>%
  bind_rows(., sw2)

ggplot(data = nee, aes(x = datetime, y = observation))+
  geom_point()+
  facet_wrap(site_id ~ variable, scales = "free_y")+
  theme_bw()

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
  fill(nee:sw, .direction = "updown")

check <- bart_mod %>%
  filter(!complete.cases(.))

ggplot(data = bart_mod, aes(x = datetime, y = nee))+
  geom_point()

write.csv(bart_mod, "./app/data/bart_mod.csv", row.names = FALSE)

####### now KONZ 

nee <- read_csv("./dev_files/neon_nee.csv")

konz <- nee %>%
  filter(site_id == "KONZ") %>%
  pivot_wider(names_from = "variable", values_from = "observation") %>%
  select(-site_id)

ggplot(data = konz, aes(x = datetime, y = nee))+
  geom_point()

write.csv(konz, "./app/data/konz.csv", row.names = FALSE)

konz_std <- konz %>%
  arrange(datetime) %>%
  mutate(datetime = date(datetime)) %>%
  filter(year(datetime) %in% c(2019:2023)) %>%
  as_tsibble(.) %>%
  tsibble::fill_gaps() %>%
  fill(nee:sw, .direction = "updown") %>%
  pivot_longer(nee:sw, names_to = "variable", values_to = "observation") %>%
  mutate(site_id = "konz") %>%
  select(site_id, datetime, variable, observation)

ggplot(data = konz_std, aes(x = datetime, y = observation)) +
  geom_point() +
  facet_wrap(vars(variable), scales = "free_y") +
  theme_bw()

check <- konz_std %>%
  filter(!complete.cases(.))

write.csv(konz_std, "./app/data/konz_std.csv", row.names = FALSE)

konz_gotcha <- konz_std %>%
  mutate(variable = ifelse(variable == "nee","net ecosystem exchange",
                           ifelse(variable == "temp","air temperaure",
                                  ifelse(variable == "vpd","vapor pressure deficit",
                                         ifelse(variable == "sw","shortwave radiation",variable)))))
write.csv(konz_gotcha, "./app/data/konz_download.csv",row.names = FALSE)
