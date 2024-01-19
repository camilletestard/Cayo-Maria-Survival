# Script to compile Cayo 2018 and 2019 thermochron data 

# load libraries --------
library(readr)
library(tidyverse)
library(googledrive)

# Load thermochron data --------
read.plus <- function(flnm) {
  read.csv(flnm, header = T, skip = 14, stringsAsFactors = FALSE) %>% 
    mutate(filename = flnm)
} # read function and add file name to identify each row

therm_data_2018 <- list.files(path = "~/Dropbox/THERMOCHRONS/2018_Thermochron_Data/2018_Thermochron_Uploads/", recursive = T, pattern = "*.csv", full.names = T) %>% 
  map_df(~ read.plus(.)) 

therm_data_2019 <- list.files(path = "~/Dropbox/THERMOCHRONS/2019_Thermochron_Data/2019_Thermochron_Uploads/", recursive = T, pattern = "*.csv", full.names = T) %>% 
  map_df(~ read.plus(.))

#therm_data_2020 <- drive_ls(path = "~/usr/local/lib/R/site-library/googledrive", recursive = T, pattern = "*.csv", full.names = T) %>% 
  map_df(~ read.plus(.))

# Compile thermochron data -----------
# Combine 2018, 2019, 2020 data 
temp_data_tmp <- rbind(therm_data_2018, therm_data_2019); rm(therm_data_2018, therm_data_2019) # add 2020 when have it 

# Extract thermochron and deployment number from file name identifier - Thermochron data files must be named correctly 
temp_data <- temp_data_tmp %>% 
  mutate(deploy_and_therm = str_replace(filename, "^.*//", "")) %>% 
  mutate(deploy_info = str_split_fixed(deploy_and_therm, "/", n = Inf)[,1]) %>% 
  mutate(therm_info = str_split_fixed(deploy_and_therm, "/", n = Inf)[,2]) %>% 
  mutate(therm_info = str_replace(therm_info, "Therm_", "")) %>% 
  mutate(ThermochronNumber = str_replace(therm_info, "_.*$", "")) %>% 
  mutate(DeploymentNumber = str_replace(deploy_info, "^.*Deploy", "")) %>%
  dplyr::select(-c(filename, deploy_and_therm, deploy_info, therm_info)) %>%
  mutate(DeploymentNumber = as.factor(as.numeric(DeploymentNumber))) %>% 
  arrange(DeploymentNumber, ThermochronNumber)
rm(temp_data_tmp)

#check that all thermochrons and deployments are represented and strings seperated correctly
table(temp_data$ThermochronNumber, temp_data$DeploymentNumber) 


# Format the file for Dropbox with proper header --------
thermochron_header <- read_xlsx(path = "~/Dropbox/THERMOCHRONS/THERMOCRON.DATA_COMPILED_Example.xlsx", col_names = T, range = cell_rows(1:15)) 

colnames(thermochron_header) <- c("Date.Time",	"Unit",	"Value",	"ThermochronNumber",	"DeploymentNumber", "Notes") # set colnames to match temp data 

thermochron_header[15,] <- "" # add space between header and thermochron data 

# Make Notes column on the compiled thermochron data so columns match 
temp_data$Notes <- "" 

# bind header to thermochron data 
Thermochron_data_compiled <- rbind(thermochron_header, temp_data); rm(thermochron_header)


# Save --------
#write.csv(x = Thermochron_data_compiled, file = "THERMOCHRON.DATA_COMPILED.csv", row.names = F) # change file path as needed 

# Save the compiled temperature data to use with thermochron plotting and stats script
setwd("~/Dropbox/THERMOCHRONS")
save(temp_data, Thermochron_data_compiled, file = "THERMOCHRON.DATA_COMPILED.RData")

