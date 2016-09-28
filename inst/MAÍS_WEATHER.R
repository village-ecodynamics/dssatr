# This script imports and manipulates the MAÍS data
# to conform to the "genetic coefficients" required to
# run the CSM-CERES-Maize model in DSSAT. Those coefficients
# are as follows:

# P1: Thermal time (GDD with 8C base) from seedling emergence to end of juvenile phase
# P2: Developmental delay (days) for each hour increase in photoperiod about 12.5
# P5: Thermal time from silking to physiological maturity
# G2: Maximum possible kernels per plant
# G3: Kernel filling rate under optimal conditions (mg/day)
# PHINT: Phylochron interval—interval in thermal time between successive leaf tip appearances

# Load some libraries we will use
library(data.table)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(FedData)

setwd("~/Desktop/MAÍS/R")

# MAÍS data were collected at two research stations:
# The New Mexico State Agricultural Research Station near Farmington, NM, and
# North Central Regional Plant Introduction Station near Ames, Iowa
# We will use get_ghcn_daily_station() from FedData to download daily GHCN data for each site.
# NM_ARS.weather <- get_ghcn_daily_station(ID = 'USC00293142', raw.dir = "../DATA/GHCN")
# NCRPIS.weather <- get_ghcn_daily_station(ID = 'USW00094989', raw.dir = "../DATA/GHCN")

## MAÍS data are in the ../DATA/MAÍS directory, which contains mainly Excel
## spreadsheets provided by Jon Sandor (jasandor@iastate.edu)
# First, read key-value sets for accessions and trials
accessions <- read_csv("../DATA/MAÍS/accessions.csv")
trials <- read_csv("../DATA/MAÍS/trials.csv") %>%
  filter(Loc == "NM")

# Then, read in all of the data, and associate with key-value sets
cob <- read_excel("../DATA/MAÍS/cob.xls") %>% 
  unite("Trial_ID", ID, Year, remove = F) %>% 
  unite("Plant_ID", Trial_ID, `Plant id`, remove = F) %>% 
  select(-ID, -`Plant id`, -ACP, -Loc, -Rep, -Year, -ACNO, -ITEM, -ethnic, -Elevm, -elevft, -lanfam, -langr, -lansubgr) %>%
  filter(Trial_ID %in% trials$Trial_ID)
ear <- read_excel("../DATA/MAÍS/ear.xls") %>% 
  unite("Trial_ID", ID, Year, remove = F) %>% 
  unite("Plant_ID", Trial_ID, `Plant id`, remove = F) %>% 
  select(-ID, -`Plant id`, -ACP, -Loc, -Rep, -Year, -ACNO, -ITEM, -ethnic, -elevm, -elevft, -lanfam, -langr, -lansubgr) %>%
  filter(Trial_ID %in% trials$Trial_ID)
ear2 <- read_excel("../DATA/MAÍS/ear 2.xls") %>% 
  unite("Trial_ID", ID, Year, remove = F) %>% 
  select(-ID, -ACP, -Loc, -Rep, -Year, -ACNO, -ITEM, -ethinic, -elevm, -elevft, -lanfam, -langr, -lansubgr) %>%
  filter(Trial_ID %in% trials$Trial_ID)
ear_shape <- read_excel("../DATA/MAÍS/ear shape.xls") %>% 
  unite("Trial_ID", ID, Year, remove = F) %>% 
  unite("Plant_ID", Trial_ID, `Plant id`, remove = F) %>% 
  select(-ID, -`Plant id`, -ACP, -Loc, -Rep, -Year, -ACNO, -ITEM, -ethnic, -elevm, -elevft, -lanfam, -langr, -lansubgr) %>%
  filter(Trial_ID %in% trials$Trial_ID)
harvest <- read_excel("../DATA/MAÍS/harvest.xls") %>% 
  unite("Trial_ID", ID, Year, remove = F) %>% 
  unite("Plant_ID", Trial_ID, `Plant id`, remove = F) %>% 
  select(-ID, -`Plant id`, -ACP, -Loc, -Rep, -Year, -ACNO, -ITEM, -ethnic, -elevm, -elevft, -lanfam, -langr, -lansubgr) %>%
  mutate(`Husk blades (presence)` = as.logical(`Husk blades (presence)`)) %>%
  filter(Trial_ID %in% trials$Trial_ID)
kernel <- read_excel("../DATA/MAÍS/kernel.xls") %>% 
  unite("Trial_ID", ID, Year, remove = F) %>% 
  unite("Plant_ID", Trial_ID, `Plant id`, remove = F) %>% 
  select(-ID, -`Plant id`, -ACP, -Loc, -Rep, -Year, -ACNO, -ITEM, -ethnic, -elevm, -elevft, -lanfam, -langr, -lansubgr) %>%
  filter(Trial_ID %in% trials$Trial_ID)
maturity <- read_excel("../DATA/MAÍS/maturity.xls") %>% 
  unite("Trial_ID", ID, Year, remove = F) %>% 
  select(-ID, -ACP, -Loc, -Rep, -Year, -ACNO, -ITEM, -ethnic, -elevm, -elevft, -lanfam, -langr, -lansubgr) %>%
  filter(Trial_ID %in% trials$Trial_ID)
phenology <- read_excel("../DATA/MAÍS/phenolology.xls") %>% 
  unite("Trial_ID", ID, Year, remove = F) %>% 
  select(-ID, -ACP, -Loc, -Rep, -Year, -ACNO, -ITEM, -ethnic, -elevm, -elevft, -lanfam, -langr, -lansubgr) %>%
  filter(Trial_ID %in% trials$Trial_ID)
tassel <- read_excel("../DATA/MAÍS/tassel.xls") %>% 
  unite("Trial_ID", ID, Year, remove = F) %>% 
  unite("Plant_ID", Trial_ID, `Plant id`, remove = F) %>% 
  select(-ID, -`Plant id`, -ACP, -Loc, -Rep, -Year, -ACNO, -ITEM, -ethnic, -elevm, -elevft, -lanfam, -langr, -lansubgr) %>%
  filter(Trial_ID %in% trials$Trial_ID)
vegetative <- read_excel("../DATA/MAÍS/vegetative.xls") %>% 
  unite("Trial_ID", ID, Year, remove = F) %>% 
  unite("Plant_ID", Trial_ID, `Plant id`, remove = F) %>% 
  select(-ID, -`Plant id`, -ACP, -Loc, -Rep, -Year, -ACNO, -ITEM, -ethnic, -elevm, -elevft, -lanfam, -langr, -lansubgr) %>%
  filter(Trial_ID %in% trials$Trial_ID)

# Find means of values in each trial (ear2, maturity, and phenology are already by trial)
cob.means <- cob %>% 
  group_by(Trial_ID) %>% 
  select(-Plant_ID,-`Cob color (white or non white)`) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

ear.means <- ear %>% 
  group_by(Trial_ID) %>% 
  select(-Plant_ID) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

harvest.means <- harvest %>% 
  group_by(Trial_ID) %>% 
  select(-Plant_ID) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

kernel.means <- kernel %>% 
  group_by(Trial_ID) %>% 
  select(-Plant_ID) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

tassel.means <- tassel %>% 
  group_by(Trial_ID) %>% 
  select(-Plant_ID) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

vegetative.means <- vegetative %>% 
  group_by(Trial_ID) %>% 
  select(-Plant_ID) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))






