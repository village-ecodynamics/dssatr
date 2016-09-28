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
# devtools::install_github("bocinsky/FedData")
library(FedData)
pkg_test("readr")
pkg_test("readxl")
pkg_test("dplyr")
pkg_test("tidyr")
pkg_test("data.table")
pkg_test("sp")
pkg_test("lubridate")
pkg_test("sirad")
pkg_test("soiltexture")

pkg_test("dssatr")

setwd("~/Desktop/MAÍS/R")

# MAÍS data were collected at two research stations:
# The New Mexico State Agricultural Research Station near Farmington, NM, and
# North Central Regional Plant Introduction Station near Ames, Iowa
# We will use FedData to download daily GHCN and SSURGO data for each site.
NM_ARS.location <- SpatialPoints(matrix(c(-108.3105,36.6888),ncol=2), proj4string = CRS("+proj=longlat +datum=WGS84"))
NM_ARS.weather <- get_ghcn_daily_station(ID = 'USC00293142', raw.dir = "../DATA/GHCN/")
NM_ARS.soil <- get_ssurgo(template = NM_ARS.location, label = "NM_ARS", raw.dir = "../DATA/SSURGO/RAW/", extraction.dir = "../DATA/SSURGO/EXTRACTIONS/")

NCRPIS.location <- SpatialPoints(matrix(c(-93.663962, 42.011351),ncol=2), proj4string = CRS("+proj=longlat +datum=WGS84"))
NCRPIS.weather <- get_ghcn_daily_station(ID = 'USW00094989', raw.dir = "../DATA/GHCN")
NCRPIS.soil <- get_ssurgo(template = NCRPIS.location, label = "NCRPIS", raw.dir = "../DATA/SSURGO/RAW/", extraction.dir = "../DATA/SSURGO/EXTRACTIONS/")

# test <- get_ghcn_daily(template = c('USC00293142','USW00094989'), label = "test", raw.dir = "../DATA/GHCN/RAW/", extraction.dir = "../DATA/GHCN/EXTRACTIONS/", force.redo = T)

# Prepare DSSAT soil file
ssurgo_to_dssat(ssurgo = NM_ARS.soil, label = "NM_ARS00", out.file = "~/Desktop/soil.sol", append = F)
ssurgo_to_dssat(ssurgo = NCRPIS.soil, label = "NCRPIS00", out.file = "~/Desktop/soil.sol", append = T)

# Prepare DSSAT weather files
ghcn_daily_station_to_dssat(ghcn_daily_station = NM_ARS.weather, ID = "USC00293142", label = "NM_ARS00", coords = NM_ARS.location@coords, out.dir = "~/Desktop/")
ghcn_daily_station_to_dssat(ghcn_daily_station = NCRPIS.weather, ID = "USW00094989", label = "NCRPIS00", coords = NCRPIS.location@coords, out.dir = "~/Desktop/")

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

### Getting GDD info for each season
# Convert GHCN weather data into GDD
NM_ARS.weather.df <- as_data_frame(FedData::station_to_data_frame(NM_ARS.weather))
NCRPIS.weather.df <- as_data_frame(FedData::station_to_data_frame(NCRPIS.weather))

# Divide temperatures by 10, and calculate GDD
NM_ARS.gdd <- NM_ARS.weather.df %>%
  dplyr::mutate(GDD=calc_gdd(tmin = TMIN/10, tmax = TMAX/10, t.base = 10, t.cap = 30)) %>%
  filter(DATE %within% interval(as.Date("2004-01-01"),as.Date("2005-12-31"))) %>%
  select(DATE,GDD) %>%
  mutate(GDD = zoo::na.approx(GDD))
NCRPIS.gdd <- NCRPIS.weather.df %>%
  dplyr::mutate(GDD=calc_gdd(tmin = TMIN/10, tmax = TMAX/10, t.base = 10, t.cap = 30)) %>%
  filter(DATE %within% interval(as.Date("2004-01-01"),as.Date("2005-12-31"))) %>%
  select(DATE,GDD) %>%
  mutate(GDD = zoo::na.approx(GDD))

# Make a table of planting and maturity dates for each trial
# For each accession, only one plot was measured (Werth 2007:31)
planting_maturity <- dplyr::left_join(phenology,maturity,by="Trial_ID") %>%
  filter(!is.na(`Black layer date`))%>%
  left_join(trials,by = "Trial_ID") %>%
  left_join(accessions, by = "ACNO") %>%
  select(Trial_ID,Loc,ACNO,ITEM,ethnic,`Planting date`,`Black layer date`) %>%
  arrange(ACNO) %>%
  mutate(planting_maturity = interval(`Planting date`,`Black layer date`)) %>%
  select(Trial_ID,Loc,ACNO,ITEM,ethnic,planting_maturity)

GDDs <- sapply(planting_maturity[['planting_maturity']],function(x){
  sum(NM_ARS.gdd[NM_ARS.gdd$DATE %within% x,'GDD'])
})

planting_maturity <- mutate(planting_maturity,GDD = GDDs)

write_csv(planting_maturity, "../OUTPUT/planting_maturity.csv")


### Reproducing Werth's MA Thesis Study
# Convert GHCN weather data into GDD





