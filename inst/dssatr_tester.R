# dssatr tester
library(dssatr)
library(httr)
library(magrittr)
library(tidyverse)

# Set a directory for testing
testDir <- "~/dssatr Test"

dir.create(testDir, showWarnings=F, recursive=T)

# Extract data for the Village Ecodynamics Project "VEPIIN" study area:
# http://village.anth.wsu.edu
data(mvnp)

output.dir <- paste0(testDir,"/OUTPUT/")
raw.dir <- paste0(output.dir,"/DATA/RAW/")
extraction.dir <- paste0(output.dir,"/DATA/EXTRACTIONS/")

# get point above Paul's Old Garden on the Crow CAnyon campus
lon <- -108.618642
lat <-  37.355880
vars <- c("prcp",
          "tmin",
          "tmax",
          "srad",
          "dayl")
years <- 1980:2015

ccac.daymet <- httr::GET(paste0("https://daymet.ornl.gov/data/send/saveData?lat=",lat,"&lon=",lon,"&measuredParams=",paste0(vars,collapse=","),"&year=",paste0(years,collapse=","))) %>%
  httr::content(as = "text",
                encoding = "UTF-8") %>%
  readr::read_csv(skip = 7) %>%
  dplyr::mutate(drad = `srad (W/m^2)` * `dayl (s)` / 1000000)

write_dssat_weather(coords = c(lon,lat),
                    dates = lubridate::as_date(paste0(ccac.daymet$year,".",ccac.daymet$yday),format="%Y.%j"),
                    tmin = ccac.daymet$`tmin (deg c)`,
                    tmax = ccac.daymet$`tmax (deg c)`,
                    prcp = ccac.daymet$`prcp (mm/day)`,
                    srad = ccac.daymet$drad,
                    file.name = "TEST",
                    output.dir = "./")



test <- dssat_spatial(template = mvnp,
                      label = "MVNP",
                      output.dir = output.dir,
                      raw.dir = raw.dir,
                      extraction.dir = extraction.dir,
                      force.redo = FALSE)
