# dssatr tester
library(dssatr)
library(httr)
library(magrittr)
library(tidyverse)
library(soilDB)

# Set a directory for testing
testDir <- "./dssatr test"

dir.create(testDir,
           showWarnings = F,
           recursive = T)

# Extract data for the Village Ecodynamics Project "VEPIIN" study area:
# http://village.anth.wsu.edu
data(mvnp)

output.dir <- stringr::str_c(testDir,"/OUTPUT") %T>%
  dir.create(showWarnings = F,
             recursive = T)
raw.dir <- stringr::str_c(output.dir,"/DATA/RAW") %T>%
  dir.create(showWarnings = F,
             recursive = T)
extraction.dir <- stringr::str_c(output.dir,"/DATA/EXTRACTIONS") %T>%
  dir.create(showWarnings = F,
             recursive = T)

# get point above Paul's Old Garden on the Crow Canyon campus
longitude <- -108.618642
latitude <-  37.355880
wkt_geom <- sf::st_point(c(longitude,latitude)) %>%
  sf::st_as_text()
years <- 1980:2015

daymet.out <- dssat_get_daymet(longitude = longitude,
                                latitude = latitude,
                                years = years) %T>%
  dssat_write_weather(file.name = "TEST0000",
                    output.dir = output.dir)

ssurgo.out <- wkt_geom %>%
  dssat_get_ssurgo()




test <- dssat_spatial(template = mvnp,
                      label = "MVNP",
                      output.dir = output.dir,
                      raw.dir = raw.dir,
                      extraction.dir = extraction.dir,
                      force.redo = FALSE)
