# dssatr tester
library(dssatr)
library(httr)
library(magrittr)
library(tidyverse)
library(cleangeo)

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
wkt_geom <- sf::st_point(c(-108.618642,37.355880))

test <- sf::st_multipoint(matrix(c(-109.25,37.95556,-109.25,37.95555,-109.25,38.05,-109.25,38.15), nrow = 4, byrow = TRUE)) %>%
  sf::st_as_text() %>%
    sf::st_as_sfc("+proj=longlat +datum=WGS84")

wkt_geom <- sf::read_sf("/Users/bocinsky/IMPORTANT/CCAC/INSTITUTE/PROJECTS/PFP/DATA/CCAC.shp") %>%
  dplyr::select(geometry) %>%
  sf::st_union()

# wkt_geom <- raster::extent(-110,-107,36,39) %>% 
#   FedData::polygon_from_extent("+proj=longlat +datum=WGS84") %>%
#   sf::st_as_sf() %$%
#   geometry %>%
#   sf::st_as_text()

ssurgo.out <- test %>%
  dssatr:::dssat_get_ssurgo() %T>%
  dssatr:::dssat_write_soils(output.dir = output.dir,
                             clean = TRUE)

daymet.out <- ssurgo.out$soils[[1]]$mapunits %>%
  dssatr:::dssat_get_daymet() %T>%
  dssatr:::dssat_write_weather(file.name = "TEST0000",
                    output.dir = output.dir)



test <- raster::raster("/Users/bocinsky/Downloads/daymet_v3_prcp_annttl_1980_na.nc4") %>%
  



test <- dssat_spatial(template = mvnp,
                      label = "MVNP",
                      output.dir = output.dir,
                      raw.dir = raw.dir,
                      extraction.dir = extraction.dir,
                      force.redo = FALSE)
