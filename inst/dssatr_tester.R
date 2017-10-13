# dssatr tester
library(dssatr)
library(magrittr)
library(units)


# Set a directory for testing
testDir <- "./dssatr test"

dir.create(testDir,
           showWarnings = F,
           recursive = T)

# Extract data for the Village Ecodynamics Project "VEPIIN" study area:
# http://village.anth.wsu.edu
# data(mvnp)

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
aoi <- sf::st_point(c(-108.618642,37.355880)) %>%
  sf::st_as_text() %>%
  sf::st_as_sfc(4326)

aoi <- sf::st_multipoint(
  matrix(c(-109.25,37.95556,-109.25,37.95555,-109.25,38.05,-109.25,38.15),
         nrow = 4,
         byrow = TRUE)
) %>%
  sf::st_as_text() %>%
  sf::st_as_sfc(4326)

aoi <- dssat_ccac

# aoi <- dssat_mvnp

# wkt_geom <- raster::extent(-110,-107,36,39) %>% 
#   FedData::polygon_from_extent("+proj=longlat +datum=WGS84") %>%
#   sf::st_as_sf() %$%
#   geometry %>%
#   sf::st_as_text()

ssurgo.out <- aoi %>%
  dssatr:::dssat_get_ssurgo()

daymet.out <- aoi %>%
  dssatr:::dssat_get_daymet(years = 2009:2012)

weather <- daymet.out
soil <- ssurgo.out
cultivars <- dssatr:::dssat_read_cultigen("~/DSSAT46/Genotype/MZCER046.CUL") %>%
  dplyr::filter(`@VAR#` == "AC0001")
earliest_planting_doy = 136
latest_planting_doy = 167
output_dir <- "./dssatr test/OUTPUT/dssat_run/"
name <- "test"

out <- dssat_run_batch(name = "test",
                       weather = daymet.out,
                       soil = ssurgo.out,
                       output_dir = "./dssatr test/OUTPUT/dssat_run/")

test <- out$yields %>%
  dplyr::group_by(field, cultivar) %>%
  dplyr::summarise(yield = mean(yield)) %>%
  dplyr::right_join(out$fields) %>%
  sf::st_sf()

plot(test["yield"])

test <- ssurgo.out %$%
  mapunits %>%
  sf::st_intersection(aoi) %>%
  sf::st_intersection(x = daymet.out %>%
                        dplyr::select(-weather),
                      y = .) %>%
  dplyr::left_join(ssurgo.out %$%
                     components %>%
                     dplyr::select(mukey, ID_SOIL),
                   by = "mukey") %>%
  dplyr::select(-mukey, -muname) %>%
  dplyr::distinct() %>%
  # tibble::as_tibble() %>%
  sf::st_as_sf()

cultivars = c("GF0001 Base Garst808-wh403")  




test <- raster::raster("/Users/bocinsky/Downloads/daymet_v3_prcp_annttl_1980_na.nc4") %>%
  



test <- dssat_spatial(template = mvnp,
                      label = "MVNP",
                      output.dir = output.dir,
                      raw.dir = raw.dir,
                      extraction.dir = extraction.dir,
                      force.redo = FALSE)
