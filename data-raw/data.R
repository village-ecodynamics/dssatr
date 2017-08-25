library(FedData)
library(sf)

##### MVNP Spatial Polygon

## Load spatial polygon for the boundary of Mesa Verde National Park in southwestern Colorado:
dir.create("./data-raw/MVNP", showWarnings = F, recursive = T)
# download shapefile directory
FedData::download_data("http://nrdata.nps.gov/programs/Lands/meve_tracts.zip", destdir="./data-raw/MVNP")
# uncompress shapefile directory
utils::unzip("./data-raw/MVNP/meve_tracts.zip", exdir="./data-raw/MVNP/meve_tracts")
# read shapefile
dssat_mvnp <- sf::st_read("./data-raw/MVNP/meve_tracts/MEVE_boundary.shp")

unlink("./data-raw/MVNP", recursive=T)

sf::st_write(dssat_mvnp,
             dsn = "./data-raw/dssat_mvnp.gpkg",
             driver = "GPKG")

devtools::use_data(dssat_mvnp, overwrite = TRUE)


## CCAC spatial polygon
dssat_ccac <- sf::read_sf("./data-raw/CCAC.shp") %>%
  dplyr::select(geometry) %>%
  sf::st_union() %>%
  sf::st_transform(4326)

devtools::use_data(dssat_ccac, overwrite = TRUE)

# DSSAT_GENERIC_SOILS_HORIZON_HYDRO.csv
dssat_soil_hydrology <- readr::read_csv("./data-raw/dssat_generic_soils_horizon_hydro.csv")
# devtools::use_data(dssat_soil_hydrology, overwrite = TRUE, internal = TRUE)

# DSSAT_DRAINAGE_CLASSES.csv
dssat_drainage_classes <- readr::read_csv("./data-raw/dssat_drainage_classes.csv", na = "NA")
# devtools::use_data(dssat_drainage_classes, overwrite = TRUE, internal = TRUE)

# DSSAT_RUNOFF_POTENTIAL.csv
dssat_runoff_potential <- readr::read_csv("./data-raw/dssat_runoff_potential.csv", na = "NA")
# devtools::use_data(dssat_runoff_potential, overwrite = TRUE, internal = TRUE)

# DSSAT_SOIL_DATA_FILE_STRUCTURE.csv
dssat_soil_data_file_structure <- readr::read_csv("./data-raw/dssat_soil_data_file_structure.csv", na = "NA")


devtools::use_data(dssat_soil_hydrology,
                   dssat_drainage_classes,
                   dssat_runoff_potential,
                   dssat_soil_data_file_structure,
                   overwrite = TRUE,
                   internal = T)

# Daymet grid
## Load spatial grid for the DAYMET dataset

dir.create("./data-raw/daymet", showWarnings = F, recursive = T)
# download shapefile directory
FedData::download_data("https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1343/daymet_v3_prcp_annttl_1980_na.nc4", destdir="./data-raw/daymet")
# read shapefile
dssat_daymet_tiles <- raster::raster("./data-raw/daymet/daymet_v3_prcp_annttl_1980_na.nc4") %>%
  raster:::readAll() %>%
  raster::setValues(NA)

dssat_daymet_tiles %<>% raster::raster(.)

raster::dataType(dssat_daymet_tiles) <- "LOG1S"

devtools::use_data(dssat_daymet_tiles,
                   overwrite = TRUE,
                   compress = "xz")

unlink("./data-raw/daymet", recursive=T)
