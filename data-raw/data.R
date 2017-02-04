library(FedData)

##### MVNP Spatial Polygon

## Load spatial polygon for the boundary of Mesa Verde National Park in southwestern Colorado:
dir.create("./data-raw/MVNP", showWarnings = F, recursive = T)
# download shapefile directory
FedData::download_data("http://nrdata.nps.gov/programs/Lands/meve_tracts.zip", destdir="./data-raw/MVNP")
# uncompress shapefile directory
utils::unzip("./data-raw/MVNP/meve_tracts.zip", exdir="./data-raw/MVNP/meve_tracts")
# read shapefile
mvnp <- rgdal::readOGR("./data-raw/MVNP/meve_tracts", layer='MEVE_boundary')

unlink("./data-raw/MVNP", recursive=T)

rgdal::writeOGR(mvnp, dsn = "./data-raw/", layer = "mvnp", driver = "ESRI Shapefile", overwrite_layer = TRUE)

devtools::use_data(mvnp, overwrite = TRUE)

# DSSAT_GENERIC_SOILS_HORIZON_HYDRO.csv
dssat_soil_hydrology <- readr::read_csv("./data-raw/DSSAT_GENERIC_SOILS_HORIZON_HYDRO.csv")
# devtools::use_data(dssat_soil_hydrology, overwrite = TRUE, internal = TRUE)

# DSSAT_DRAINAGE_CLASSES.csv
dssat_drainage_classes <- readr::read_csv("./data-raw/DSSAT_DRAINAGE_CLASSES.csv", na = "NA")
# devtools::use_data(dssat_drainage_classes, overwrite = TRUE, internal = TRUE)

# DSSAT_RUNOFF_POTENTIAL.csv
dssat_runoff_potential <- readr::read_csv("./data-raw/DSSAT_RUNOFF_POTENTIAL.csv", na = "NA")
# devtools::use_data(dssat_runoff_potential, overwrite = TRUE, internal = TRUE)

# DSSAT_SOIL_DATA_FILE_STRUCTURE.csv
dssat_soil_data_file_structure <- readr::read_csv("./data-raw/DSSAT_SOIL_DATA_FILE_STRUCTURE.csv", na = "NA")


devtools::use_data(dssat_soil_hydrology,
                   dssat_drainage_classes,
                   dssat_runoff_potential,
                   dssat_soil_data_file_structure,
                   overwrite = TRUE,
                   internal = T)
