
#' Run a spatial DSSAT model from FedData downloads
#'
#'This function reads in several gridded datasets necessary for running a
#'spatial DSSAT model, selects the resolution to run the model at based on
#'the lowest resolution dataset (Daymet), aggregates all other datasets
#'down to that resolution, and preps and runs the simulation on a per-cell basis
#'
#' @param template A Raster* or Spatial* object to serve
#' as a template for cropping.
#' @param label A character string naming the study area.
#' @param output.dir A character string indicating a directory where data should be output.
#' @param raw.dir A character string indicating where raw downloaded files should be put.
#' The directory will be created if missing. Defaults to \code{[output.dir]/DATA/RAW/}.
#' @param extraction.dir A character string indicating where the extracted and cropped SSURGO shapefiles should be put.
#' The directory will be created if missing. Defaults to \code{[output.dir]/DATA/EXTRACTIONS/}.
#' @param force.redo If an extraction for this template and label already exists, should a new one be created? Defaults to FALSE.
#' @return A named list containing "spatial" and "tabular" data.
#' @export
#' @importFrom FedData get_ssurgo get_daymet
#' @importFrom magrittr %<>% %>%
dssat_spatial <- function(template, label, output.dir = "./OUTPUT/", raw.dir = paste0(output.dir,"/DATA/RAW/"), extraction.dir = paste0(output.dir,"/DATA/EXTRACTIONS/"), force.redo = FALSE){
  raw.dir %<>%
    normalizePath(mustWork = FALSE)

  extraction.dir %<>%
    normalizePath(mustWork = FALSE)

  # Prepare the weather data

  cat("Loading the DAYMET weather data \n")
  DAYMET <- FedData::get_daymet(template = template,
                                label = label,
                                elements = c("prcp",
                                             "tmin",
                                             "tmax",
                                             "srad",
                                             "dayl"),
                                raw.dir = paste0(raw.dir,"/DAYMET/"),
                                extraction.dir = paste0(extraction.dir,"/DAYMET/"),
                                force.redo = force.redo)

  # Calculate daily solar accumulated radiation
  DAYMET$drad <- DAYMET$srad * DAYMET$dayl / 1000000

  # A template raster from the Daymet data
  rast.temp <- DAYMET[[1]][[1]][[1]]

  coords <- sp::SpatialPoints(rast.temp)
  raster::projection(coords) <- raster::projection(rast.temp)
  coords <- sp::spTransform(coords,"+proj=longlat +ellps=WGS84")@coords
  dates <- lubridate::as_date(names(DAYMET$prcp),format="X%Y.%m.%d")
  rowcols <- cbind(raster::rowFromCell(rast.temp,1:nrow(coords)),
                   raster::colFromCell(rast.temp,1:nrow(coords)))

  names(data) <- signals
  dir.create(paste0(output.dir, "/WEATHER/"), recursive = T, showWarnings = F)

  for(cell in 1:nrow(coords)){
    if(file.exists(paste0(output.dir, "/WEATHER/",sprintf("%08d", cell),".WTH"))) next
    cat("\n",cell)
    prep_weather(out.dir = paste0(output.dir, "/WEATHER/"),
                 file.name = paste0(sprintf("%08d", cell)),
                 dates = dates,
                 coords = coords[cell,],
                 tmin = DAYMET$tmin[cell],
                 tmax = DAYMET$tmax[cell],
                 prcp = DAYMET$prcp[cell],
                 srad = DAYMET$drad[cell])
  }

  ###
  cat("Loading the NED elevation data\n")
  NED <- FedData::get_ned(template = template,
                          label = label,
                          res = "1",
                          raw.dir = paste0(raw.dir,"/NED/"),
                          extraction.dir = paste0(extraction.dir,"/NED/"),
                          force.redo = force.redo)

  cat("Loading the NRCS soils data \n")
  SSURGO <- FedData::get_ssurgo(template = template,
                                label = label,
                                raw.dir = paste0(raw.dir,"/SSURGO/"),
                                extraction.dir = paste0(extraction.dir,"/SSURGO/"),
                                force.redo = force.redo)




  return(list(SSURGO=SSURGO, DAYMET=DAYMET))
}
