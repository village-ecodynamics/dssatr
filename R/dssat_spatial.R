
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
dssat_spatial <- function(template,
                          label,
                          output.dir = "./OUTPUT/",
                          raw.dir = paste0(output.dir,"/DATA/RAW/"),
                          extraction.dir = paste0(output.dir,"/DATA/EXTRACTIONS/"),
                          force.redo = FALSE){
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

  prep_weather(daymet=DAYMET, label=label, output.dir=paste0(output.dir,"/WEATHER/"))

  # Prepare the soils data
  cat("Loading the NRCS soils data \n")
  SSURGO <- FedData::get_ssurgo(template = template,
                                label = label,
                                raw.dir = paste0(raw.dir,"/SSURGO/"),
                                extraction.dir = paste0(extraction.dir,"/SSURGO/"),
                                force.redo = force.redo)

  prep_soils(ssurgo=SSURGO[[i]], label=label, out.dir="/Users/Bocinsky/Desktop/MAÍS/DATA/DSSAT/SOIL")


  ###
  cat("Loading the NED elevation data\n")
  NED <- FedData::get_ned(template = template,
                          label = label,
                          res = "1",
                          raw.dir = paste0(raw.dir,"/NED/"),
                          extraction.dir = paste0(extraction.dir,"/NED/"),
                          force.redo = force.redo)





  return(list(SSURGO = SSURGO, DAYMET = DAYMET))
}
