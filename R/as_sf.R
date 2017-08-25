#' Convert a Raster* object to an sf object
#'
#' Convert a Raster* object to an sf object.
#' This does not preserve the data, but only converts
#' the grid to an sf object with geometry type POLYGONS.
#' @param x Raster* to be converted into an object class \code{sf}
#' @param ... further specifications, see \link{st_as_sf}
#' @method st_as_sf Raster
#' @name st_as_sf
#' @export
st_as_sf.Raster = function(x, ...) {
  if (!requireNamespace("sp", quietly = TRUE))
    stop("package sp required, please install it first")
  
  x %>%
    as('SpatialPolygons') %>%
    sf::st_as_sf()
}
