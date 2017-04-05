dssat_get_daymet <- function(x,
                             years = 1980:2015,
                             vars = c("prcp",
                                      "tmin",
                                      "tmax",
                                      "srad",
                                      "dayl")){
  
  data(daymet)
  
  x %<>% 
    sf::st_transform(4326)
  
  x.sp <- x %>%
    sf::st_transform(raster::projection(daymet)) %>%
    as("Spatial") %>%
    FedData::polygon_from_extent()
  
  tiles <- daymet %>%
    raster::crop(x.sp,
                 snap = "out") %>%
    as('SpatialPolygons') %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326)
  
  suppressMessages({
    tiles %<>%
      dplyr::filter(
        sf::st_intersects(
          tiles,
          x,
          sparse = F) %>%
          apply(1, any)
      )  %>%
      dplyr::mutate(tile = 1:n())
  })
  
  daymet <- tiles %>%
    purrr::by_row(function(x){
      suppressWarnings({
        coords <- x %$%
          geometry %>%
          sf::st_centroid() %>%
          magrittr::extract2(1) %>%
          as.numeric()
      })
      
      stringr::str_c("https://daymet.ornl.gov/data/send/saveData?",
                     "lat=",coords[2],
                     "&lon=",coords[1],
                     "&measuredParams=",stringr::str_c(vars, collapse = ","),
                     "&year=",stringr::str_c(years, collapse = ",")) %>%
        httr::GET() %>%
        httr::content(as = "text",
                      encoding = "UTF-8") %>%
        readr::read_csv(skip = 7) %>%
        dplyr::mutate(drad = `srad (W/m^2)` * `dayl (s)` / 1000000) %>%
        return()
      
    },
    .to = "weather") %>%
    sf::st_as_sf()
  
  
  geoms_char <- x %>%
    sf::st_geometry_type() %>%
    as.character()
  
  geoms_char <- c(geoms_char,
                  geoms_char %>%
                    gsub(pattern = "MULTI",
                         replacement = "",
                         x = .),
                  stringr::str_c("MULTI",geoms_char)
  ) %>%
    unique()

  class(daymet) <- c(class(daymet),"weather")
  
  return(daymet)
}
