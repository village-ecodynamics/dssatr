dssat_get_daymet <- function(coords,
                             longitude,
                             latitude,
                             years = 1980:2015){
  vars <- c("prcp",
            "tmin",
            "tmax",
            "srad",
            "dayl")
  
  if(missing(coords)){
    if(missing(longitude))
      stop("Either coords or longitude/latitude vectors must be supplied.")
    
    if(!identical(length(longitude),length(latitude)))
      warning("Longitude and latitude vectors are different lengths. Values will be recycled.")
    
    coords <- tibble::tibble(longitude = longitude,
                             latitude = latitude)
  }
  
  if(is.matrix(coords)) {
    coords %<>%
      tibble::as_tibble() %>%
      magrittr::set_names(c("longitude","latitude"))
  }

  daymet <- stringr::str_c("https://daymet.ornl.gov/data/send/saveData?",
                         "lat=",coords$latitude,
                         "&lon=",coords$longitude,
                         "&measuredParams=",stringr::str_c(vars, collapse = ","),
                         "&year=",stringr::str_c(years, collapse = ",")) %>%
    purrr::map(function(x){
      x %>%
        httr::GET() %>%
        httr::content(as = "text",
                      encoding = "UTF-8") %>%
        readr::read_csv(skip = 7) %>%
        dplyr::mutate(drad = `srad (W/m^2)` * `dayl (s)` / 1000000)
    }) %>%
    tibble(weather = ., geometry = coords %>%
             purrr::by_row(purrr::lift_vl(sf::st_point)) %$%
            `.out` %>%
             sf::st_sfc(crs = "+proj=longlat +datum=WGS84")) %>%
    sf::st_as_sf()
  
    return(daymet)
}
