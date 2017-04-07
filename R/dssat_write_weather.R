dssat_write_weather <- function(weather,
                                output.dir = "."){
  
  dir.create(output.dir,
             showWarnings = FALSE,
             recursive = TRUE)
  
  data(daymet)
  
  weather %>%
    purrr::by_row(function(x){
      # Daymet cells are small enough that we can suppress warning about
      # st_centroid not giving correct centroids for longitude/latitude data
      suppressWarnings({
        coords <- x$geometry %>%
          sf::st_centroid() %>%
          unlist()
      })
      
      dates <- x %$%
        weather %>%
        magrittr::extract2(1) %$%
        stringr::str_c(year,".",yday) %>%
        lubridate::as_date(format="%Y.%j")
      
      tmin <- x %$%
        weather %>%
        magrittr::extract2(1) %$%
        `tmin (deg c)`
      
      tmax <- x %$%
        weather %>%
        magrittr::extract2(1) %$%
        `tmax (deg c)`
      
      prcp <- x %$%
        weather %>%
        magrittr::extract2(1) %$%
        `prcp (mm/day)`
      
      srad <- x %$%
        weather %>%
        magrittr::extract2(1) %$%
        drad
      
      file.name <- x$tile %>% stringr::str_pad(width = 8, pad = "0")
      
      dssat_write_weather_file(coords = coords,
                               dates = dates,
                               tmin = tmin,
                               tmax = tmax,
                               prcp = prcp,
                               srad = srad,
                               file.name = file.name,
                               output.dir = output.dir)
    })
  
  return(output.dir)
}
