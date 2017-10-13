dssat_write_weather <- function(weather,
                                output_dir = "."){
  
  dir.create(output_dir,
             showWarnings = FALSE,
             recursive = TRUE)
  
  weather %>%
    purrrlyr::by_row(function(x){
      
      dssat_write_weather_file(
        coords = suppressWarnings({
          x$geometry %>%
            sf::st_centroid() %>%
            unlist()
        }),
        date = x %$%
          weather %>%
          magrittr::extract2(1) %$%
          date,
        TMIN = x %$%
          weather %>%
          magrittr::extract2(1) %$%
          TMIN,
        TMAX = x %$%
          weather %>%
          magrittr::extract2(1) %$%
          TMAX,
        RAIN = x %$%
          weather %>%
          magrittr::extract2(1) %$%
          RAIN,
        SRAD = x %$%
          weather %>%
          magrittr::extract2(1) %$%
          SRAD,
        file.name = x$tile %>% stringr::str_pad(width = 8, pad = "0"),
        output_dir = output_dir)
    })
  
  return(output_dir)
}
