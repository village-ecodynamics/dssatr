dssat_get_daymet <- function(x,
                             years = 1980:2016,
                             vars = c("prcp",
                                      "tmin",
                                      "tmax",
                                      "srad",
                                      "dayl")){
  
  daymet <- dssatr::get_daymet(x,
                               years,
                               vars) %>%
    dplyr::mutate(weather = purrr::map(weather, .f = function(x){
      x %>%
        dplyr::mutate(`srad` = (`srad` * (`dayl`/with(ud_units, d)) / 1000000) %>% set_units(parse_unit("MJ m-2 day-1"))) %>%
        dplyr::select(-dayl) %>%
        dplyr::rename(RAIN = prcp,
                      SRAD = srad,
                      TMAX = tmax,
                      TMIN = tmin)
    })) %>%
    dplyr::mutate(tile = 1:n())
  
  return(daymet)
}
