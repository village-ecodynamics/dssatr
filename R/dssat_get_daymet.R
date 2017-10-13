dssat_get_daymet <- function(x,
                             years = 1980:2016,
                             vars = c("prcp",
                                      "tmin",
                                      "tmax",
                                      "srad",
                                      "dayl")){
  
  dssatr::get_daymet(x,
                               years,
                               vars) %>%
    dplyr::mutate(weather = purrr::map(weather, .f = function(x){
      x %>%
        dplyr::mutate(`srad` = (units::set_units(`srad`,units::parse_unit("J day-1 m-2")) * units::set_units(`dayl`,d)) %>% 
                        units::set_units(units::parse_unit("MJ m-2"))) %>%
        dplyr::select(-dayl) %>%
        dplyr::rename(RAIN = prcp,
                      SRAD = srad,
                      TMAX = tmax,
                      TMIN = tmin)
    })) %>%
    dplyr::mutate(tile = 1:n())
}
