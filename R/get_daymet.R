#' Download and crop the 1-km DAYMET daily weather dataset.
#' 
#' This function purposefully masks the [FedData::get_daymet()] function, 
#' and will likely replace it eventually.
#' @param x An object class \code{sf} to serve as a template
#' @param years A numeric vector of years to extract.
#' @param vars A character vector of variables to extract.\cr
#' The available variables are:
#' \describe{
#'   \item{dayl}{Duration of the daylight period in seconds per day.
#' This calculation is based on the period of the day during 
#' which the sun is above a hypothetical flat horizon.}
#'   \item{prcp}{Daily total precipitation in millimeters per day, 
#' sum of all forms converted to water-equivalent.}
#'   \item{srad}{Incident shortwave radiation flux density in watts per square meter, 
#' taken as an average over the daylight period of the day.\cr
#' NOTE: Daily total radiation (MJ/m2/day) can be calculated as follows:\cr
#' \deqn{\frac{srad \times dayl}{l000000}}{srad*dayl/l000000}}
#'   \item{swe}{Snow water equivalent in kilograms per square meter.
#' The amount of water contained within the snowpack.}
#'   \item{tmax}{Daily maximum 2-meter air temperature in degrees Celsius.}
#'   \item{tmin}{Daily minimum 2-meter air temperature in degrees Celsius.}
#'   \item{vp}{Water vapor pressure in pascals. Daily average partial pressure of water vapor.}
#' }
#' @name get_daymet
#' @export
get_daymet <- function(x,
                             years = 1980:2016,
                             vars = c("tmax",
                                      "tmin",
                                      "srad",
                                      "vp",
                                      "swe",
                                      "prcp",
                                      "dayl")){

  data(dssat_daymet_tiles)
  
  if(sf::st_geometry_type(x) == "POINT") {
    x %<>%
      sf::st_transform(26912) %>%
      sf::st_buffer(0.0000001) %>%
      sf::st_transform(x %>%
  sf::st_crs())
  }

  tiles <- dssat_daymet_tiles %>%
    raster::crop(x %>%
                   sf::st_transform(raster::projection(dssat_daymet_tiles)) %>%
                   sf::st_bbox() %>%
                   magrittr::extract(c("xmin","xmax","ymin","ymax")) %>%
                   as.vector(),
                 snap = "out") %>%
    sf::st_as_sf() %>%
    sf::st_transform(x %>% 
                       sf::st_crs())
  
  suppressMessages({
    tiles %<>%
      dplyr::filter(
        sf::st_intersects(
          tiles,
          x,
          sparse = F) %>%
          apply(1, any)
      )
  })
  
  daymet <- tiles %>%
    purrrlyr::by_row(function(x){
      suppressWarnings({
        coords <- x %$%
          geometry %>%
          sf::st_centroid() %>%
          magrittr::extract2(1) %>%
          as.numeric()
      })
      
      out <- stringr::str_c("https://daymet.ornl.gov/data/send/saveData?",
                     "lat=",coords[2],
                     "&lon=",coords[1],
                     "&measuredParams=",stringr::str_c(vars, collapse = ","),
                     "&year=",stringr::str_c(years, collapse = ",")) %>%
        httr::GET() %>%
        httr::content(as = "text",
                      encoding = "UTF-8") %>%
        readr::read_csv(skip = 7)
      
      out %<>%
        dplyr::mutate(date = as.Date(stringr::str_c(year,"-",yday),
                                     format = "%Y-%j")) %>%
        dplyr::select(-year,-yday) %>%
        dplyr::select(date, dplyr::everything())
      
      names(out) <- gsub("deg c", "dC", names(out))
      names(out) <- gsub("/day", "/d", names(out))
      
      out_units <- names(out) %>% 
        stringr::str_extract("\\(.*?\\)") %>%
        stringr::str_replace("\\(","") %>%
        stringr::str_replace("\\)","")
      
      indices <- names(out) %>% 
        grep(pattern = "\\(.*?\\)")
      
      out[indices] %<>%
        purrr::map2(.y = out_units[indices],
                    .f = function(x, y){
                      x * with(units::ud_units, eval(parse(text = y)))
                    }) %>%
        tibble::as_tibble()
      
      names(out) <- names(out) %>% 
        stringr::str_replace(" \\(.*","")
      
      return(out)
      
    },
    .to = "weather") %>%
    sf::st_as_sf()

  class(daymet) <- c(class(daymet),"weather")
  
  return(daymet)
}
