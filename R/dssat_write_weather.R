dssat_write_weather <- function(weather,
                                coords,
                                dates,
                                tmin,
                                tmax,
                                prcp,
                                srad,
                                file.name = "NAMELESS",
                                output.dir = "."){
  
  if(!missing(weather)){
    coords <- weather$geometry %>% 
      unlist()
    
    dates <- weather %$%
      weather %>%
      magrittr::extract2(1) %$%
      stringr::str_c(year,".",yday) %>%
      lubridate::as_date(format="%Y.%j")
    
    tmin <- weather %$%
      weather %>%
      magrittr::extract2(1) %$%
      `tmin (deg c)`
    
    tmax <- weather %$%
      weather %>%
      magrittr::extract2(1) %$%
      `tmax (deg c)`
    
    prcp <- weather %$%
      weather %>%
      magrittr::extract2(1) %$%
      `prcp (mm/day)`
    
    srad <- weather %$%
      weather %>%
      magrittr::extract2(1) %$%
      drad
  }
  
  file.name <- paste0(sprintf("%-8s", file.name))
  out.file <- paste0(output.dir,"/",file.name,".WTH")
  
  dates <- format(dates,"%y%j")
  
  header1 <- paste0("*WEATHER DATA: ", file.name)
  header2 <- ""
  header3 <- "@ INSI      LAT     LONG"
  header4 <- paste0("  INSI  ",
                    format(round(coords[2], digits=3), width=7, digits=3, nsmall=3)," ",
                    format(round(coords[1], digits=3), width=8, digits=3, nsmall=3))
  header5 <- ""
  header6 <- "@DATE  SRAD  TMAX  TMIN  RAIN"
  daily.lines <- paste0(dates," ",
                        format(round(srad, digits=1), width=5, digits=1, nsmall=1)," ",
                        format(round(tmax, digits=1), width=5, digits=1, nsmall=1)," ",
                        format(round(tmin, digits=1), width=5, digits=1, nsmall=1)," ",
                        format(round(prcp, digits=1), width=5, digits=1, nsmall=1))
  
  readr::write_lines(c(header1,header2,header3,header4,header5,header6,daily.lines), path = out.file)
  
  return(out.file)
}