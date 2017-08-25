dssat_write_weather_file <- function(coords,
                                     date,
                                     TMIN,
                                     TMAX,
                                     RAIN,
                                     SRAD,
                                     file.name = "NAMELESS",
                                     output.dir = "."){
  
  file.name <- paste0(sprintf("%-8s", file.name))
  out.file <- paste0(output.dir,"/",file.name,".WTH")
  
  date <- format(date,"%y%j")
  
  header1 <- paste0("*WEATHER DATA: ", file.name)
  header2 <- ""
  header3 <- "@ INSI      LAT     LONG"
  header4 <- paste0("  INSI  ",
                    format(round(coords[2], digits=3), width=7, digits=3, nsmall=3)," ",
                    format(round(coords[1], digits=3), width=8, digits=3, nsmall=3))
  header5 <- ""
  header6 <- "@DATE  SRAD  TMAX  TMIN  RAIN"
  daily.lines <- paste0(date," ",
                        format(round(SRAD %>% as.numeric(), digits=1), width=5, digits=1, nsmall=1)," ",
                        format(round(TMAX %>% as.numeric(), digits=1), width=5, digits=1, nsmall=1)," ",
                        format(round(TMIN %>% as.numeric(), digits=1), width=5, digits=1, nsmall=1)," ",
                        format(round(RAIN %>% as.numeric(), digits=1), width=5, digits=1, nsmall=1))
  
  readr::write_lines(c(header1,header2,header3,header4,header5,header6,daily.lines), path = out.file)
  
  return(out.file)
}
