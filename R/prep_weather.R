prep_weather <- function(out.dir, file.name, dates, coords, tmin, tmax, prcp, srad){

  header1 <- paste0("*WEATHER DATA: ", file.name)
  header2 <- ""
  header3 <- "@ INSI      LAT     LONG"
  header4 <- paste0("  CELL  ",
                    format(round(coords[[2]], digits=3), width=7, digits=3, nsmall=3)," ",
                    format(round(coords[[1]], digits=3), width=8, digits=3, nsmall=3))
  header5 <- ""
  header6 <- "@DATE  SRAD  TMAX  TMIN  RAIN"
  daily.lines <- paste0(dates," ",
                        format(round(srad, digits=1), width=5, digits=1, nsmall=1)," ",
                        format(round(tmax, digits=1), width=5, digits=1, nsmall=1)," ",
                        format(round(tmin, digits=1), width=5, digits=1, nsmall=1)," ",
                        format(round(prcp, digits=1), width=5, digits=1, nsmall=1))

  fileConn<-file(paste0(out.dir,"/",file.name,".WTH"))
  writeLines(c(header1,header2,header3,header4,header5,header6,daily.lines), fileConn)
  close(fileConn)
  return(TRUE)
}
