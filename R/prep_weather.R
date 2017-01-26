prep_weather <- function(daymet, label, output.dir){
  dir.create(output.dir, recursive = T, showWarnings = F)

  daymet %<>% lapply(FUN = function(x){
    if(raster::inMemory(x)) return(x)
    return(raster::readAll(x))
  })

  # Calculate daily solar accumulated radiation
  daymet$drad <- daymet$srad * daymet$dayl / 1000000

  # A template raster from the Daymet data
  rast.temp <- daymet[[1]][[1]][[1]]

  coords <- sp::SpatialPoints(rast.temp)
  raster::projection(coords) <- raster::projection(rast.temp)
  coords <- sp::spTransform(coords,"+proj=longlat +ellps=WGS84")@coords
  dates <- lubridate::as_date(names(daymet$prcp),format="X%Y.%m.%d")
  dates <- paste0(format(dates,"%y"),format(dates,"%j"))
  rowcols <- cbind(raster::rowFromCell(rast.temp,1:nrow(coords)),
                   raster::colFromCell(rast.temp,1:nrow(coords)))

  files <- foreach::foreach(cell = 1:raster::ncell(rast.temp)) %do% {
    file.name <- paste0(sprintf("%08d", cell))
    if(file.exists(paste0(output.dir,"/",file.name,".WTH"))) return(paste0(output.dir,"/",file.name,".WTH"))
    # cat("\n",cell)


    this.coords <- coords[cell,]
    tmin <- daymet$tmin[cell][1,]
    tmax <- daymet$tmax[cell][1,]
    prcp <- daymet$prcp[cell][1,]
    srad <- daymet$drad[cell][1,]

    header1 <- paste0("*WEATHER DATA: ", file.name)
    header2 <- ""
    header3 <- "@ INSI      LAT     LONG"
    header4 <- paste0("  CELL  ",
                      format(round(this.coords[[2]], digits=3), width=7, digits=3, nsmall=3)," ",
                      format(round(this.coords[[1]], digits=3), width=8, digits=3, nsmall=3))
    header5 <- ""
    header6 <- "@DATE  SRAD  TMAX  TMIN  RAIN"
    daily.lines <- paste0(dates," ",
                          format(round(drad, digits=1), width=5, digits=1, nsmall=1)," ",
                          format(round(tmax, digits=1), width=5, digits=1, nsmall=1)," ",
                          format(round(tmin, digits=1), width=5, digits=1, nsmall=1)," ",
                          format(round(prcp, digits=1), width=5, digits=1, nsmall=1))

    fileConn<-file(paste0(output.dir,"/",file.name,".WTH"))
    writeLines(c(header1,header2,header3,header4,header5,header6,daily.lines), fileConn)
    close(fileConn)
    return(paste0(output.dir,"/",file.name,".WTH"))
  }
  return(files)
}
