## This function reads in several gridded datasets necessary for running a
## spatial DSSAT model, selects the resolution to run the model at based on
## the lowest resolution dataset (PRISM), aggregates all other datasets down to that resolution,
## and preps and runs the simulation on a per-cell basis.
dssat_spatial <- function(ssurgo, nhd, ned, weather, raw.dir = "./DATA/", force.redo = FALSE){
  raw.dir <- path.expand(raw.dir)

  cat("Extracting and loading the NRCS soils data \n")
  SSURGO <- FedData::get_ssurgo(template=template, label=label, raw.dir=paste(raw.dir,"/RAW/SSURGO/",sep=''), extraction.dir=paste(raw.dir,"/EXTRACTIONS/SSURGO/",sep=''), force.redo=force.redo)

  cat("Extracting and loading the NHD hydrology data \n")
  NHD <- FedData::get_nhd(template=template, label=label, raw.dir=paste(raw.dir,"/RAW/NHD/",sep=''), force.redo=force.redo)

  cat("Extracting and loading the NED elevation data, and draining the dammed DEM \n")
  NED <- FedData::extract_ned(template=template, label=label, raw.dir=paste(raw.dir,"/NED/",sep=''), res="1", drain=T, NHD.raw.dir=paste(raw.dir,"/NHD/",sep=''), NRCS.raw.dir=paste(raw.dir,"/NRCS/",sep=''), force.redo=force.redo)

  cat("Extracting and loading the monthly PRISM data: TMIN \n")
  PRISM.monthly.tmin <- getPRISM_MONTHLYData(template=template, type='tmin', out.dir=paste(raw.dir,"/PRISM/EXTRACTIONS/", sep=''), monthly.dir=paste(raw.dir,"/PRISM/LT81_800M/",sep=''), label=label, force.redo=F)
  PRISM.monthly.tmin <- annualizePRISM_MONTHLY(prism.brick=PRISM.monthly.tmin, months=c(5,6,7,8,9), fun="mean")
  PRISM.monthly.tmin <- subset(PRISM.monthly.tmin,grep(paste(1901:2012,collapse="|"),names(PRISM.monthly.tmin)))

  #   out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep='')
  #   template.poly.latlon <- SPDFfromPolygon(spTransform(polygonFromExtent(template),CRS("+proj=longlat +ellps=GRS80")))
  #   PRISM.monthly.tmin <- crop(PRISM.monthly.tmin,template.poly.latlon,snap='out')
  #   writeGDAL(as(PRISM.monthly.tmin, "SpatialGridDataFrame"), paste(out.dir,"CCAC","/","tmin","_MONTHLY_FINAL_800.tif", sep=''), drivername="GTiff", type="Float32", options=c("INTERLEAVE=PIXEL", "COMPRESS=DEFLATE", "ZLEVEL=9"))
  #   write.csv(names(PRISM.monthly.tmin),paste(out.dir,"CCAC","/","tmin","_MONTHLY_FINAL_800_BANDS.csv", sep=''), row.names=F, col.names=F)
  #
  #   PRISM.monthly.tmin <- brick(readGDAL(paste("../OUTPUT/",label,"_tmin_grow_retro.tif", sep=''), silent = TRUE))
  #   names(PRISM.monthly.tmin) <- 1:2000


  cat("Extracting and loading the monthly PRISM data: TMAX \n")
  PRISM.monthly.tmax <- getPRISM_MONTHLYData(template=template, type='tmax', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), monthly.dir=paste(raw.dir,"PRISM/LT81_800M/",sep=''), label=label, force.redo=F)
  PRISM.monthly.tmax <- annualizePRISM_MONTHLY(prism.brick=PRISM.monthly.tmax, months=c(5,6,7,8,9), fun="mean")
  PRISM.monthly.tmax <- subset(PRISM.monthly.tmax,grep(paste(1901:2012,collapse="|"),names(PRISM.monthly.tmax)))

  #   PRISM.monthly.tmax <- getPRISM_MONTHLYData(template=template, type='tmax', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), monthly.dir=paste(raw.dir,"PRISM/LT81_800M/",sep=''), label=label, force.redo=F)
  #   out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep='')
  #   template.poly.latlon <- SPDFfromPolygon(spTransform(polygonFromExtent(template),CRS("+proj=longlat +ellps=GRS80")))
  #   PRISM.monthly.tmax <- crop(PRISM.monthly.tmax,template.poly.latlon,snap='out')
  #   writeGDAL(as(PRISM.monthly.tmax, "SpatialGridDataFrame"), paste(out.dir,"CCAC","/","tmax","_MONTHLY_FINAL_800.tif", sep=''), drivername="GTiff", type="Float32", options=c("INTERLEAVE=PIXEL", "COMPRESS=DEFLATE", "ZLEVEL=9"))
  #   write.csv(names(PRISM.monthly.tmax),paste(out.dir,"CCAC","/","tmax","_MONTHLY_FINAL_800_BANDS.csv", sep=''), row.names=F, col.names=F)
  #
  #   PRISM.monthly.tmax <- brick(readGDAL(paste("../OUTPUT/",label,"_tmax_grow_retro.tif", sep=''), silent = TRUE))
  #   names(PRISM.monthly.tmax) <- 1:2000

  cat("Extracting and loading the monthly PRISM data: PRCP \n")
  PRISM.monthly.prcp <- getPRISM_MONTHLYData(template=template, type='ppt', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), monthly.dir=paste(raw.dir,"PRISM/LT81_800M/",sep=''), label=label, force.redo=F)
  PRISM.monthly.prcp <- annualizePRISM_MONTHLY(prism.brick=PRISM.monthly.prcp, months=c(5,6,7,8,9), fun="sum")
  PRISM.monthly.prcp <- subset(PRISM.monthly.prcp,grep(paste(1901:2012,collapse="|"),names(PRISM.monthly.prcp)))

  #   PRISM.monthly.prcp <- getPRISM_MONTHLYData(template=template, type='ppt', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), monthly.dir=paste(raw.dir,"PRISM/LT81_800M/",sep=''), label=label, force.redo=F)
  #   out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep='')
  #   template.poly.latlon <- SPDFfromPolygon(spTransform(polygonFromExtent(template),CRS("+proj=longlat +ellps=GRS80")))
  #   PRISM.monthly.prcp <- crop(PRISM.monthly.prcp,template.poly.latlon,snap='out')
  #   writeGDAL(as(PRISM.monthly.prcp, "SpatialGridDataFrame"), paste(out.dir,"CCAC","/","ppt","_MONTHLY_FINAL_800.tif", sep=''), drivername="GTiff", type="Float32", options=c("INTERLEAVE=PIXEL", "COMPRESS=DEFLATE", "ZLEVEL=9"))
  #   write.csv(names(PRISM.monthly.prcp),paste(out.dir,"CCAC","/","ppt","_MONTHLY_FINAL_800_BANDS.csv", sep=''), row.names=F, col.names=F)
  #

  #   PRISM.monthly.prcp <- brick(readGDAL(paste("../OUTPUT/",label,"_prcp_grow_retro.tif", sep=''), silent = TRUE))
  #   names(PRISM.monthly.prcp) <- 1:2000

  cat("Extracting and loading the daily PRISM data: TMIN \n")
  #   PRISM.daily.tmin <- getPRISM_DAILYData(template=template, date.format="Y%Y.M%m.D%d", type='tmin', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), daily.dir=paste(raw.dir,"PRISM/DAILY/",sep=''), label=label, year.range=1981:2013, force.redo=F)
  PRISM.daily.tmin <- getPRISM_DAILYData(template=template, type='tmin', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), daily.dir=paste(raw.dir,"PRISM/DAILY/",sep=''), label=label, year.range=1981:2013, force.redo=F)
  PRISM.daily.tmin <- subset(PRISM.daily.tmin,grep("X1985",names(PRISM.daily.tmin)))

  #   PRISM.daily.tmin <- getPRISM_DAILYData(template=template, date.format="Y%Y.M%m.D%d", type='tmin', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), daily.dir=paste(raw.dir,"PRISM/DAILY/",sep=''), label=label, year.range=1981:2013, force.redo=F)
  #   out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep='')
  #   template.poly.latlon <- SPDFfromPolygon(spTransform(polygonFromExtent(template),CRS("+proj=longlat +ellps=GRS80")))
  #   PRISM.daily.tmin <- crop(PRISM.daily.tmin,template.poly.latlon,snap='out')
  #   writeGDAL(as(PRISM.daily.tmin, "SpatialGridDataFrame"), paste(out.dir,"CCAC","/","tmin","_DAILY_FINAL_800.tif", sep=''), drivername="GTiff", type="Float32", options=c("INTERLEAVE=PIXEL", "COMPRESS=DEFLATE", "ZLEVEL=9"))
  #   write.csv(names(PRISM.daily.tmin),paste(out.dir,"CCAC","/","tmin","_DAILY_FINAL_800_BANDS.csv", sep=''), row.names=F, col.names=F)
  #

  cat("Extracting and loading the daily PRISM data: TMAX \n")
  #   PRISM.daily.tmax <- getPRISM_DAILYData(template=template, date.format="Y%Y.M%m.D%d", type='tmax', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), daily.dir=paste(raw.dir,"PRISM/DAILY/",sep=''), label=label, year.range=1981:2013, force.redo=F)
  PRISM.daily.tmax <- getPRISM_DAILYData(template=template, type='tmax', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), daily.dir=paste(raw.dir,"PRISM/DAILY/",sep=''), label=label, year.range=1981:2013, force.redo=F)
  PRISM.daily.tmax <- subset(PRISM.daily.tmax,grep("X1985",names(PRISM.daily.tmax)))

  #   PRISM.daily.tmax <- getPRISM_DAILYData(template=template, date.format="Y%Y.M%m.D%d", type='tmax', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), daily.dir=paste(raw.dir,"PRISM/DAILY/",sep=''), label=label, year.range=1981:2013, force.redo=F)
  #   out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep='')
  #   template.poly.latlon <- SPDFfromPolygon(spTransform(polygonFromExtent(template),CRS("+proj=longlat +ellps=GRS80")))
  #   PRISM.daily.tmax <- crop(PRISM.daily.tmax,template.poly.latlon,snap='out')
  #   writeGDAL(as(PRISM.daily.tmax, "SpatialGridDataFrame"), paste(out.dir,"CCAC","/","tmax","_DAILY_FINAL_800.tif", sep=''), drivername="GTiff", type="Float32", options=c("INTERLEAVE=PIXEL", "COMPRESS=DEFLATE", "ZLEVEL=9"))
  #   write.csv(names(PRISM.daily.tmax),paste(out.dir,"CCAC","/","tmax","_DAILY_FINAL_800_BANDS.csv", sep=''), row.names=F, col.names=F)

  cat("Extracting and loading the daily PRISM data: PRCP \n")
  #   PRISM.daily.prcp <- getPRISM_DAILYData(template=template, date.format="Y%Y.M%m.D%d", type='ppt', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), daily.dir=paste(raw.dir,"PRISM/DAILY/",sep=''), label=label, year.range=1981:2013, force.redo=F)
  PRISM.daily.prcp <- getPRISM_DAILYData(template=template, type='ppt', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), daily.dir=paste(raw.dir,"PRISM/DAILY/",sep=''), label=label, year.range=1981:2013, force.redo=F)
  PRISM.daily.prcp <- subset(PRISM.daily.prcp,grep("X1985",names(PRISM.daily.prcp)))

  #   PRISM.daily.prcp <- getPRISM_DAILYData(template=template, date.format="Y%Y.M%m.D%d", type='ppt', out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep=''), daily.dir=paste(raw.dir,"PRISM/DAILY/",sep=''), label=label, year.range=1981:2013, force.redo=F)
  #   out.dir=paste(raw.dir,"PRISM/EXTRACTIONS/", sep='')
  #   template.poly.latlon <- SPDFfromPolygon(spTransform(polygonFromExtent(template),CRS("+proj=longlat +ellps=GRS80")))
  #   PRISM.daily.prcp <- crop(PRISM.daily.prcp,template.poly.latlon,snap='out')
  #   writeGDAL(as(PRISM.daily.prcp, "SpatialGridDataFrame"), paste(out.dir,"CCAC","/","ppt","_DAILY_FINAL_800.tif", sep=''), drivername="GTiff", type="Float32", options=c("INTERLEAVE=PIXEL", "COMPRESS=DEFLATE", "ZLEVEL=9"))
  #   write.csv(names(PRISM.daily.prcp),paste(out.dir,"CCAC","/","ppt","_DAILY_FINAL_800_BANDS.csv", sep=''), row.names=F, col.names=F)

  ## Rasterize the NRCS mukey polygons, and fill them using the Reservoir filler, using the drained DEMs
  ## This takes a long time (like an hour for the VEPIIN area)
  cat("Rasterizing the NRCS soils data \n")
  if(!file.exists(paste(raw.dir,"NRCS/EXTRACTIONS/",label,"/RASTERIZED_MUKEYS_1arcsec.tif",sep='')) | force.redo){
    NRCS.rast <- raster::rasterize(NRCS,NED,field="ID", na.rm=T)
    writeGDAL(as(NRCS.rast, "SpatialGridDataFrame"),paste(raw.dir,"NRCS/EXTRACTIONS/",label,"/RASTERIZED_MUKEYS_1arcsec.tif",sep=''), drivername="GTiff", type="Int16", mvFlag=-32768, options=c("INTERLEAVE=PIXEL", "COMPRESS=DEFLATE", "ZLEVEL=9"))
  }
  NRCS.rast <- raster(paste(raw.dir,"NRCS/EXTRACTIONS/",label,"/RASTERIZED_MUKEYS_1arcsec.tif",sep=''))

  # Load reservoir and dam shapefiles
  cat("Filling NRCS reservoir and dam soils \n")
  if(file.exists(paste(raw.dir,"NHD/EXTRACTIONS/",label,"/vectors/Reservoirs.shp", sep=''))){
    reservoirs <- readOGR(paste(raw.dir,"NHD/EXTRACTIONS/",label,"/vectors", sep=''),"Reservoirs", verbose=F)
    dams <- readOGR(paste(raw.dir,"NRCS/EXTRACTIONS/",label,"/vectors", sep=''),"Dams", verbose=F)

    # Get raster cells in reservoirs and dams, and set them to NA
    projection(dams) <- projection(reservoirs)
    bad.data.vect <- gUnion(dams,reservoirs)
    bad.data.rast <- raster::extract(NRCS.rast, bad.data.vect, cellnumbers=T)
    NRCS.rast[bad.data.rast[[1]][,1]] <- NA

    # Fill the missing reservoir/dam soils using linear discriminant analysis
    NRCS.rast.filled <- fillReservoirSoils(gapped.soil.raster=NRCS.rast, dem.raster=NED,  label=label, raw.dir=paste(raw.dir,"NRCS/",sep=''), force.redo=force.redo)
    projection(NRCS.rast.filled) <- projection(PRISM.monthly.tmin)
  }else{
    NRCS.rast.filled <- NRCS.rast
  }


  cat("Prepping DSSAT soils data \n")
  NRCS.components.mapping <- prepDSSATSoils(NRCS.rast.filled=NRCS.rast.filled, label=label, nrcs.dir=paste(raw.dir,"NRCS/",sep=''), out.dir="../DSSAT45/")

  # Get a raster of PRISM cell numbers
  PRISM.cells <- setValues(PRISM.monthly.tmin[[1]],1:ncell(PRISM.monthly.tmin))
  PRISM.cells.NRCS <- raster::resample(PRISM.cells, NRCS.rast.filled, method="ngb")
  # Generate table of soil IDs versus PRISM cells
  PRISM.soils <- data.frame(PRISM_CELL=getValues(PRISM.cells.NRCS),ID=getValues(NRCS.rast.filled))
  PRISM.soils <- unique(PRISM.soils)
  PRISM.soils <- PRISM.soils[complete.cases(PRISM.soils),]
  PRISM.soils <- PRISM.soils[order(PRISM.soils$PRISM_CELL),]
  PRISM.soils <- base::merge(PRISM.soils,unique(NRCS.components.mapping),by="ID",all.x=T,all.y=F, sort=F)
  PRISM.soils <- PRISM.soils[order(PRISM.soils$PRISM_CELL),]
  PRISM.soils <- PRISM.soils[,c("PRISM_CELL","COKEY_MASTER","DEPTH")]
  PRISM.soils <- unique(PRISM.soils)
  PRISM.soils <- PRISM.soils[!is.na(PRISM.soils$COKEY_MASTER),]

  # Resample daily PRISM down to Monthly cells (800m resolution)
  PRISM.daily.tmin <- raster::resample(PRISM.daily.tmin, PRISM.monthly.tmin, method="bilinear")
  PRISM.daily.tmax <- raster::resample(PRISM.daily.tmax, PRISM.monthly.tmax, method="bilinear")
  PRISM.daily.prcp <- raster::resample(PRISM.daily.prcp, PRISM.monthly.prcp, method="bilinear")

  locations <- coordinates(PRISM.monthly.tmin)
  recon.years <- as.numeric(gsub("X","",names(PRISM.monthly.tmin)))

  repeats <- rep(1:8,length.out=nrow(locations))
  repeats <- repeats[order(repeats)]

  locations.split <- split(as.data.frame(locations), repeats)
  rm(locations)

  cells.out <- mclapply(locations.split,FUN=function(locations,...){
    directory <- paste("../DSSAT45/TEMP",rownames(locations)[1],"/",sep='')
    dir.create(directory)
    system(paste("cp ../DSSAT45/BASE/* ",directory,sep=''))
    system(paste("cp ../DSSAT45/soil.sol ",directory,sep=''))

    out <- vector('list',nrow(locations))
    names(out) <- rownames(locations)
    for(i in 1:nrow(locations)){
      cell <- as.numeric(rownames(locations)[i])
      cat("Running DSSAT CERES-maize on cell ", cell, " of ", ncell(PRISM.monthly.tmin),"\n", sep="")
      if(length(which(PRISM.soils$PRISM_CELL==cell))==0){
        out[[i]] <- NA
      }
      prepDSSATWeather(out.dir=directory,
                       coords=coordinates(PRISM.monthly.tmin)[cell,],
                       location=paste("CEL",formatC(cell, width = 5, format = "d", flag = "0"), sep=''),
                       annual.series.tmin=as.numeric(PRISM.monthly.tmin[cell]),
                       annual.series.tmax=as.numeric(PRISM.monthly.tmax[cell]),
                       annual.series.prcp=as.numeric(PRISM.monthly.prcp[cell]),
                       daily.series.tmin=as.numeric(PRISM.daily.tmin[cell]),
                       daily.series.tmax=as.numeric(PRISM.daily.tmax[cell]),
                       daily.series.prcp=as.numeric(PRISM.daily.prcp[cell]),
                       recon.days=c(121:273),
                       recon.years=recon.years)

      cell.soils <- PRISM.soils[PRISM.soils$PRISM_CELL==cell,"COKEY_MASTER"]
      depths <- PRISM.soils[PRISM.soils$PRISM_CELL==cell,"DEPTH"]
      prepDSSATParameters(location=cell,local.soils=cell.soils, soil.depths=depths, out.dir=directory, recon.years=recon.years)

      setwd(directory)
      output <- system(paste("./DSCSM045.EXE A CEL",formatC(cell, width = 5, format = "d", flag = "0"),".MZX",sep=''), intern=T, ignore.stderr = T)
      unlink(paste("CEL",formatC(cell, width = 5, format = "d", flag = "0"),".WTH", sep=''), force=T)
      unlink(paste("CEL",formatC(cell, width = 5, format = "d", flag = "0"),".MZX", sep=''), force=T)
      setwd("../../R_FINAL")

      output <- output[!(output %in% "")]
      output <- output[grep("RUN",output,invert=T)]
      output <- output[grep("dap",output,invert=T)]
      output <- output[grep("Crop",output,invert=T)]
      output <- as.numeric(substr(output,25,30))
      output[output<0] <- 0
      output <- split(output, rep(as.character(cell.soils),each=length(recon.years)))

      out[[i]] <- output
    }
    unlink(directory, recursive=T, force=T)
    return(out)
  },mc.cores=8)
  unlink("../DSSAT45/soil.sol", force=T)

  cells.out <- unlist(cells.out, recursive=F, use.names = F)

  save(cells.out,file=paste("../OUTPUT/",label,"_DSSAT_RAW.Rdata",sep=''))

  load(paste("../OUTPUT/",label,"_DSSAT_RAW.Rdata",sep=''))

  PRISM.soils.connection <- data.frame(CELL=1:ncell(PRISM.cells.NRCS),PRISM_CELL=getValues(PRISM.cells.NRCS),SOIL_ID=getValues(NRCS.rast.filled))

  # Create an empty list for data
  production.out.list <- vector('list',ncell(PRISM.cells.NRCS))
  cells <- PRISM.soils.connection[is.na(PRISM.soils.connection$SOIL_ID),"CELL"]
  production.out.list[cells] <- rep(list(rep(NA,length(recon.years))),length(cells))

  for(i in unique(PRISM.soils.connection$PRISM_CELL)){
    PRISM.soils.connection.sub <- PRISM.soils.connection[PRISM.soils.connection$PRISM_CELL==i,]
    unique.ids <- unique(PRISM.soils.connection.sub$SOIL_ID)

    for(j in unique.ids){
      if(is.na(j)){next}

      mukey.soils <- NRCS.components.mapping[NRCS.components.mapping$ID==j,]

      soils <- cells.out[[i]]
      soils <- as.data.frame(do.call('cbind',soils))

      soils <- soils[,as.character(mukey.soils$COKEY_MASTER[mukey.soils$COKEY_MASTER %in% as.numeric(names(soils))])]
      weights <- mukey.soils[mukey.soils$COKEY_MASTER %in% as.numeric(names(soils)),"COMP_PCT"]

      production <- rowSums(t(t(soils) * weights))

      cells <- PRISM.soils.connection[PRISM.soils.connection$SOIL_ID==j & PRISM.soils.connection$PRISM_CELL==i,"CELL"]
      cells <- cells[!is.na(cells)]

      temp <- lapply(cells,function(x,...){production})
      production.out.list[cells] <- rep(list(production),length(cells))

    }
  }

  production.out.matrix <- do.call('rbind',production.out.list)
  # Create empty raster stack
  production.out.brick <- brick(PRISM.cells.NRCS,values=F,nl=length(recon.years))
  production.out.brick <- setValues(production.out.brick,production.out.matrix)
  names(production.out.brick) <- recon.years
  #   cols <- rev(terrain.colors(nb))
  #   production.out.brick.coarse <- aggregate(production.out.brick,fact=6)

  #   cat("DSSAT ran on ",nrow(locations)," cells.", sep='')
  return(production.out.brick)
}
