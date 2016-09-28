## This script records the entire analysis reported in 

setwd("/Users/Bocinsky/Desktop/MAÍS/R/")

# devtools::install_github("bocinsky/FedData")
library(FedData)
pkg_test("ncdf")
pkg_test("parallel")
pkg_test("abind")
pkg_test("soiltexture")
pkg_test("rgeos")
pkg_test("data.table")
pkg_test("RColorBrewer")
pkg_test("classInt")

source("./prep_dssat_weather.R")
source("./prep_dssat_soils.R")
source("./run_dssat.R")

rasterOptions(maxmemory=2e+10)
rasterOptions(chunksize=2e+08)

VEPN <- spTransform(readOGR("../DATA/","CMV",stringsAsFactors=F),CRS("+proj=lcc +x_0=0 +y_0=0 +lon_0=-100 +lat_0=42.5 +lat_1=25 +lat_2=60 +ellps=WGS84"))
VEPS <- spTransform(readOGR("../DATA/","NRG",stringsAsFactors=F),CRS("+proj=lcc +x_0=0 +y_0=0 +lon_0=-100 +lat_0=42.5 +lat_1=25 +lat_2=60 +ellps=WGS84"))

templates <- list(VEPN,VEPS)
template.names <- c("VEPN","VEPS")
header.names <- c("VEPN","VEPS")
signals <- c("prcp","tmin","tmax","srad","dayl")


## Daymet data
extract_daymet <- function(signal, templates, template.names, daymet.dir, out.dir){
  if(length(template.names) != length(templates)){
    warning("Number of templates and template names do not match! Template names will be recycled.")
    template.names <- rep(template.names,length.out <- length(templates))
  }
  if(signal=="prcp"){
    datatype <- "Int16"
  }else{
    datatype <- "Float32"
  }
  junk <- lapply(template.names,function(name){dir.create(paste0(out.dir,"/",name,"/"), showWarnings = F)})
  all.files <- list.files(path=daymet.dir, pattern=signal, full.names=T)
  all.files <- sort(all.files)
  
  
  cl <- makeCluster(detectCores())
  junk <- parLapply(cl, all.files, function(daymet.file, templates, template.names, out.dir){
    for(i in 1:length(templates)){
      if(file.exists(paste0(out.dir,"/",template.names[[i]],"/",basename(daymet.file)))) next
      system(paste("gdal_translate -projwin",raster::xmin(templates[[i]]),raster::ymax(templates[[i]]),raster::xmax(templates[[i]]),raster::ymin(templates[[i]]),daymet.file,paste0(out.dir,"/",template.names[[i]],"/",basename(daymet.file)),'-of netCDF','-ot',datatype,'-strict','-co "FORMAT=NC4"','-co "COMPRESS=DEFLATE"','-co "ZLEVEL=9"','-q'))
      
    }
    return(NULL)
  }, templates=templates, template.names=template.names, out.dir=out.dir)
  stopCluster(cl)
  
}
junk <- lapply(c("prcp","tmin","tmax","srad","dayl"),extract_daymet,templates=list(VEPN,VEPS),template.names=c("VEPN","VEPS"),daymet.dir="/Volumes/BOCINSKY_DATA/DATA/DAYMET/",out.dir="/Users/Bocinsky/Desktop/MAÍS/DATA/DAYMET/")


dir.create("/Users/Bocinsky/Desktop/MAÍS/DATA/DSSAT/WEATHER", showWarnings = F, recursive = T)

for(i in length(templates)){
  data <- lapply(signals,function(signal){
    return(lapply(list.files(path=paste0("/Users/Bocinsky/Desktop/MAÍS/DATA/DAYMET/",template.names[[i]]), pattern=signal, full.names=T),raster::brick))
  })
  names(data) <- signals
  rast.temp <- data[[1]][[1]][[1]]
  
  dates <- as.Date(sapply(data[[1]],names),format="X%Y.%m.%d")
  dates <- paste0(format(dates,"%y"),format(dates,"%j"))
  
  data <- lapply(data,function(x){
    lapply(x,as.array)
  })
  data <- lapply(data,function(x){
    do.call(abind,x)
  })
  names(data) <- signals
  
  data[['drad']] <- data[['srad']] * data[['dayl']] / 1000000
  
  coords <- SpatialPoints(rast.temp)
  projection(coords) <- projection(rast.temp)
  coords <- spTransform(coords,CRS("+proj=longlat +ellps=WGS84"))@coords
  
  for(cell in 1:nrow(coords)){
    if(file.exists(paste0("/Users/Bocinsky/Desktop/MAÍS/DATA/DSSAT/WEATHER/",template.names[[i]],sprintf("%04d", cell),".WTH"))) next
    cat("\n",cell)
    prep_dssat_weather(out.dir="/Users/Bocinsky/Desktop/MAÍS/DATA/DSSAT/WEATHER", file.name=paste0(template.names[[i]],sprintf("%04d", cell)), dates=dates, coords=coords[cell,], tmin=data[['tmin']][rowFromCell(rast.temp,cell),colFromCell(rast.temp,cell),], tmax=data[['tmax']][rowFromCell(rast.temp,cell),colFromCell(rast.temp,cell),], prcp=data[['prcp']][rowFromCell(rast.temp,cell),colFromCell(rast.temp,cell),], srad=data[['drad']][rowFromCell(rast.temp,cell),colFromCell(rast.temp,cell),])
  }
  
}

NED <- lapply(1:length(templates),function(i){
  return(get_ned(template=templates[[i]], label=template.names[[i]], raw.dir = "../DATA/NED/RAW/", extraction.dir = "../DATA/NED/EXTRACTIONS/"))
})
names(NED) <- template.names

SSURGO <- lapply(1:length(templates),function(i){
  return(get_ssurgo(template=templates[[i]], label=template.names[[i]], raw.dir = "../DATA/SSURGO/RAW/", extraction.dir = "../DATA/SSURGO/EXTRACTIONS/"))
})
names(SSURGO) <- template.names

soils <- lapply(1:length(templates), function(i){
  prep_dssat_soils(ssurgo=SSURGO[[i]], label=template.names[[i]], out.dir="/Users/Bocinsky/Desktop/MAÍS/DATA/DSSAT/SOIL")
})
names(soils) <- template.names

rast.temps <- lapply(1:length(templates), function(i){
  out <- raster(raster(list.files(path=paste0("/Users/Bocinsky/Desktop/MAÍS/DATA/DAYMET/",template.names[[i]]), pattern=signals[[1]], full.names=T)[[1]]))
  out <- as(out,"SpatialPolygons")
  out <- spTransform(out,CRS(projection(SSURGO[[i]][['spatial']])))
  return(out)
})
names(rast.temps) <- template.names


soil.cells <- lapply(1:length(templates)[1], function(i){
  SSURGO[[i]][['spatial']] <- crop(SSURGO[[i]][['spatial']],rast.temps[[i]])
  
  # Split ssurgo soil polygons by daymet grid
  intersections <- gIntersects(SSURGO[[i]][['spatial']], rast.temps[[i]], byid=TRUE, returnDense=F)
  ssurgo.split <- lapply(1:length(SSURGO[[i]][['spatial']]),function(poly){
    out <- tryCatch(gIntersection(SSURGO[[i]][['spatial']][poly,], rast.temps[[i]][intersections[[poly]]],byid=TRUE, drop_lower_td=TRUE),error=function(e){as(SSURGO[[i]][['spatial']][poly,],'SpatialPolygons')})
    out.data <- data.frame(MUKEY=rep(SSURGO[[i]][['spatial']][poly,][["MUKEY"]],length(out)), CELL=intersections[[poly]])
    out <- SpatialPolygonsDataFrame(out,data=out.data, match.ID=F)
  })
  ssurgo.split <- lapply(1:length(ssurgo.split), function(x){spChFIDs(ssurgo.split[[x]], paste(x,as.character(ssurgo.split[[x]]$CELL),as.character(ssurgo.split[[x]]$MUKEY),sep="_"))})
  ssurgo.split <- do.call(rbind,ssurgo.split)
  
  ssurgo.split.data <- unique(ssurgo.split@data)
  
  # Get mukey components
  cokeys <- soils[[i]][,c("mukey",'cokey')]
  names(cokeys) <- c("MUKEY","COKEY")
  
  # Merge split data and mukey components
  ssurgo.split.data <- unique(merge(ssurgo.split.data, cokeys, all.x=T, all.y=F)[,c('CELL','COKEY')])
  ssurgo.split.data <- ssurgo.split.data[!is.na(ssurgo.split.data$COKEY),]
  ssurgo.split.data <- data.table(ssurgo.split.data)
  setkey(ssurgo.split.data,CELL)
  
  # Run DSSAT by cell
  split.cells <- split(unique(ssurgo.split.data[["CELL"]]),sample(rep(1:8,length.out=length(unique(ssurgo.split.data[["CELL"]])))))
  ssurgo.split.data.split <- lapply(split.cells,function(cells){ssurgo.split.data[CELL %in% cells]})
  
  cl <- makeCluster(detectCores())
  clusterExport(cl=cl, varlist=c("template.names", "ssurgo.split.data", "i", "data.table","run_dssat"))
  junk <- parLapply(cl,ssurgo.split.data.split,function(ssurgo.splitted){
    all.weather <- list.files("/Users/Bocinsky/Desktop/MAÍS/DATA/DSSAT/WEATHER", pattern=paste(paste0(template.names[[i]],sprintf("%04d", unique(ssurgo.splitted[["CELL"]]))),collapse="|"), full.names=T)
    working <- base::tempfile()
    dir.create(working, recursive=T, showWarnings=F)
    system(paste("cp -r ~/DSSAT46/BASE/* ",working,sep=''))
    dir.create(paste0(working,"/Weather"), recursive=T, showWarnings=F)
    file.copy(all.weather,paste0(working,"/Weather"))
    file.copy(paste0("/Users/Bocinsky/Desktop/MAÍS/DATA/DSSAT/SOIL/",template.names[[i]],"/soil.sol"), paste0(working,"/Soil"), overwrite=T, recursive=T)
    
    junk <- lapply(unique(ssurgo.splitted[["CELL"]]),function(j){
    
      weather.soil.dt <- ssurgo.split.data[CELL==j & !is.na(COKEY)]

      run_dssat(name=template.names[[i]], weather.soil.dt=weather.soil.dt, start.day="81001", dssat.dir=working, out.dir="/Users/Bocinsky/Desktop/MAÍS/OUTPUT/DSSAT/")
      
      return(NULL)
    })
  })
  stopCluster(cl)
  return(ssurgo.split)
})

soil.yields <- lapply(1:length(templates), function(i){
  all.data <- lapply(list.files("/Users/Bocinsky/Desktop/MAÍS/OUTPUT/DSSAT/", pattern=template.names[[i]], full.names=T),fread)
  all.data <- rbindlist(all.data)
  setkey(all.data,SOIL)
  cokeys <- soils[[i]][,c("mukey",'cokey','comppct.r')]
  names(cokeys) <- c("MUKEY","SOIL","PERCENT")
  cokeys <- data.table(cokeys)
  setkey(cokeys,SOIL)
  
  soils.percent <- cokeys[all.data]
  setkey(soils.percent,MUKEY)
  
  soils.percent[,PERCENT_YIELD:=YIELD*PERCENT/100]
  soils.percent[,CELL:=as.character(as.numeric(CELL))]
  soils.percent[,MUKEY:=as.character(as.numeric(MUKEY))]
  average.yields <- soils.percent[,sum(PERCENT_YIELD),by=.(MUKEY,CELL,YEAR,CULTIVAR)]
  average.cult <- average.yields[,mean(V1),by=.(MUKEY,CELL,CULTIVAR)]
  average.ts <- average.yields[,mean(V1),by=.(YEAR,CULTIVAR)]
  # max.yields <- soils.percent[,max(YIELD),by=.(MUKEY,CELL,YEAR,CULTIVAR)]
  
  soil.cells@data$MUKEY <- as.character(soil.cells@data$MUKEY)
  soil.cells@data$CELL <- as.character(soil.cells@data$CELL)
  
  phi <- (1+sqrt(5))/2
  mai <- c(0.25,0.25,0,0)
  fig.width <- 7.3
  fig.height <- fig.width/phi
  margins <- 0.5
  plot.height <- (fig.height-(margins*1.25))
  legend.cex <- 0.75
  
  quartz(file="../FIGURES/cult_ts.pdf", width=fig.width, height=fig.height, antialias=FALSE, bg="white", type='pdf', family="Gulim", pointsize=8, dpi=600)
  par(mai=c(margins,margins,margins*0.5,margins*0.5), xpd=T)
  
  plot(1, type='n', xaxs="i",yaxs="i", xlab="", ylab="", ylim=c(0,1000), xlim=c(1981,2013), axes=FALSE, main='', xpd=T)

  cols <- brewer.pal(length(unique(average.ts$CULTIVAR)),"Set1")

  for(j in 1:length(cols)){
    lines(y=average.ts[CULTIVAR==unique(average.ts$CULTIVAR)[[j]]]$V1,x=1981:2013,col=cols[[j]])
  }
  
  par(lend=1)
  legend("topleft", lwd=2, legend=unique(average.ts$CULTIVAR), col=cols,box.col=NA, bg=NA)

  axis(1, at=seq(1981,2013,1),labels=seq(1981,2013,1), cex.axis=0.8)
  axis(2, at=seq(0,1000,100), cex.axis=0.8)
  
  mtext("Yield (kg/ha)", side=2, line=2.25)
  mtext("Year AD", side=1, line=2.25)
  
  dev.off()
  
  
  
  for(cult in unique(average.cult$CULTIVAR)){
    phi <- (1+sqrt(5))/2
    mai <- c(0.25,0.25,0,0)
    fig.width <- 7.3
    fig.height <- fig.width/phi
    margins <- 0.5
    plot.height <- (fig.height-(margins*1.25))
    legend.cex <- 0.75
    
    quartz(file=paste0("../FIGURES/",cult,".png"), width=fig.width, height=fig.height, antialias=FALSE, bg="white", type='png', family="Gulim", pointsize=8, dpi=600)
    par(mai=c(margins,margins,margins,margins), xpd=T)
    
    spplot(merge(soil.cells,as.data.frame(average.cult[CULTIVAR==cult]),by=c("MUKEY","CELL"), all.x=F),"V1", at=seq(0,1500,length.out=1000), col.regions=colorRampPalette(brewer.pal(11,"RdYlGn"))(1000), col="transparent", main=cult)
    
    dev.off()
    
    }

  })
