prepDSSATSoils <- function(NRCS.rast.filled, label, nrcs.dir, out.dir){
  # Load all soil data for study area
  NRCS.polys <- readOGR(path.expand(paste(nrcs.dir,"/EXTRACTIONS/",label,"/vectors/",sep='')),"soils", verbose=F)
  NRCS.mapunit <- read.csv(paste(nrcs.dir,"/EXTRACTIONS/",label,"/tables/mapunit.csv", sep=''),stringsAsFactors=F)
  NRCS.muaggatt <- read.csv(paste(nrcs.dir,"/EXTRACTIONS/",label,"/tables/muaggatt.csv", sep=''),stringsAsFactors=F)
  NRCS.comp <- read.csv(paste(nrcs.dir,"/EXTRACTIONS/",label,"/tables/comp.csv", sep=''),stringsAsFactors=F)
  NRCS.chorizon <- read.csv(paste(nrcs.dir,"/EXTRACTIONS/",label,"/tables/chorizon.csv", sep=''),stringsAsFactors=F)
  
  # Get list of mukeys in study area
  mukey.areas <- NRCS.polys@data[NRCS.polys$ID %in% getValues(NRCS.rast.filled),]
  mukey.areas <- unique(mukey.areas)
  names(mukey.areas) <- c("mukey","area","id")
  
  # Get a list of unique soil components in the study area
  NRCS.components.all <- merge(mukey.areas,NRCS.comp,all.x=T, all.y=T)
  NRCS.components <- NRCS.components.all[NRCS.components.all$majcompflag=="Yes",]
  NRCS.components.slim <- NRCS.components[,c("area","id","mukey","cokey","compname","comppct_r")]
  NRCS.components.slim <- NRCS.components.slim[order(-NRCS.components$comppct_r),]
  NRCS.components.slim <- NRCS.components.slim[order(NRCS.components.slim$area,NRCS.components.slim$compname),]
  NRCS.components.areas <- NRCS.components.slim[,c("area","compname")]
  NRCS.components.unique <- NRCS.components.slim[!duplicated(NRCS.components.areas),]
  
  # Get the component and horizon data for the unique components
  NRCS.components.unique <- NRCS.components[NRCS.components$cokey %in% NRCS.components.unique$cokey,]
  NRCS.components.unique <- NRCS.components.unique[order(NRCS.components.unique$cokey),]
  NRCS.components.unique <- NRCS.components.unique[!is.na(NRCS.components.unique$albedodry_r),]
  NRCS.horizons.unique <- NRCS.chorizon[NRCS.chorizon$cokey %in% NRCS.components.unique$cokey,]
  NRCS.horizons.unique <- NRCS.horizons.unique[order(NRCS.horizons.unique$cokey,NRCS.horizons.unique$hzdepb_r),]
  NRCS.horizons.unique <- NRCS.horizons.unique[!is.na(NRCS.horizons.unique$claytotal_r),]
  NRCS.components.unique <- NRCS.components.unique[NRCS.components.unique$cokey %in% NRCS.horizons.unique$cokey,]
  
  # Sanitize the texture data
  NRCS.horizons.unique$silttotal_r <- 100-(NRCS.horizons.unique$claytotal_r + NRCS.horizons.unique$sandtotal_r)
  NRCS.horizons.unique[is.na(NRCS.horizons.unique$silttotal_r),]$silttotal_r <- (100-NRCS.horizons.unique[is.na(NRCS.horizons.unique$silttotal_r),]$claytotal_r)/2
  NRCS.horizons.unique[is.na(NRCS.horizons.unique$sandtotal_r),]$sandtotal_r <- (100-NRCS.horizons.unique[is.na(NRCS.horizons.unique$sandtotal_r),]$claytotal_r)/2
  NRCS.horizons.unique.textures <- NRCS.horizons.unique[,c("claytotal_r","sandtotal_r","silttotal_r")]
  names(NRCS.horizons.unique.textures) <- c("CLAY","SAND","SILT")
  NRCS.horizons.unique.textures[NRCS.horizons.unique.textures<0] <- 0
  NRCS.horizons.unique.textures[rowSums(NRCS.horizons.unique.textures) > 100,] <- NRCS.horizons.unique.textures[rowSums(NRCS.horizons.unique.textures) > 100,]/rowSums(NRCS.horizons.unique.textures[rowSums(NRCS.horizons.unique.textures) > 100,])*100
  NRCS.horizons.unique$TEXTURE <- TT.points.in.classes(NRCS.horizons.unique.textures, class.sys = "USDA.TT", PiC.type="t")
  NRCS.horizons.unique$TEXTURE <- gsub(",.*","",NRCS.horizons.unique$TEXTURE)
  DSSAT.generic.soil.horizons <- read.csv("../DSSAT_GENERIC_SOILS_HORIZON_HYDRO.csv")
  
  
  NRCS.components.mapping <- base::merge(NRCS.components.all[,c("mukey","area","id","compname","cokey","comppct_r_corr")],NRCS.components.unique[,c("area","compname","cokey")],by=c("area","compname"),all.x=T,all.y=F)
  NRCS.components.mapping <- NRCS.components.mapping[order(NRCS.components.mapping$compname),]
  NRCS.components.mapping <- NRCS.components.mapping[!is.na(NRCS.components.mapping$id),]
  names(NRCS.components.mapping) <- c("AREA","COMPNAME","MUKEY","ID","COKEY_ORIG","COMP_PCT","COKEY_MASTER")
  depths <- aggregate(NRCS.horizons.unique[,c("cokey","hzdepb_r")],by=list(NRCS.horizons.unique$cokey),max)[,c("cokey","hzdepb_r")]
  names(depths) <- c("COKEY_MASTER","DEPTH")
  NRCS.components.mapping <- merge(NRCS.components.mapping,depths,all.x=T,all.y=F)
  
  NRCS.components.unique <- NRCS.components.unique[NRCS.components.unique$cokey %in% NRCS.components.mapping$COKEY_MASTER,]
  NRCS.horizons.unique <- NRCS.horizons.unique[NRCS.horizons.unique$cokey %in% NRCS.components.mapping$COKEY_MASTER,]
  
  ## Build DSSAT series data
  #     ID_SOIL <- format(strtrim(paste(NRCS.components.unique$area,NRCS.components.unique$compname,sep='_'),width=10), width=10)
  ID_SOIL <- sprintf("%010d", NRCS.components.unique$cokey)  
  # ID_SOIL <- format(as.character(NRCS.components.unique$cokey), width=10)
  #   SLSOURCE <- format(strtrim(NRCS.components.unique$cokey, width=11), width=11)
  SLSOURCE <- format(strtrim(paste(NRCS.components.unique$area,NRCS.components.unique$compname,sep='_'),width=11), width=11)
  SLTX <- format("RKB", width=5)
  SLDP <- format(aggregate(NRCS.horizons.unique[,c("cokey","hzdepb_r")],by=list(NRCS.horizons.unique$cokey),max)[,c("hzdepb_r")], width=5, digits=0)
  SLDESCRIP <- format(NRCS.components.unique$compname, width=50)
  SITE <- format(NRCS.components.unique$area, width=11)
  COUNTRY <- format("USA", width=11)
  LAT <- format("", width=8)
  LONG <- format("", width=8)
  SCSFAMILY <- format(NRCS.components.unique$taxpartsize, width=50)
  SCOM <- format(-99, width=5)
  SALB <- format(NRCS.components.unique$albedodry_r, width=5, digits=2)
  SLU1 <- format(-99, width=5)
  
  # Drainage class after DSSAT 4.5 manual Vol. 2, Section 1.4.3.2, Table 2
  SLDR <- as.character(NRCS.components.unique$drainagecl)
  SLDR[SLDR=="Excessively drained"] <- format(0.85, width=5)
  SLDR[SLDR=="Somewhat excessively drained"] <- format(0.75, width=5)
  SLDR[SLDR=="Well drained"] <- format(0.6, width=5)
  SLDR[SLDR=="Moderately well drained"] <- format(0.4, width=5)
  SLDR[SLDR=="Somewhat poorly drained"] <- format(0.25, width=5)
  SLDR[SLDR=="Poorly drained"] <- format(0.5, width=5)
  SLDR[SLDR=="Very poorly drained"] <- format(0.01, width=5)
  SLDR[SLDR==""] <- format(0.4, width=5)
  
  # Runoff potential after DSSAT 4.5 manual Vol. 2, Section 1.4.3.2, Table 2
  SLRO <- as.character(NRCS.components.unique$runoff)
  SLRO[SLRO %in% c("Negligible","Very low")] <- format(61, width=5)
  SLRO[SLRO %in% c("Low","Medium")] <- format(73, width=5)
  SLRO[SLRO %in% c("High")] <- format(81, width=5)
  SLRO[SLRO %in% c("Very high")] <- format(84, width=5)
  
  SLNF <- format(1, width=5)
  SLPF <- format(1, width=5)
  SMHB <- format(-99, width=5)
  SMPX <- format(-99, width=5)
  SMKE <- format(-99, width=5)
  DSSAT.soil.series <- data.frame(ID_SOIL,SLSOURCE,SLTX,SLDP,SLDESCRIP,SITE,COUNTRY,LAT,LONG,SCSFAMILY,SCOM,SALB,SLU1,SLDR,SLRO,SLNF,SLPF,SMHB,SMPX,SMKE,stringsAsFactors = F)
  DSSAT.soil.series <- gsub(" NA", "-99", as.matrix(DSSAT.soil.series))
  DSSAT.soil.series <- gsub("NA ", "-99", as.matrix(DSSAT.soil.series))
  DSSAT.soil.series <- as.data.frame(DSSAT.soil.series, stringsAsFactors=F)
  DSSAT.soil.series.names <- names(DSSAT.soil.series)
  widths <- c(10,11,5,5,50,11,11,8,8,50,5,5,5,5,5,5,5,5,5,5)
  justifys <- c("left","left","right","right","left","left","left","right","right","left",rep("right",10))
  for(i in 1:ncol(DSSAT.soil.series)){
    DSSAT.soil.series.names[i] <- format(DSSAT.soil.series.names[i], width=widths[i], justify=justifys[i])
  }
  DSSAT.soil.series.names <- gsub("SCSFAMILY ","SCS FAMILY",DSSAT.soil.series.names)
  
  ## Build DSSAT horizon data
  SLB <- format(NRCS.horizons.unique$hzdepb_r, width=5, digits=0)
  SLMH <- format(-99, width=5)
  SLLL <- format(round(NRCS.horizons.unique$wfifteenbar_r/100, digits=2), width=5, digits=2, nsmall=2)
  SLLL[SLLL=="   NA"] <- format(round(DSSAT.generic.soil.horizons$LL[match(NRCS.horizons.unique$TEXTURE[SLLL=="   NA"],as.character(DSSAT.generic.soil.horizons$SOIL.CLASS))], digits=2), width=5, digits=2, nsmall=2)
  
  SDUL <- format(round(NRCS.horizons.unique$wthirdbar_r/100, digits=2), width=5, digits=2, nsmall=2)
  SDUL[SDUL=="   NA"] <- format(round(DSSAT.generic.soil.horizons$DUL[match(NRCS.horizons.unique$TEXTURE[SDUL=="   NA"],as.character(DSSAT.generic.soil.horizons$SOIL.CLASS))], digits=2), width=5, digits=2, nsmall=2)
  
  SSAT <- format(round(NRCS.horizons.unique$wsatiated_r/100, digits=2), width=5, digits=2, nsmall=2)
  SSAT[SSAT=="   NA"] <- format(round(DSSAT.generic.soil.horizons$SAT[match(NRCS.horizons.unique$TEXTURE[SSAT=="   NA"],as.character(DSSAT.generic.soil.horizons$SOIL.CLASS))], digits=2), width=5, digits=2, nsmall=2)
  
  SSAT <- pmax(SSAT,SDUL)
  
  SRGF <- exp(-0.02 * ((NRCS.horizons.unique$hzdepb_r+NRCS.horizons.unique$hzdept_r)/2))
  SRGF[NRCS.horizons.unique$hzdepb_r<=15] <- 1
  SRGF <- format(round(SRGF, digits=2), width=5, digits=1, nsmall=1)
  
  SSKS <- format(round(NRCS.horizons.unique$ksat_r * (0.36), digits=1), width=5, digits=1, nsmall=1)
  SBDM <- format(-99, width=5)
  SLOC <- format(round(NRCS.horizons.unique$om_r, digits=2), width=5, digits=2, nsmall=2)
  SLCL <- format(round(NRCS.horizons.unique$claytotal_r, digits=1), width=5, digits=1, nsmall=1)
  SLSI <- format(round(NRCS.horizons.unique$silttotal_r,digits=1), width=5, digits=1, nsmall=1)
  SLCF <- format(-99, width=5)
  SLNI <- format(-99, width=5)
  SLHW <- format(round(NRCS.horizons.unique$ph1to1h2o_r, digits=1), width=5, digits=1, nsmall=1)
  SLHB <- format(round(NRCS.horizons.unique$ph01mcacl2_r, digits=1), width=5, digits=1, nsmall=1)
  SCEC <- format(round(NRCS.horizons.unique$cec7_r, digits=1), width=5, digits=1, nsmall=1)
  SLPX <- format(round(NRCS.horizons.unique$pbray1_r, digits=1), width=5, digits=1, nsmall=1)
  SLPT <- format(round(NRCS.horizons.unique$ptotal_r, digits=1), width=5, digits=1, nsmall=1)
  SLPO <- format(-99, width=5)
  SLCA <- format(round(NRCS.horizons.unique$caco3_r/1000, digits=1), width=5, digits=1, nsmall=1)
  SLAL <- format(round(NRCS.horizons.unique$extral_r, digits=1), width=5, digits=1, nsmall=1)
  SLFE <- format(round(NRCS.horizons.unique$freeiron_r, digits=1), width=5, digits=1, nsmall=1)
  SLMN <- format(-99, width=5)
  SLBS <- format(-99, width=5)
  SLPA <- format(-99, width=5)
  SLPB <- format(-99, width=5)
  SLKE <- format(-99, width=5)
  SLMG <- format(-99, width=5)
  SLNA <- format(-99, width=5)
  SLSU <- format(-99, width=5)
  SLEC <- format(round(NRCS.horizons.unique$ec_r, digits=1), width=5, digits=1, nsmall=1)
  DSSAT.soil.horizons <- data.frame(SLB,SLMH,SLLL,SDUL,SSAT,SRGF,SSKS,SBDM,SLOC,SLCL,SLSI,SLCF,SLNI,SLHW,SLHB,SCEC,SLPX,SLPT,SLPO,SLCA,
                                    SLAL,SLFE,SLMN,SLBS,SLPA,SLPB,SLKE,SLMG,SLNA,SLSU,SLEC)
  DSSAT.soil.horizons <- gsub(" NA", "-99", as.matrix(DSSAT.soil.horizons))
  DSSAT.soil.horizons <- gsub("NA ", "-99", as.matrix(DSSAT.soil.horizons))
  DSSAT.soil.horizons <- as.data.frame(DSSAT.soil.horizons, stringsAsFactors=F)
  DSSAT.soil.horizons.names <- format(names(DSSAT.soil.horizons),width=5, justify="right")
  
  fileConn<-file(paste(out.dir,"soil.sol",sep=''), open="wt")
  close(fileConn)
  
  fileConn<-file(paste(out.dir,"soil.sol",sep=''), open="at")
  writeLines(c(paste("! Component-wise soils data for the ",label," study area. ",nrow(NRCS.components.unique)," unique major components.",sep=''),""), fileConn)
  close(fileConn)
  
  for(i in 1:nrow(NRCS.components.unique)){
    blank <- ''
    horizons <- DSSAT.soil.horizons[NRCS.horizons.unique$cokey %in% NRCS.components.unique[i,"cokey"],]
    header1 <- paste(c("*", DSSAT.soil.series[i,1]," ",paste(" ",DSSAT.soil.series[i,2:5], sep='')), collapse='')
    header2 <- paste(c("@", DSSAT.soil.series.names[6],paste(" ",DSSAT.soil.series.names[7:10],sep='')),collapse='')
    data2 <- paste(paste(" ", DSSAT.soil.series[i,6:10], sep=''), collapse='')
    header3 <- paste(c("@", DSSAT.soil.series.names[11], paste(" ",DSSAT.soil.series.names[12:20], sep='')), collapse='')
    data3 <- paste(paste(" ", DSSAT.soil.series[i,11:20], sep=''),collapse='')
    header4 <- paste(c("@",DSSAT.soil.horizons.names[1], paste(" ",DSSAT.soil.horizons.names[2:16], sep='')), collapse='')
    data4 <- apply(horizons[,1:16],1,function(x){paste(paste(" ",x, sep=''),collapse='')})
    #     header5 <- paste(c("@",DSSAT.soil.horizons.names[1], paste(" ",DSSAT.soil.horizons.names[17:31], sep='')), collapse='')
    #     data5 <- apply(horizons[,c(1,17:31)],1,function(x){paste(paste(" ",x, sep=''),collapse='')})
    #     
    fileConn<-file(paste(out.dir,"soil.sol",sep=''), open="at")
    writeLines(c(header1,header2,data2,header3,data3,header4,data4,blank,blank), fileConn)
    # writeLines(c(header1,header2,data2,header3,data3,header4,data4,header5,data5,blank,blank), fileConn)
    
    close(fileConn)
    #     write.fwf(daily.series,file=paste(out.dir,location,".WTH",sep=''), rownames=F, colnames=F, sep="  ", na="", append=T)
  }
  
  return(NRCS.components.mapping)
}
