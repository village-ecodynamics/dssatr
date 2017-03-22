dssat_write_soils <- function(soils,
                              file.name = "NAMELESS",
                              output.dir = "./"){
  
  ## Build DSSAT series data
  #     ID_SOIL <- format(strtrim(paste(NRCS.components.unique$area,NRCS.components.unique$compname,sep='_'),width=10), width=10)
  ID_SOIL <- sprintf("%010d", soils$soils[[1]]$components$cokey)
  # ID_SOIL <- format(as.character(NRCS.components.unique$cokey), width=10)
  #   SLSOURCE <- format(strtrim(NRCS.components.unique$cokey, width=11), width=11)
  SLSOURCE <- format(strtrim(NRCS.comp$compname,width=11), width=11)
  SLTX <- format("RKB", width=5)
  SLDP <- format(NRCS.comp$hzdepb.r, width=5, digits=0)
  SLDESCRIP <- format(NRCS.comp$compname, width=50)
  SITE <- format(NRCS.comp$mukey, width=11)
  COUNTRY <- format("USA", width=11)
  LAT <- format("", width=8)
  LONG <- format("", width=8)
  SCSFAMILY <- format(NRCS.comp$taxpartsize, width=50)
  SCOM <- format(-99, width=5)
  SALB <- format(NRCS.comp$albedodry.r, width=5, digits=2)
  SLU1 <- format(-99, width=5)

  # Drainage class after DSSAT 4.5 manual Vol. 2, Section 1.4.3.2, Table 2
  SLDR <- as.character(NRCS.comp$drainagecl)
  SLDR[SLDR=="Excessively drained"] <- format(0.85, width=5)
  SLDR[SLDR=="Somewhat excessively drained"] <- format(0.75, width=5)
  SLDR[SLDR=="Well drained"] <- format(0.6, width=5)
  SLDR[SLDR=="Moderately well drained"] <- format(0.4, width=5)
  SLDR[SLDR=="Somewhat poorly drained"] <- format(0.25, width=5)
  SLDR[SLDR=="Poorly drained"] <- format(0.5, width=5)
  SLDR[SLDR=="Very poorly drained"] <- format(0.01, width=5)
  SLDR[SLDR==""] <- format(0.4, width=5)

  # Runoff potential after DSSAT 4.5 manual Vol. 2, Section 1.4.3.2, Table 2
  SLRO <- as.character(NRCS.comp$runoff)
  SLRO[SLRO %in% c("Negligible","Very low")] <- format(61, width=5)
  SLRO[SLRO %in% c("Low","Medium")] <- format(73, width=5)
  SLRO[SLRO %in% c("High")] <- format(81, width=5)
  SLRO[SLRO %in% c("Very high")] <- format(84, width=5)
  SLRO[SLRO==""] <- format(73, width=5)

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
    DSSAT.soil.series.names[i] <- format(DSSAT.soil.series.names[i],
                                         width = widths[i],
                                         justify = justifys[i])
  }
  DSSAT.soil.series.names <- gsub("SCSFAMILY ","SCS FAMILY",DSSAT.soil.series.names)

  ## Build DSSAT horizon data
  SLB <- format(NRCS.chorizon$hzdepb.r, width=5, digits=0)
  SLMH <- format(-99, width=5)
  SLLL <- format(round(NRCS.chorizon$wfifteenbar.r/100, digits=2), width=5, digits=2, nsmall=2)
  SLLL[SLLL=="   NA"] <- format(round(DSSAT.generic.soil.horizons$LL[match(NRCS.chorizon$TEXTURE[SLLL=="   NA"],as.character(DSSAT.generic.soil.horizons$SOIL.CLASS))], digits=2), width=5, digits=2, nsmall=2)

  SDUL <- format(round(NRCS.chorizon$wthirdbar.r/100, digits=2), width=5, digits=2, nsmall=2)
  SDUL[SDUL=="   NA"] <- format(round(DSSAT.generic.soil.horizons$DUL[match(NRCS.chorizon$TEXTURE[SDUL=="   NA"],as.character(DSSAT.generic.soil.horizons$SOIL.CLASS))], digits=2), width=5, digits=2, nsmall=2)

  SSAT <- format(round(NRCS.chorizon$wsatiated.r/100, digits=2), width=5, digits=2, nsmall=2)
  SSAT[SSAT=="   NA"] <- format(round(DSSAT.generic.soil.horizons$SAT[match(NRCS.chorizon$TEXTURE[SSAT=="   NA"],as.character(DSSAT.generic.soil.horizons$SOIL.CLASS))], digits=2), width=5, digits=2, nsmall=2)

  SSAT <- pmax(SSAT,SDUL)

  SRGF <- exp(-0.02 * ((NRCS.chorizon$hzdepb.r+NRCS.chorizon$hzdept.r)/2))
  SRGF[NRCS.chorizon$hzdepb.r<=15] <- 1
  SRGF <- format(round(SRGF, digits=2), width=5, digits=1, nsmall=1)

  SSKS <- format(round(NRCS.chorizon$ksat.r * (0.36), digits=1), width=5, digits=1, nsmall=1)
  SBDM <- format(-99, width=5)
  SLOC <- format(round(NRCS.chorizon$om.r, digits=2), width=5, digits=2, nsmall=2)
  SLCL <- format(round(NRCS.chorizon$claytotal.r, digits=1), width=5, digits=1, nsmall=1)
  SLSI <- format(round(NRCS.chorizon$silttotal.r,digits=1), width=5, digits=1, nsmall=1)
  SLCF <- format(-99, width=5)
  SLNI <- format(-99, width=5)
  SLHW <- format(round(NRCS.chorizon$ph1to1h2o.r, digits=1), width=5, digits=1, nsmall=1)
  SLHB <- format(round(NRCS.chorizon$ph01mcacl2.r, digits=1), width=5, digits=1, nsmall=1)
  SCEC <- format(round(NRCS.chorizon$cec7.r, digits=1), width=5, digits=1, nsmall=1)
  SLPX <- format(round(NRCS.chorizon$pbray1.r, digits=1), width=5, digits=1, nsmall=1)
  SLPT <- format(round(NRCS.chorizon$ptotal.r, digits=1), width=5, digits=1, nsmall=1)
  SLPO <- format(-99, width=5)
  SLCA <- format(round(NRCS.chorizon$caco3.r/1000, digits=1), width=5, digits=1, nsmall=1)
  SLAL <- format(round(NRCS.chorizon$extral.r, digits=1), width=5, digits=1, nsmall=1)
  SLFE <- format(round(NRCS.chorizon$freeiron.r, digits=1), width=5, digits=1, nsmall=1)
  SLMN <- format(-99, width=5)
  SLBS <- format(-99, width=5)
  SLPA <- format(-99, width=5)
  SLPB <- format(-99, width=5)
  SLKE <- format(-99, width=5)
  SLMG <- format(-99, width=5)
  SLNA <- format(-99, width=5)
  SLSU <- format(-99, width=5)
  SLEC <- format(round(NRCS.chorizon$ec.r, digits=1), width=5, digits=1, nsmall=1)
  DSSAT.soil.horizons <- data.frame(SLB,
                                    SLMH,
                                    SLLL,
                                    SDUL,
                                    SSAT,
                                    SRGF,
                                    SSKS,
                                    SBDM,
                                    SLOC,
                                    SLCL,
                                    SLSI,
                                    SLCF,
                                    SLNI,
                                    SLHW,
                                    SLHB,
                                    SCEC,
                                    SLPX,
                                    SLPT,
                                    SLPO,
                                    SLCA,
                                    SLAL,
                                    SLFE,
                                    SLMN,
                                    SLBS,
                                    SLPA,
                                    SLPB,
                                    SLKE,
                                    SLMG,
                                    SLNA,
                                    SLSU,
                                    SLEC)
  DSSAT.soil.horizons <- gsub(" NA", "-99", as.matrix(DSSAT.soil.horizons))
  DSSAT.soil.horizons <- gsub("NA ", "-99", as.matrix(DSSAT.soil.horizons))
  DSSAT.soil.horizons <- as.data.frame(DSSAT.soil.horizons, stringsAsFactors=F)
  DSSAT.soil.horizons.names <- format(names(DSSAT.soil.horizons),width=5, justify="right")

  dir.create(paste0(out.dir,"/",label),showWarnings = F, recursive = T)

  fileConn<-file(paste0(out.dir,"/",label,"/soil.sol"), open="wt")
  close(fileConn)

  fileConn<-file(paste0(out.dir,"/",label,"/soil.sol"), open="at")
  readr::write_lines(x = c(paste0("! Component-wise soils data for the ",
                                  label,
                                  " study area. ",
                                  nrow(NRCS.comp),
                                  " unique major components."),
                           ""),
                     path = fileConn)
  close(fileConn)

  for(i in 1:nrow(NRCS.comp)){
    blank <- ''
    horizons <- DSSAT.soil.horizons[NRCS.chorizon$cokey %in% NRCS.comp[i,"cokey"],]
    header1 <- paste0(c("*", DSSAT.soil.series[i,1]," ",
                       paste0(" ",DSSAT.soil.series[i,2:5])),
                     collapse='')
    header2 <- paste0(c("@", DSSAT.soil.series.names[6],
                       paste0(" ",DSSAT.soil.series.names[7:10])),
                     collapse='')
    data2 <- paste0(paste0(" ", DSSAT.soil.series[i,6:10]),
                    collapse='')
    header3 <- paste0(c("@", DSSAT.soil.series.names[11],
                        paste0(" ",DSSAT.soil.series.names[12:20])),
                      collapse='')
    data3 <- paste0(paste0(" ", DSSAT.soil.series[i,11:20]),
                    collapse='')
    header4 <- paste0(c("@",DSSAT.soil.horizons.names[1],
                        paste0(" ",DSSAT.soil.horizons.names[2:16])),
                      collapse='')
    data4 <- apply(horizons[,1:16],1,function(x){
      paste0(paste0(" ",x),
             collapse='')
    })

    fileConn <- file(paste0(out.dir,"/",label,"/soil.sol"), open="at")
    readr::write_lines(x = c(header1,
                             header2,
                             data2,
                             header3,
                             data3,
                             header4,
                             data4,
                             blank,
                             blank),
                       path = fileConn)
    close(fileConn)
  }

  return(NRCS.comp)
}
