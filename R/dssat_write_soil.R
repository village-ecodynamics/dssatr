dssat_write_soil <- function(soil,
                             output_dir = ".",
                             append = FALSE){
  
  if(!inherits(soil,
               what = "soil"))
    stop("dssat_write_soil() requires an S3 object of class 'soil' from the dssatr library.")
  
  if(!append)
    unlink(stringr::str_c(output_dir,"/soil.sol"))
  
  if(!file.exists(stringr::str_c(output_dir,"/soil.sol"))){
    fileConn <- file(stringr::str_c(output_dir,"/soil.sol"),
                     open = "wt")
    close(fileConn)
  }

  soil$components %<>%
    dplyr::mutate(ID_SOIL = sprintf("%010s",ID_SOIL),
                  SLSOURCE = format(strtrim(SLSOURCE,width=11), width=11),
                  SLTX = format("", width=5),
                  SLDP = format(SLDP, width=5, digits=0),
                  SLDESCRIP = format(SLDESCRIP, width=50),
                  SITE = format(SITE, width=11),
                  COUNTRY = format(COUNTRY, width=11),
                  LAT = format(LAT, width=8),
                  LONG = format(LONG, width=8),
                  SCSFAMILY = format(SCSFAMILY, width=50),
                  SCOM = format(-99, width=5),
                  SALB = format(SALB, width=5, digits=2),
                  SLU1 = format(-99, width=5),
                  SLDR = format(SLDR, width=5),
                  SLRO = format(SLRO, width=5),
                  SLNF = format(1, width=5),
                  SLPF = format(1, width=5),
                  SMHB = format(-99, width=5),
                  SMPX = format(-99, width=5),
                  SMKE = format(-99, width=5)
    )
  DSSAT.soil.series <- soil$components %>%
    dplyr::ungroup() %>%
    dplyr::select(ID_SOIL,
                  SLSOURCE,
                  SLTX,
                  SLDP,
                  SLDESCRIP,
                  SITE,
                  COUNTRY,
                  LAT,
                  LONG,
                  SCSFAMILY,
                  SCOM,
                  SALB,
                  SLU1,
                  SLDR,
                  SLRO,
                  SLNF,
                  SLPF,
                  SMHB,
                  SMPX,
                  SMKE)
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
  soil$horizons %<>%
    dplyr::mutate(SLB = format(SLB, width=5, digits=0),
                  SLMH = format(-99, width=5),
                  SLLL = format(round(SLLL, digits=2), width=5, digits=2, nsmall=2),
                  SDUL = format(round(SDUL, digits=2), width=5, digits=2, nsmall=2),
                  SSAT = format(round(SSAT, digits=2), width=5, digits=2, nsmall=2),
                  SRGF = format(round(SRGF, digits=2), width=5, digits=1, nsmall=1),
                  SSKS = format(round(SSKS, digits=1), width=5, digits=1, nsmall=1),
                  SBDM = format(-99, width=5),
                  SLOC = format(round(SLOC, digits=2), width=5, digits=2, nsmall=2),
                  SLCL = format(round(SLCL, digits=1), width=5, digits=1, nsmall=1),
                  SLSI = format(round(SLSI,digits=1), width=5, digits=1, nsmall=1),
                  SLCF = format(-99, width=5),
                  SLNI = format(-99, width=5),
                  SLHW = format(round(as.numeric(SLHW), digits=1), width=5, digits=1, nsmall=1),
                  SLHB = format(round(as.numeric(SLHB), digits=1), width=5, digits=1, nsmall=1),
                  SCEC = format(round(as.numeric(SCEC), digits=1), width=5, digits=1, nsmall=1),
                  SLPX = format(round(as.numeric(SLPX), digits=1), width=5, digits=1, nsmall=1),
                  SLPT = format(round(as.numeric(SLPT), digits=1), width=5, digits=1, nsmall=1),
                  SLPO = format(-99, width=5),
                  SLCA = format(round(as.numeric(SLCA), digits=1), width=5, digits=1, nsmall=1),
                  SLAL = format(round(as.numeric(SLAL), digits=1), width=5, digits=1, nsmall=1),
                  SLFE = format(round(as.numeric(SLFE), digits=1), width=5, digits=1, nsmall=1),
                  SLMN = format(-99, width=5),
                  SLBS = format(-99, width=5),
                  SLPA = format(-99, width=5),
                  SLPB = format(-99, width=5),
                  SLKE = format(-99, width=5),
                  SLMG = format(-99, width=5),
                  SLNA = format(-99, width=5),
                  SLSU = format(-99, width=5),
                  SLEC = format(round(SLEC, digits=1), width=5, digits=1, nsmall=1))
  
  DSSAT.soil.horizons <- soil$horizons %>%
    dplyr::ungroup() %>%
    dplyr::select(SLB,
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
  
  readr::write_lines(x = c(stringr::str_c("! Component-wise soils data for the study area. ",
                                          nrow(soil$components),
                                          " unique major components."),
                           ""),
                     path = stringr::str_c(output_dir,"/soil.sol"),
                     append = TRUE)
  
  for(i in 1:nrow(soil$components)){
    blank <- ''
    horizons <- DSSAT.soil.horizons[soil$horizons$cokey %in% soil$components[i,"cokey"],]
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
    
    
    readr::write_lines(x = c(header1,
                             header2,
                             data2,
                             header3,
                             data3,
                             header4,
                             data4,
                             blank,
                             blank),
                       path = stringr::str_c(output_dir,"/soil.sol"),
                       append = T)
  }
  
  return(stringr::str_c(output_dir,"/soil.sol"))
}
