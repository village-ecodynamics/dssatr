run_dssat <- function(name, weather.soil.dt, start.day, dssat.dir, out.dir){
  weather <- paste0(name,sprintf("%04d", unique(weather.soil.dt[["CELL"]])))
  if(file.exists(paste0(out.dir,weather,".csv"))) return()
  dir.create(out.dir, recursive=T, showWarnings=F)
  
  cultivars <- c("LWPEUB Werth Pueblo",
                 "LWNORT Werth Northern",
                 "LWSOUT Werth Southern",
                 "LWCORN Werth Cornbelt",
                 "GF0001 Base Garst808-wh403")
  n.cultivars <- length(cultivars)
  
  soils <- weather.soil.dt[["COKEY"]]
  
  treatment.tab <- expand.grid(CULTIVAR=1:n.cultivars,SOIL=sort(as.numeric(as.factor(soils))))
  
  
  ## HEADER ##
  header.lines <- c(paste0("*EXP.DETAILS: ",weather,"MZ RAINFED MAIZE"),
                    "",
                    "*GENERAL",
                    "@PEOPLE",
                    "BOCINSKY,R.K.",
                    "@ADDRESS",
                    "WSU, PULLMAN",
                    "@SITE",
                    paste0(weather),
                    "")
  
  ## TREATMENTS ##
  treatments <- lapply(1:nrow(treatment.tab),function(i){
    real <- i
    while(i>99){
      i <- i-99
    }
    paste(sprintf("%2d", i)," 1 0 0 RAINFED                   ",sprintf("%2d", treatment.tab$CULTIVAR[[real]])," ",sprintf("%2d", treatment.tab$SOIL[[real]]),"  0  1  1  0  0  0  0  0  0  0  1",sep='')
  })
  treatment.lines <- c("*TREATMENTS                        -------------FACTOR LEVELS------------",
                       "@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM",
                       treatments,
                       "")
  
  ## CULTIVARS ##
  cultivar.lines <- unlist(lapply(1:length(cultivars),function(i){
    paste0(sprintf("%2d", i)," MZ ",cultivars[[i]])
  }))
  cultivar.lines <- c("*CULTIVARS",
                      "@C CR INGENO CNAME",
                      cultivar.lines,
                      "")
  
  ## FIELDS ##
  fields <- unlist(lapply(1:length(soils),function(i){
    paste(sprintf("%2d", i)," ",soils[[i]]," ",weather,"   -99     0 DR000     0     0 00000  -99  ",sprintf("%4d", -99),"  ",sprintf("%010d", soils[[i]])," RAINFED",sep='')
  }))
  field.lines <- c("*FIELDS",
                   "@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME",
                   fields,
                   "")
  
  ## INITIAL CONDITIONS ##
  initial.cond.lines <- c("*INITIAL CONDITIONS",
                          "@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME",
                          paste(" 1    MZ ",start.day,"   100     0     1     1   -99  1000    .8     0   100    15    -99",sep=''),
                          "@C  ICBL  SH2O  SNH4  SNO3",
                          " 1     5    .3   -99   -99",
                          " 1    15    .3   -99   -99",
                          " 1    30    .3   -99   -99",
                          " 1    60    .3   -99   -99",
                          " 1    90    .3   -99   -99",
                          " 1   120    .3   -99   -99",
                          "")
  
  ## PLANTING DETAILS ##
  planting.details.lines <- c("*PLANTING DETAILS",
                              "@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME",
                              paste(" 1 ",81136,"   -99   0.5   0.5     S     H   100   -99    15   -99   -99   -99     4     0                        -99",sep=''),
                              "")
  
  ## SIMULATION CONTROLS ##
  simulation.control.lines <- c("*SIMULATION CONTROLS",
                                "@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL",
                                paste0( " 1 GE          ",sprintf("%5d", 33),"     1     S ",start.day,"  0001 ",weather,"                  MZCER"),
                                "@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2",
                                " 1 OP              Y     N     N     N     N     N     N     N     M",
                                "@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL",
                                " 1 ME              M     M     E     R     S     R     R     1     G     S     2",
                                "@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS",
                                " 1 MA              R     N     N     N     M",
                                "@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT",
                                " 1 OU              N     N     Y     1     N     N     N     N     N     N     N     N     N",
                                "")
  
  automatic.management.lines <- c("@  AUTOMATIC MANAGEMENT",
                                  "@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN",
                                  " 1 PL          81136 81167     0   100    30    40    10",
                                  "@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF",
                                  " 1 IR             30    50   100 GS000 IR001    10     1",
                                  "@N RESIDUES    RIPCN RTIME RIDEP",
                                  " 1 RE            100     1    20",
                                  "@N HARVEST     HFRST HLAST HPCNP HPCNR",
                                  " 1 HA              0 82135   100     0")
  
  dir.create(dssat.dir, recursive=T, showWarnings = F)
  
  fileConn<-file(paste0(dssat.dir,"/",weather,".MZX"), raw=T)
  writeLines(as.character(c(header.lines,treatment.lines,cultivar.lines,field.lines,initial.cond.lines,planting.details.lines,simulation.control.lines,automatic.management.lines)), fileConn)
  close(fileConn)
  
  currentwd <- getwd()
  setwd(paste0(dssat.dir,"/"))
  system(paste0("./dscsm046.exe A ",weather,".MZX"), ignore.stdout=T, ignore.stderr=T)
  
  
  
  out <- read.fwf(file="./Summary.OUT", skip=4, widths=c(64, 9, 9, 11, 8, 57, 5), header=F, as.is=T)
  out <- out[,c(2,3,5,7)]
  names(out) <- c("SOIL","CELL","YEAR","YIELD")
  out$CULTIVAR <- rep(treatment.tab$CULTIVAR,each=33)
  out$CULTIVAR <- cultivars[out$CULTIVAR]
  
  out$CELL <- gsub(name,"",out$CELL)
  out$YEAR <- gsub("001$","",out$YEAR)
  out$YIELD[out$YIELD == "-99"] <- 0
  
  file.remove(paste0(dssat.dir,"/",weather,".MZX"))
  
  setwd(currentwd)
  
  write.csv(out, paste0(out.dir,weather,".csv"), row.names=F)  
}
