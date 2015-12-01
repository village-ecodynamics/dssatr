prepDSSATParameters <- function(location.id, local.soils, soil.depths, out.dir, recon.years){
  start.year <- sprintf("%04d", recon.years[1])
  
  ## HEADER ##
  header.lines <- c("*EXP.DETAILS: CCAC001MZ RAINFED MAIZE",
                    "",
                    "*GENERAL",
                    "@PEOPLE",
                    "BOCINSKY,R.K.",
                    "@ADDRESS",
                    "WSU, PULLMAN",
                    "@SITE",
                    "Mesa Verde National Park, Colorado",
                    "")
  
  ## TREATMENTS ##
  treatments <- unlist(lapply(1:length(local.soils),function(x){paste(sprintf("%2d", x)," 1 0 0 RAINFED                    1 ",sprintf("%2d", x),"  0  1  1  0  0  0  0  0  0  1  1",sep='')}))
  treatment.lines <- c("*TREATMENTS                        -------------FACTOR LEVELS------------",
                       "@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM",
                       treatments,
                       "")
  
  ## CULTIVARS ##
  cultivar.lines <- c("*CULTIVARS",
                      "@C CR INGENO CNAME",
                      " 1 MZ AC0001 TOHONO O'odham",
                      "")
  
  ## FIELDS ##
  fields <- unlist(lapply(1:length(local.soils),function(x){paste(sprintf("%2d", x)," ",paste("CEL",formatC(location.id, width = 5, format = "d", flag = "0"), sep='')," ",paste("CEL",formatC(location.id, width = 5, format = "d", flag = "0"), sep=''),"   -99     0 DR000     0     0 00000 -99   ",sprintf("%4d", soil.depths[x]),"  ",sprintf("%010d", local.soils[x])," RAINFED",sep='')}))
  field.lines <- c("*FIELDS",
                   "@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME",
                   fields,
                   "")
  
  ## INITIAL CONDITIONS ##
  initial.cond.lines <- c("*INITIAL CONDITIONS",
                          "@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME",
                          paste(" 1    MZ ",substr(start.year,3,4),"142   100     0     1     1   -99  1000    .8     0   100    15 -99",sep=''),
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
                              paste(" 1 ",substr(start.year,3,4),"142   -99   0.5   0.5     S     H   100   -99    15   -99   -99   -99     4     0                        RAINFED",sep=''),
                              "")
  
  ## HARVEST DETAILS ##
  harvest.details.lines <- c("*HARVEST DETAILS",
                             "@H HDATE  HSTG  HCOM HSIZE   HPC  HBPC HNAME",
                             paste(" 1 ",substr(start.year,3,4),"270 GS006     H     A   100     0 MAIZE",sep=''),
                             "")
  
  ## SIMULATION CONTROLS ##
  simulation.control.lines <- c("*SIMULATION CONTROLS",
                                "@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL",
                                paste(" 1 GE          ",sprintf("%5d", length(recon.years)),"     1     S ",substr(start.year,3,4),"142  0001 RAINFED",sep=''),
                                "@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2",
                                " 1 OP              Y     N     N     N     N     N     N     N     D",
                                "@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL",
                                " 1 ME              M     M     E     P     R     C     D     1     G     S     2",
                                "@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS",
                                " 1 MA              R     N     N     N     R",
                                "@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT",
                                " 1 OU              N     N     Y     1     N     N     N     N     N     N     0     N     N")
  
  fileConn<-file(paste(out.dir,paste("CEL",formatC(location.id, width = 5, format = "d", flag = "0"), sep=''),".MZX",sep=''))
  writeLines(c(header.lines,treatment.lines,cultivar.lines,field.lines,initial.cond.lines,planting.details.lines,harvest.details.lines,simulation.control.lines), fileConn)
  close(fileConn)
  
}
