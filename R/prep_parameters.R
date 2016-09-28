prep_dssat_parameters <- function(weather, soil, start.day, end.day, out.dir){
  ## HEADER ##
  header.lines <- c(paste0("*EXP.DETAILS: ",weather,"_",soil," RAINFED MAIZE"),
                    "",
                    "*GENERAL",
                    "@PEOPLE",
                    "BOCINSKY,R.K.",
                    "@ADDRESS",
                    "WSU, PULLMAN",
                    "@SITE",
                    paste0(weather,"_",soil),
                    "")
  
  ## TREATMENTS ##
  treatments <- unlist(lapply(1:length(soil),function(x){paste(sprintf("%2d", x)," 1 0 0 RAINFED                    1 ",sprintf("%2d", x),"  0  1  1  0  0  0  0  0  0  0  1",sep='')}))
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
  fields <- unlist(lapply(1:length(soil),function(x){paste(sprintf("%2d", x)," ",soil[[x]]," ",weather[[x]],"   -99     0 DR000     0     0 00000 -99   ",sprintf("%4d", -99),"  ",sprintf("%010d", soil[[x]])," RAINFED",sep='')}))
  field.lines <- c("*FIELDS",
                   "@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME",
                   fields,
                   "")
  
  ## INITIAL CONDITIONS ##
  initial.cond.lines <- c("*INITIAL CONDITIONS",
                          "@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME",
                          paste(" 1    MZ ",start.day,"   100     0     1     1   -99  1000    .8     0   100    15 -99",sep=''),
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
                                paste0(" 1 GE          ",sprintf("%5d", 33),"     1     S ",start.day,"  0001 ",weather,"_",soil,"         MZCER"),
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
  
  dir.create(out.dir, recursive=T, showWarnings = F)
  
  fileConn<-file(paste(out.dir,weather,".MZX",sep=''))
  writeLines(c(header.lines,treatment.lines,cultivar.lines,field.lines,initial.cond.lines,planting.details.lines,simulation.control.lines,automatic.management.lines), fileConn)
  close(fileConn)
  
}
