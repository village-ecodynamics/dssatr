dssat_run_batch <- function(name,
                      cultivars = dssat_read_cultigen("~/DSSAT46/Genotype/MZCER046.CUL"),
                      weather,
                      soil,
                      start_day = "81001",
                      dssat_dir = "./dssatr test/OUTPUT/dssat_run/"){
  
  # Get a geometric representation of soil/weather combinations
  suppressWarnings({
    weather_soil <- soil %$%
      mapunits %>%
      sf::st_intersection(aoi) %>%
      sf::st_intersection(x = weather %>%
                            dplyr::select(-weather),
                          y = .) %>%
      dplyr::left_join(soil %$%
                         components %>%
                         dplyr::select(mukey, ID_SOIL),
                       by = "mukey") %>%
      dplyr::select(-mukey, -muname) %>%
      dplyr::distinct() %>%
      tibble::as_tibble() %>%
      sf::st_as_sf() %>%
      dplyr::mutate(tile = stringr::str_pad(tile, width = 8, pad = "0")) %>%
      dplyr::arrange(tile, ID_SOIL)
  })
  
  cultivars <- dssatr:::dssat_read_cultigen("~/DSSAT46/Genotype/MZCER046.CUL") %>%
    dplyr::slice(1:2) %>%
    dplyr::mutate(cultivar = 1:n())
  
  fields <- weather_soil %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::select(tile, ID_SOIL) %>%
    dplyr::distinct() %>%
    dplyr::mutate(field = 1:n()) %>%
    dplyr::select(field, tile, ID_SOIL)
  
  treatment.tab <- expand.grid(
    cultivar = cultivars$cultivar,
    field = fields$field
  ) %>%
    dplyr::mutate(treatment = 1:n())
  
  ## HEADER ##
  header.lines <- c(paste0("*EXP.DETAILS: ","MZ RAINFED MAIZE"),
                    "",
                    "*GENERAL",
                    "@PEOPLE",
                    "BOCINSKY,R.K.",
                    "@ADDRESS",
                    "WSU, PULLMAN",
                    "@SITE",
                    "")
  
  ## TREATMENTS ##
  treatments <- purrr::map(1:nrow(treatment.tab),function(i){
    real <- i
    while(i>99){
      i <- i-99
    }
    paste0(sprintf("%2d", i),
           " 1 0 0 ",
           "RAINFED                   ",
           sprintf("%2d", treatment.tab$cultivar[[real]])," ",
           sprintf("%2d", treatment.tab$field[[real]])," ",
           " 0  1  1  0  0  0  0  0  0  0  1")
  })
  treatment.lines <- c("*TREATMENTS                        -------------FACTOR LEVELS------------",
                       "@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM",
                       treatments,
                       "")
  
  ## CULTIVARS ##
  cultivar.lines <- 1:nrow(cultivars) %>%
    purrr::map(function(i){
      stringr::str_c(sprintf("%2d", cultivars$cultivar[[i]]),
                     " MZ ",
                     cultivars[i,]$`@VAR#`, " ",
                     cultivars[i,]$`VRNAME..........` %>%
                       stringr::str_pad(width = 13,
                                        side = "left")
      )
    }) %>%
    unlist()
  
  cultivar.lines <- c("*CULTIVARS",
                      "@C CR INGENO CNAME",
                      cultivar.lines,
                      "")
  
  ## FIELDS ##
  fields <- 1:nrow(fields) %>%
    purrr::map(function(i){
      stringr::str_c(sprintf("%2d", fields$field[[i]])," ",
                     fields$ID_SOIL[[i]]," ",
                     fields$tile[[i]]," ",
                     "  -99     0 DR000     0     0 00000  -99  ",
                     sprintf("%4d", -99),"  ",
                     sprintf("%010s", fields$ID_SOIL[[i]])," RAINFED")
    }) %>%
    unlist()
  
  
  field.lines <- c("*FIELDS",
                   "@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME",
                   fields,
                   "")
  
  ## INITIAL CONDITIONS ##
  initial.cond.lines <- c("*INITIAL CONDITIONS",
                          "@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME",
                          paste(" 1    MZ ",start_day,"   100     0     1     1   -99  1000    .8     0   100    15    -99",sep=''),
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
                              paste(" 1 ","81136","   -99   0.5   0.5     S     H   100   -99    15   -99   -99   -99     4     0                        -99",sep=''),
                              "")
  
  ## SIMULATION CONTROLS ##
  simulation.control.lines <- c("*SIMULATION CONTROLS",
                                "@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL",
                                paste0( " 1 GE          ",sprintf("%5d", 33),"     1     S ",start_day,"  0001 ","test" %>% stringr::str_pad(24, side = "right")," "," MZCER"),
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
  
  dir.create(dssat_dir, recursive=T, showWarnings = F)
  
  fileConn<-file(paste0(dssat_dir,"/","test0000.MZX"), raw=T)
  writeLines(as.character(c(header.lines,
                            treatment.lines,
                            cultivar.lines,
                            field.lines,
                            initial.cond.lines,
                            planting.details.lines,
                            simulation.control.lines,
                            automatic.management.lines)), 
             fileConn)
  close(fileConn)
  
  system2(command = "cp",
          args = c("-r",
                   "~/DSSAT46/BASE/*",
                   shQuote(dssat_dir)
          )
  )
  
  soil %>%
    dssatr:::dssat_write_soil(output.dir = dssat_dir)
  
  weather %>%
    dssatr:::dssat_write_weather(output.dir = paste0(dssat_dir, "/WEATHER"))
  
  currentwd <- getwd()
  setwd(paste0(dssat_dir,"/"))
  system2(command = "./dscsm046.exe",
          args = c("A",
                   "test0000.MZX"),
          stdout = FALSE,
          stderr = FALSE,
          wait = TRUE
  )
  setwd(currentwd)
  
  summary_out <- dssatr:::dssat_read_summary_out(paste0(dssat_dir,"/Summary.OUT")) %>%
    dplyr::select(TRNO,
                  SDAT,
                  HWAM) %>%
    dplyr::mutate(SDAT = SDAT %>%
                    lubridate::year()) %>%
    dplyr::rename(treatment = TRNO,
                  year = SDAT,
                  yield = HWAM) %>%
    dplyr::mutate(yield = ifelse(is.na(yield),
                                 0,
                                 yield))
  
  return(summary_out)
}
