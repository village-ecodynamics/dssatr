#' Run a batch spatial dssat analysis from a 
#' cultigen, weather, and soil object.
#'
#' @param name 
#' @param cultivars 
#' @param weather 
#' @param soil 
#' @param earliest_planting_doy 
#' @param latest_planting_doy 
#' @param output_dir 
#'
#' @return A list containing field, cultivar, and yield information.
#' @export
dssat_run_batch <- function(name,
                      cultivars = dssat_read_cultigen("~/DSSAT46/Genotype/MZCER046.CUL") %>%
                        dplyr::filter(`@VAR#` %in% c("AC0001",
                                                     "GF0001",
                                                     "GF0101",
                                                     "GF0201",
                                                     "GF0301",
                                                     "GF0401",
                                                     "GF0501")),
                      weather,
                      soil,
                      earliest_planting_doy = 136,
                      latest_planting_doy = 167,
                      output_dir = tempfile()){
  
  # Transform weather to WGS84
  weather %<>% sf::st_transform(crs = 4326)
  
  # Write the soil file to the output directory (soil.sol)
  soil %>%
    dssat_write_soil(output_dir = output_dir)
  
  # Write the weather files to the output directory (*.WTH files)
  weather %>%
    dssat_write_weather(output_dir = output_dir)
  
  # Get the years of the simpulation from the weather file
  years <- weather$weather[[1]]$date %>%
    lubridate::year() %>%
    unique() %>%
    stringr::str_trunc(2,"left",ellipsis = "")
  
  # Get a geometric representation of soil/weather combinations
  suppressWarnings({
    weather_soil <- soil %$%
      mapunits %>%
      sf::st_intersection(x = weather %>%
                            dplyr::select(-weather),
                          y = .) %>%
      dplyr::left_join(soil %$%
                         components %>%
                         dplyr::select(mukey, ID_SOIL, `Component percent`),
                       by = "mukey") %>%
      dplyr::select(-muname) %>%
      dplyr::distinct() %>%
      tibble::as_tibble() %>%
      sf::st_as_sf() %>%
      dplyr::mutate(tile = stringr::str_pad(tile, width = 8, pad = "0")) %>%
      dplyr::arrange(tile,
                     mukey,
                     -`Component percent`,
                     ID_SOIL) #%>%
      #na.omit()
  })
  
  cultivars <- cultivars %>%
    dplyr::mutate(cultivar = 1:n()) 
  
  fields <- weather_soil %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::select(tile, mukey, ID_SOIL) %>%
    dplyr::distinct() %>%
    dplyr::mutate(field = 1:n()) %>%
    dplyr::select(field, tile, mukey, ID_SOIL)
  
  # There can be a maximum of 99 cultivars and/or 99 fields
  # in a DSSAT run due to file format limitations. Therefore,
  # we must split up the analysis.
  cultivars %<>% 
    split(rep(1:ceiling(nrow(.)/99),each = 99)[1:nrow(.)]) %>%
    purrr::map(function(x){
      x %>%
        dplyr::mutate(id = 1:nrow(.))
    })
  
  fields %<>% 
    split(rep(1:ceiling(nrow(.)/99),each = 99)[1:nrow(.)]) %>%
    purrr::map(function(x){
      x %>%
        dplyr::mutate(id = 1:nrow(.))
    })
  
  summary_out <- lapply(cultivars, function(cult){
    lapply(fields, function(fiel){
      
      treatments <- expand.grid(
        cultivar = cult$id,
        field = fiel$id
      ) %>%
        dplyr::mutate(treatment = 1:n()) %>%
        tibble::as_tibble()
      
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
      treatment.lines <- purrr::map(1:nrow(treatments),function(i){
        real <- i
        while(i>99){
          i <- i-99
        }
        paste0(sprintf("%2d", i),
               " 1 0 0 ",
               "RAINFED                   ",
               sprintf("%2d", treatments$cultivar[[real]])," ",
               sprintf("%2d", treatments$field[[real]])," ",
               " 0  1  1  0  0  0  0  0  0  0  1")
      }) %>%
        unlist()
      treatment.lines <- c("*TREATMENTS                        -------------FACTOR LEVELS------------",
                           "@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM",
                           treatment.lines,
                           "")
      
      ## CULTIVARS ##
      cultivar.lines <- 1:nrow(cult) %>%
        purrr::map(function(i){
          real <- i
          while(i>99){
            i <- i-99
          }
          stringr::str_c(sprintf("%2d", i),
                         " MZ ",
                         cult[real,]$`@VAR#`, " ",
                         cult[real,]$`VRNAME..........` %>%
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
      field.lines <- 1:nrow(fiel) %>%
        purrr::map(function(i){
          real <- i
          while(i>99){
            i <- i-99
          }
          stringr::str_c(sprintf("%2d", i)," ",
                         fiel$ID_SOIL[[real]]," ",
                         fiel$tile[[real]]," ",
                         "  -99     0 DR000     0     0 00000  -99  ",
                         sprintf("%4d", -99),"  ",
                         sprintf("%010s", fiel$ID_SOIL[[real]])," RAINFED")
        }) %>%
        unlist()
      
      
      field.lines <- c("*FIELDS",
                       "@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME",
                       field.lines,
                       "")
      
      ## INITIAL CONDITIONS ##
      initial.cond.lines <- c("*INITIAL CONDITIONS",
                              "@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME",
                              stringr::str_c(" 1    MZ ",years[[1]], "001","   100     0     1     1   -99  1000    .8     0   100    15    -99"),
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
                                  stringr::str_c(" 1 ",years[[1]], earliest_planting_doy,"   -99   0.5   0.5     S     H   100   -99    15   -99   -99   -99     4     0                        -99"),
                                  "")
      
      ## SIMULATION CONTROLS ##
      simulation.control.lines <- c("*SIMULATION CONTROLS",
                                    "@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL",
                                    stringr::str_c( " 1 GE          ",sprintf("%5d", length(years)),"     1     S ",years[[1]], "001","  0001 ","test" %>% stringr::str_pad(24, side = "right")," "," MZCER"),
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
                                      stringr::str_c(" 1 PL          ", years[[1]], earliest_planting_doy, " ", years[[1]], latest_planting_doy,"     0   100    30    40    10"),
                                      "@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF",
                                      " 1 IR             30    50   100 GS000 IR001    10     1",
                                      "@N RESIDUES    RIPCN RTIME RIDEP",
                                      " 1 RE            100     1    20",
                                      "@N HARVEST     HFRST HLAST HPCNP HPCNR",
                                      stringr::str_c(" 1 HA              0 ",years[[2]], earliest_planting_doy-1,"   100     0"))
      
      dir.create(output_dir, recursive=T, showWarnings = F)
      
      fileConn<-file(paste0(output_dir,"/","test0000.MZX"), raw=T)
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
      
      currentwd <- getwd()
      setwd(paste0(output_dir,"/"))
      system2(command = "/users/bocinsky/DSSAT47/dscsm047",
              args = c("A",
                       "test0000.MZX"),
              stdout = FALSE,
              stderr = FALSE,
              wait = TRUE
      )
      setwd(currentwd)

      summary_out <- dssat_read_summary_out(paste0(output_dir,"/Summary.OUT")) %>%
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
                                     yield)) %>%
        dplyr::select(-treatment) %>%
        dplyr::bind_cols(treatments %>%
                           dplyr::slice(rep(1:n(), each=length(years)))) %>%
        dplyr::select(field, cultivar, year, yield, -treatment) %>%
        dplyr::left_join(fiel %>%
                           dplyr::select(field,
                                  id) %>%
                           dplyr::rename(field_id = field,
                             field = id),
                           by = c("field")) %>%
        dplyr::left_join(cult %>%
                           dplyr::select(cultivar,
                                         id) %>%
                           dplyr::rename(cultivar_id = cultivar,
                                         cultivar = id),
                         by = c("cultivar")) %>%
        dplyr::select(cultivar_id,
                      field_id,
                      year,
                      yield) %>%
        dplyr::rename(field = field_id,
                      cultivar = cultivar_id)
    }) %>%
      dplyr::bind_rows()
  }) %>%
    dplyr::bind_rows()
  

  out <- list(fields = weather_soil %>% 
         dplyr::left_join(fields %>% 
                            dplyr::bind_rows() %>%
                            dplyr::select(-id),
                          by = c("tile",
                                 "mukey",
                                 "ID_SOIL")) %>%
         dplyr::select(field,
                       dplyr::everything()),
       cultivars = cultivars %>%
         dplyr::bind_rows() %>%
         dplyr::select(-id)%>%
         dplyr::select(cultivar,
                       dplyr::everything()),
       yields = summary_out)
  
  class(out) <- c(class(out), "yields")
  
  return(out)
}
