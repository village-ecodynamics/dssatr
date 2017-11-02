# dssatr tester
library(dssatr)
library(magrittr)
library(units)
library(ggplot2)

# Set a directory for testing
testDir <- "./dssatr test"

dir.create(testDir,
           showWarnings = F,
           recursive = T)

# Extract data for the Village Ecodynamics Project "VEPIIN" study area:
# http://village.anth.wsu.edu
# data(mvnp)

output.dir <- stringr::str_c(testDir,"/OUTPUT/dssat_run") %T>%
  dir.create(showWarnings = F,
             recursive = T)
raw.dir <- stringr::str_c(output.dir,"/DATA/RAW") %T>%
  dir.create(showWarnings = F,
             recursive = T)
extraction.dir <- stringr::str_c(output.dir,"/DATA/EXTRACTIONS") %T>%
  dir.create(showWarnings = F,
             recursive = T)

# # get point above Paul's Old Garden on the Crow Canyon campus
# aoi <- sf::st_point(c(-108.618642,37.355880)) %>%
#   sf::st_as_text() %>%
#   sf::st_as_sfc(4326)
# 
# aoi <- sf::st_multipoint(
#   matrix(c(-109.25,37.95556,-109.25,37.95555,-109.25,38.05,-109.25,38.15),
#          nrow = 4,
#          byrow = TRUE)
# ) %>%
#   sf::st_as_text() %>%
#   sf::st_as_sfc(4326)

# aoi <- dssat_ccac

aoi <- dssat_mvnp

# aoi <- dssat_hopi

# wkt_geom <- raster::extent(-110,-107,36,39) %>% 
#   FedData::polygon_from_extent("+proj=longlat +datum=WGS84") %>%
#   sf::st_as_sf() %$%
#   geometry %>%
#   sf::st_as_text()

ssurgo.out <- aoi %>%
  dssatr:::dssat_get_ssurgo()

ssurgo.out$mapunits %<>%
  sf::st_make_valid()

daymet.out <- aoi %>%
  dssatr:::dssat_get_daymet(years = 2010:2012)

weather <- daymet.out
soil <- ssurgo.out
cultivars <- dssatr:::dssat_read_cultigen("~/DSSAT46/Genotype/MZCER046.CUL") %>%
  dplyr::filter(`@VAR#` == "AC0001")
earliest_planting_doy = 136
latest_planting_doy = 167
output_dir <- "./dssatr test/OUTPUT/dssat_run/"
name <- "test"

system.time({
  out <- dssat_run_batch(name = "test",
                         weather = daymet.out,
                         soil = ssurgo.out,
                         cultivars = dssatr:::dssat_read_cultigen("~/DSSAT46/Genotype/MZCER046.CUL") %>%
                           dplyr::filter(`@VAR#` == "AC0001"),
                         output_dir = "./dssatr test/OUTPUT/dssat_run/")
  
})


test <- out$yields %>%
  # dplyr::group_by(field, cultivar) %>%
  # dplyr::summarise(yield = mean(yield)) %>%
  dplyr::left_join(out$fields) %>%
  dplyr::select(year:`Component percent`,
                -ID_SOIL) %>%

  dplyr::group_by(tile,mukey,year) %>%
  dplyr::arrange(year,tile,mukey) %>%
  dplyr::mutate(yield = ifelse(is.na(yield),0,yield)) %>%
  dplyr::summarise(yield = weighted.mean(x = yield,
                                         w = `Component percent`,
                                         na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(tile, mukey) %>%
  dplyr::summarise(yield = max(yield)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(tile,mukey) %>%
  dplyr::left_join(out$fields %>%
                     dplyr::select(tile, mukey) %>% 
                     dplyr::distinct()) %>% 
  sf::st_sf()
  
test %>%
  # dplyr::filter(year == 1985) %>%
  dplyr::select(yield) %>%
  ggplot() +
  geom_sf(aes(fill = yield),
          colour = NA) +
  scale_fill_distiller("Yield (kg/ha)",
                       palette = "RdYlGn",
                       direction = 1) + # fill with brewer colors 
  theme(line = element_blank(),                          # remove axis lines .. 
        axis.text=element_blank(),                       # .. tickmarks..
        axis.title=element_blank(),                      # .. axis labels..
        panel.background = element_blank())












plot.ssurgo.components <- function(x, .vars, .funs){
  x$components %>%
    dplyr::select(mukey, `Component percent`, !!!.vars) %>%
    dplyr::group_by(mukey) %>%
    dplyr::summarise_at(.vars = .vars,
                        .funs = .funs) %>%
    dplyr::right_join(ssurgo.out$mapunits) %>%
    sf::st_sf() %>%
    dplyr::select(!!! .vars) %>%
    dplyr::mutate_at(.vars = .vars,
                     .funs = function(y){ifelse(is.infinite(y),0,y)}) %>%
    plot()
}
  
ssurgo.out %>%
  plot.ssurgo.components(.vars = dplyr::vars(SALB, SLRO),
                         .funs = funs(weighted.mean(.,
                                                    w = `Component percent`,
                                                    na.rm = TRUE)))

ssurgo.out %>%
  plot.ssurgo.components(.vars = dplyr::vars(SALB, `Component percent`),
                         .funs = funs(max(., na.rm = TRUE)))
