dssat_get_ssurgo <- function(x){
  
  x %<>% 
    sf::st_transform(4326) %>%
    sf::st_geometry() %>%
    sf::st_sf()
  
  wkt_geom <- x %>%
    sf::st_union() %>%
    sf::st_as_text()
  
  geoms_char <- x %>%
    sf::st_geometry_type() %>%
    as.character()
  
  geoms_char <- c(geoms_char,
                  geoms_char %>%
                    gsub(pattern = "MULTI",
                         replacement = "",
                         x = .),
                  stringr::str_c("MULTI",geoms_char)
  ) %>%
    unique()
  
  
  mapunit <- wkt_geom %>%
    stringr::str_c(
      "select G.MupolygonWktWgs84 as geom, mapunit.mukey, muname
          FROM mapunit
          CROSS APPLY SDA_Get_MupolygonWktWgs84_from_Mukey(mapunit.mukey) as G
          WHERE mukey IN (
          SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('",.,"')
          )"
    ) %>%
    dssat_sda_query() %>%
    dplyr::mutate(mukey = as.character(mukey),
                  geom = sf::st_as_sfc(geom, 
                                       crs = 4326)) %>%
    tibble::as_tibble() %>%
    sf::st_as_sf()
  
  suppressMessages({
    mapunit %<>%
      dplyr::filter(
        sf::st_intersects(
          mapunit,
          x,
          sparse = F) %>%
          apply(1, any)
      ) %>%
      dplyr::group_by(mukey, muname)  %>%
      dplyr::summarise(geom = sf::st_combine(geom)) %>%
      dplyr::ungroup() %>%
      sf::st_as_sf() %>%
      sf::st_cast()
  })
  
  component <- stringr::str_c("SELECT mukey, cokey, compname, albedodry_r, comppct_r, taxclname, taxpartsize, drainagecl, runoff
                     FROM component
                 WHERE mukey IN (",
                              stringr::str_c(mapunit$mukey %>% unique(),
                                             collapse = ","),
                              ")") %>%
    dssat_sda_query() %>%
    dplyr::filter(!is.na(albedodry_r)) %>%
    dplyr::arrange(cokey) %>%
    dplyr::mutate(mukey = as.character(mukey),
                  cokey = as.character(cokey),
                  `Component percent` = comppct_r,
                  ID_SOIL = cokey,
                  SLSOURCE = compname,
                  SLTX = NA,
                  # SLDP = max_depth_vect(ID_SOIL),
                  SLDESCRIP = taxclname,
                  SITE = mukey,
                  COUNTRY = "USA",
                  LAT = "",
                  LONG = "",
                  SCSFAMILY = taxpartsize,
                  SCOM = NA,
                  SALB = albedodry_r,
                  SLU1 = NA,
                  
                  # Drainage class after DSSAT 4.5 manual Vol. 2, Section 1.4.3.2, Table 2
                  SLDR = (dssat_drainage_classes$SLDR)[match(drainagecl,dssat_drainage_classes$`Drainage Class`)],
                  
                  # Runoff potential after DSSAT 4.5 manual Vol. 2, Section 1.4.3.2, Table 2
                  runoff = ifelse(is.na(runoff), "", runoff),
                  SLRO = (dssat_runoff_potential$SLRO)[match(runoff,dssat_runoff_potential$`Runoff Potential`)],

                  
                  SLNF = 1,
                  SLPF = 1,
                  SMHB = NA,
                  SMPX = NA,
                  SMKE = NA) %>%
    dplyr::select(mukey, cokey, `Component percent`, ID_SOIL:SMKE)
  
  ## HORIZON LEVEL DATA ##
  horizon <- stringr::str_c("SELECT *
                                  FROM chorizon
                                  WHERE cokey IN (", stringr::str_c(component$cokey, collapse = ","),")") %>%
    dssat_sda_query() %>%
    dplyr::arrange(cokey,
                   hzdepb_r) %>%
    dplyr::filter(!is.na(claytotal_r)) %>%
    dplyr::mutate(silttotal_r = 100 - (claytotal_r + sandtotal_r),
                  silttotal_r = ifelse(is.na(silttotal_r),
                                       (100 - claytotal_r) / 2,
                                       silttotal_r),
                  sandtotal_r = ifelse(is.na(sandtotal_r),
                                       (100 - claytotal_r) / 2,
                                       sandtotal_r))
  
  
  # Bind with texture data from USDA
  horizon %<>% dplyr::bind_cols(
    tibble::tibble(TEXTURE = soiltexture::TT.points.in.classes(tri.data = horizon %>%
                                                                 dplyr::select(claytotal_r,sandtotal_r,silttotal_r) %>% 
                                                                 dplyr::rename(CLAY = claytotal_r,
                                                                               SAND = sandtotal_r,
                                                                               SILT = silttotal_r) %>%
                                                                 as.data.frame(),
                                                               class.sys = "USDA.TT",
                                                               PiC.type="t"))
  )
  
  horizon %<>%
    dplyr::mutate( cokey = as.character(cokey),
      SLB = hzdepb_r,
                   SLMH = NA,
                   
                   SLLL = wfifteenbar_r/100,
                   SLLL = ifelse(is.na(SLLL), dssat_soil_hydrology$LL[match(TEXTURE,as.character(dssat_soil_hydrology$`SOIL CLASS`))], SLLL),
                   
                   SDUL = wthirdbar_r/100,
                   SDUL = ifelse(is.na(SDUL), dssat_soil_hydrology$DUL[match(TEXTURE,as.character(dssat_soil_hydrology$`SOIL CLASS`))], SDUL),
                   
                   SSAT = wsatiated_r/100,
                   SSAT = ifelse(is.na(SSAT), dssat_soil_hydrology$SAT[match(TEXTURE,as.character(dssat_soil_hydrology$`SOIL CLASS`))], SSAT),
                   SSAT = max(SSAT,SDUL),
                   
                   SRGF = exp(-0.02 * ((hzdepb_r + hzdept_r) / 2)),
                   SRGF = ifelse(hzdepb_r<=15, 1, SRGF),
                   
                   SSKS = ksat_r * (0.36),
                   SBDM = NA,
                   SLOC = om_r,
                   SLCL = claytotal_r,
                   SLSI = silttotal_r,
                   SLCF = NA,
                   SLNI = NA,
                   SLHW = ph1to1h2o_r,
                   SLHB = ph01mcacl2_r,
                   SCEC = cec7_r,
                   SLPX = pbray1_r,
                   SLPT = ptotal_r,
                   SLPO = NA,
                   SLCA = caco3_r/1000,
                   SLAL = extral_r,
                   SLFE = freeiron_r,
                   SLMN = NA,
                   SLBS = NA,
                   SLPA = NA,
                   SLPB = NA,
                   SLKE = NA,
                   SLMG = NA,
                   SLNA = NA,
                   SLSU = NA,
                   SLEC = ec_r) %>%
    dplyr::select(cokey,
                  SLB:SLEC)
  
  
  # Functions to get the maximum depth of a layer
  max_depth <- function(component_key){
    horizon %>%
      dplyr::filter(cokey == component_key) %>%
      dplyr::summarise(max(SLB)) %$%
      `max(SLB)`
  }
  
  max_depth_vect <- function(component_key_vect){
    component_key_vect %>%
      sapply(max_depth)
  }
  
  component %<>%
    dplyr::mutate(SLDP = max_depth_vect(ID_SOIL))
  
  mapunit %<>%
    sf::st_intersection(x)
  
  out <- list(mapunits = mapunit,
              components = component,
              horizons = horizon)
  
  class(out) <- c(class(out), "soil")
  
  return(out)
}
