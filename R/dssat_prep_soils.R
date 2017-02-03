dssat_prep_soils <- function(ssurgo){
  # Load DSSAT data
  data("dssat_soil_hydrology")
  data("dssat_drainage_classes")
  data("dssat_runoff_potential")
  
  # DSSAT requires soils data at the mapunit, component, and horizon levels.
  # This function takes SSURGO data as output from the FedData package, and returns
  # an object just containing the data needed by DSSAT.
  
  ## HORIZON LEVEL DATA ##
  NRCS.horizon <- ssurgo$tabular$chorizon %>%
    tibble::as_tibble() %>%
    dplyr::arrange(cokey,hzdepb.r) %>%
    dplyr::filter(!is.na(claytotal.r)) %>%
    dplyr::mutate(silttotal.r = 100-(claytotal.r + sandtotal.r),
                  silttotal.r = ifelse(is.na(silttotal.r),(100-claytotal.r)/2,silttotal.r),
                  sandtotal.r = ifelse(is.na(sandtotal.r),(100-claytotal.r)/2,sandtotal.r))
  
  # Bind with texture data from USDA
  NRCS.horizon %<>% bind_cols(
    tibble::tibble(TEXTURE = soiltexture::TT.points.in.classes(tri.data = NRCS.horizon %>%
                                                                 dplyr::select(claytotal.r,sandtotal.r,silttotal.r) %>% 
                                                                 dplyr::rename(CLAY = claytotal.r,
                                                                               SAND = sandtotal.r,
                                                                               SILT = silttotal.r) %>%
                                                                 as.data.frame(),
                                                               class.sys = "USDA.TT",
                                                               PiC.type="t"))
  )
  
  NRCS.horizon %<>%
    dplyr::mutate( SLB = hzdepb.r,
                   SLMH = NA,
                   
                   SLLL = wfifteenbar.r/100,
                   SLLL = ifelse(is.na(SLLL), dssat_soil_hydrology$LL[match(TEXTURE,as.character(dssat_soil_hydrology$`SOIL CLASS`))], SLLL),
                   
                   SDUL = wthirdbar.r/100,
                   SDUL = ifelse(is.na(SDUL), dssat_soil_hydrology$DUL[match(TEXTURE,as.character(dssat_soil_hydrology$`SOIL CLASS`))], SDUL),
                   
                   SSAT = wsatiated.r/100,
                   SSAT = ifelse(is.na(SSAT), dssat_soil_hydrology$SAT[match(TEXTURE,as.character(dssat_soil_hydrology$`SOIL CLASS`))], SSAT),
                   SSAT = max(SSAT,SDUL),
                   
                   SRGF = exp(-0.02 * ((hzdepb.r + hzdept.r) / 2)),
                   SRGF = ifelse(hzdepb.r<=15, 1, SRGF),
                   
                   SSKS = ksat.r * (0.36),
                   SBDM = NA,
                   SLOC = om.r,
                   SLCL = claytotal.r,
                   SLSI = silttotal.r,
                   SLCF = NA,
                   SLNI = NA,
                   SLHW = ph1to1h2o.r,
                   SLHB = ph01mcacl2.r,
                   SCEC = cec7.r,
                   SLPX = pbray1.r,
                   SLPT = ptotal.r,
                   SLPO = NA,
                   SLCA = caco3.r/1000,
                   SLAL = extral.r,
                   SLFE = freeiron.r,
                   SLMN = NA,
                   SLBS = NA,
                   SLPA = NA,
                   SLPB = NA,
                   SLKE = NA,
                   SLMG = NA,
                   SLNA = NA,
                   SLSU = NA,
                   SLEC = ec.r) %>%
    dplyr::select(cokey,
                  SLB:SLEC) %>%
    dplyr::group_by(cokey)
  
  
  ## COMPONENT LEVEL DATA ##  
  
  # Functions to get the maximum depth of a layer
  max_depth <- function(component_key){
    NRCS.horizon %>%
      dplyr::filter(cokey == component_key) %>%
      dplyr::summarise(max(SLB)) %$%
      `max(SLB)`
  }
  
  max_depth_vect <- function(component_key_vect){
    component_key_vect %>%
      sapply(max_depth)
  }
  
  NRCS.component <- ssurgo$tabular$component %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na(albedodry.r), cokey %in% NRCS.horizon$cokey) %>%
    dplyr::arrange(cokey) %>%
    dplyr::mutate(`Component percent` = comppct.r,
                  ID_SOIL = cokey,
                  SLSOURCE = compname,
                  SLTX = NA,
                  SLDP = max_depth_vect(ID_SOIL),
                  SLDESCRIP = taxclname,
                  SITE = mukey,
                  COUNTRY = "USA",
                  LAT = "",
                  LONG = "",
                  SCSFAMILY = taxpartsize,
                  SCOM = NA,
                  SALB = albedodry.r,
                  SLU1 = NA,
                  
                  # Drainage class after DSSAT 4.5 manual Vol. 2, Section 1.4.3.2, Table 2
                  SLDR = (dssat_drainage_classes$SLDR)[match(drainagecl,dssat_drainage_classes$`Drainage Class`)],
                  
                  # Runoff potential after DSSAT 4.5 manual Vol. 2, Section 1.4.3.2, Table 2
                  SLRO = (dssat_runoff_potential$SLRO)[match(runoff,dssat_runoff_potential$`Runoff Potential`)],
                  
                  SLNF = 1,
                  SLPF = 1,
                  SMHB = NA,
                  SMPX = NA,
                  SMKE = NA) %>%
    dplyr::select(mukey,cokey,`Component percent`,ID_SOIL:SMKE) %>%
    dplyr::group_by(mukey)
  
  
  ## MAPUNIT LEVEL DATA ##  
  NRCS.mapunit <- ssurgo$spatial
  NRCS.mapunit@data %<>%
    tibble::as_tibble() %>%
    dplyr::left_join(ssurgo$tabular$mapunit %>%
                       dplyr::mutate(mukey = as.factor(mukey)) %>%
                       dplyr::select(mukey,muname)
                       , by = c("MUKEY" = "mukey")) %>%
    dplyr::mutate(muname = gsub(" MLRA 36","",muname)) %>%
    dplyr::select(AREASYMBOL,
                  MUKEY,
                  muname) %>%
    dplyr::mutate(MUKEY = as.factor(MUKEY)) %>%
    dplyr::rename(area = AREASYMBOL,
                  mukey = MUKEY)
  
  return(list(mapunits = NRCS.mapunit,
              components = NRCS.component,
              horizons = NRCS.horizon))
  
}
