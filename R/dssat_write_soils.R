dssat_write_soils <- function(soils,
                              output.dir = ".",
                              clean = FALSE){
  
  if(!inherits(soils,
           what = "soils"))
    stop("dssat_write_soils() requires an S3 object of class 'soils' from the dssatr library.")
  
  dir.create(output.dir,
             showWarnings = FALSE,
             recursive = TRUE)
  
  if(clean)
    unlink(stringr::str_c(output.dir,"/soil.sol"))
  
  purrr::walk(soils$soils,
              dssat_write_soil,
              output.dir = output.dir)
  
  return(stringr::str_c(output.dir,"/soil.sol"))
  
}
