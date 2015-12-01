generateDailySeries <- function(annual.series, single.year.daily.training, recon.days, recon.years, fun){
  if(length(recon.years)!=length(annual.series)){
    stop("ERROR! Length of recon years not the same as length of annual series.")
  }
  
  #   year.vector <- rep(recon.years,each=length(recon.days))
  #   day.vector <- rep(recon.days,times=length(recon.years))
  
  agg.function <- get(fun)
  annual.daily.training.recon <- single.year.daily.training[recon.days]
  daily.training.norm.recon <- agg.function(annual.daily.training.recon)
  
  daily.series <- unlist(lapply(annual.series, FUN=function(x,...){deltaAdjust(target.norm=x,daily=annual.daily.training.recon, daily.norm=daily.training.norm.recon, fun=fun)}))
  
  #   data.out <- data.frame(YEAR=year.vector,DAY=day.vector,SERIES=daily.series)
  
  return(daily.series)
}
