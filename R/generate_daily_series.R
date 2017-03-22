generate_daily_series <- function(annual.series, single.year.daily.training, recon.days, recon.years, fun){
  if(length(recon.years) != length(annual.series)){
    stop("ERROR! Length of recon years not the same as length of annual series.")
  }

  agg.function <- get(fun)
  annual.daily.training.recon <- single.year.daily.training[recon.days]
  daily.training.norm.recon <- agg.function(annual.daily.training.recon)

  daily.series <- annual.series %>%
    lapply(FUN = function(x,...){
      delta_adjust(target.norm = x,
                   daily = annual.daily.training.recon,
                   daily.norm = daily.training.norm.recon,
                   fun = fun)
    }) %>%
    unlist()

  return(daily.series)
}
