prepDSSATWeather <- function(out.dir, coords, location, annual.series.tmin,annual.series.tmax,annual.series.prcp, daily.series.tmin, daily.series.tmax, daily.series.prcp, recon.days, recon.years){
  
  daily.series.tmin.retro <- generateDailySeries(annual.series=annual.series.tmin, single.year.daily.training=daily.series.tmin,recon.days=recon.days,recon.years=recon.years, fun="mean")
  daily.series.tmax.retro <- generateDailySeries(annual.series=annual.series.tmax, single.year.daily.training=daily.series.tmax,recon.days=recon.days,recon.years=recon.years, fun="mean")
  daily.series.prcp.retro <- generateDailySeries(annual.series=annual.series.prcp, single.year.daily.training=daily.series.prcp,recon.days=recon.days,recon.years=recon.years, fun="sum")
  
  
  
  year.vector <- rep(recon.years,each=length(recon.days))
  year.vector <- formatC(year.vector, width = 4, format = "d", flag = "0") 
  day.vector <- rep(recon.days,times=length(recon.years))
  day.vector <- formatC(day.vector, width = 3, format = "d", flag = "0") 
  
  DATES <- as.Date(paste(year.vector,day.vector,sep="-"),format="%Y-%j")
  SRAD <- mh(Tmin=daily.series.tmin.retro, Tmax=daily.series.tmax.retro, days=DATES, lat=coords[2])
  
  year.vector <- substr(year.vector,3,4)
  
  #   header <- paste("@DATE  SRAD  TMIN  TMAX  RAIN", sep='')
  #   daily.lines <- paste(year.vector,day.vector,"  ",format(-99, width=4, digits=1, nsmall=1),"  ", format(round(daily.series.tmin.retro, digits=1), width=4, digits=1, nsmall=1),"  ",format(round(daily.series.tmax.retro, digits=1), width=4, digits=1, nsmall=1),"  ",format(round(daily.series.prcp.retro*10, digits=1), width=4, digits=1, nsmall=1), sep='')
  
  header1 <- paste("*WEATHER DATA: ",location,sep='')
  header2 <- ""
  header3 <- "@ INSI      LAT     LONG"
  header4 <- "  CELL   27.398  -81.940"
  header5 <- "@DATE  SRAD  TMIN  TMAX  RAIN"
  daily.lines <- paste(year.vector,day.vector,"  ",format(round(SRAD, digits=1), width=4, digits=1, nsmall=1),"  ", format(round(daily.series.tmin.retro, digits=1), width=4, digits=1, nsmall=1),"  ",format(round(daily.series.tmax.retro, digits=1), width=4, digits=1, nsmall=1),"  ",format(round(daily.series.prcp.retro*10, digits=1), width=4, digits=1, nsmall=1), sep='')
  
  
  
  #   daily.lines <- paste(year.vector,day.vector,"  ",format(round(daily.series.tmin.retro, digits=1), width=4, digits=1, nsmall=1),"  ",format(round(daily.series.tmax.retro, digits=1), width=4, digits=1, nsmall=1),"  ",format(round(daily.series.prcp.retro*10, digits=1), width=4, digits=1, nsmall=1), sep='')
  
  
  #   format(daily.series.tmin.retro[1], width=5, digits=1, nsmall=1)
  
  #   daily.series <- data.frame(DATE=paste(year.vector,day.vector,sep=''), SRAD=round(10.01,digits=1), TMIN=round(daily.series.tmin.retro,digits=1), TMAX=round(daily.series.tmax.retro,digits=1), RAIN=round(daily.series.prcp.retro*10,digits=1))
  
  fileConn<-file(paste(out.dir,location,".WTH",sep=''))
  writeLines(c(header1,header2,header3,header4,header5,daily.lines), fileConn)
  close(fileConn)
  
  
  #   write.fwf(daily.series,file=paste(out.dir,location,".WTH",sep=''), rownames=F, colnames=F, sep="  ", na="", append=T)
  
}
