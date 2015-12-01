deltaAdjust <- function(target.norm,daily,daily.norm, fun){
  
  if(fun=="sum"){
    target.daily <- daily*(target.norm/daily.norm)
  }else if(fun=="mean"){
    target.daily <- daily+(target.norm-daily.norm)
  }
  
  return(target.daily)
}