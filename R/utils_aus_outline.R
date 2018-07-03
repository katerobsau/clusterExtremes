utils_mainland <- function(){
  mainland_region = ozRegion(sections = c(1:5,7))
  mainland_xPnts = mainland_yPnts = NULL
  for(i in mainland_region$lines){
    mainland_xPnts = c(mainland_xPnts, i$x)
    mainland_yPnts = c(mainland_yPnts, i$y)
  }
  mainland_df = data.frame(Long = mainland_xPnts, Lat = mainland_yPnts)
  return(mainland_df)
}

utils_tasmania <- function(){
  tas_region = ozRegion(sections = 6)
  tas_xPnts = tas_yPnts = NULL
  for(i in tas_region$lines){
    tas_xPnts = c(tas_xPnts, i$x)
    tas_yPnts = c(tas_yPnts, i$y)
  }
  tas_df = data.frame(Long = tas_xPnts, Lat = tas_yPnts)
  return(tas_df)
}
