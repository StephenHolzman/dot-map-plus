calcView <- function(longitude,latitude,meters){
  
  #states <- getSTATEBOUNDARIES()
  
  p <- SpatialPoints(cbind(longitude,latitude))
  
  ur <- destPoint(p = p, b = 63.053, d = meters)
  ul <- destPoint(p = p, b = -63.053, d = meters)
  br <- destPoint(p = p, b = 116.947, d = meters)
  bl <- destPoint(p = p, b = -116.947, d = meters)
  
  pol <- Polygon(rbind(ur, br, bl, ul))
  
  pol <- Polygons(list(pol),"p1")
  
  pol <- SpatialPolygons(list(pol))
  
  pol
  
}