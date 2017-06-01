getBLOCKS <- function(statefips,year=2016){
  #create a blocks directory if it doesn't exist.
  dir <- file.path("input","shapefiles","blocks",year,statefips)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  
  #only downloads if the file doesn't already exist
  downloadBLOCKS(statefips=statefips,dir=dir,year=year)
  
  require(rgdal)
  rgdal::readOGR(dsn = dir, layer = paste0("tl_",year,"_",statefips,"_tabblock10"))
}