getCOUNTYBOUNDARIES <- function(year="2016"){
  
  #Construct the census url
  url <- paste0("ftp://ftp2.census.gov/geo/tiger/TIGER",year,"/COUNTY/tl_",year,"_us_county.zip")
  
  #Create a 'FIPS' directory in the 'data' directory in case it doesn't exist
  dir <- file.path("input","shapefiles","counties",year)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  
  localpath <- file.path(dir,paste0("tl_",year,"_us_county.zip"))
  
  #check if the file already exists
  if(!file.exists(localpath)){
    
    #If it doesn't exist, download it
    download.file(url,localpath,method="auto")
    unzip(localpath,exdir = dir)
  }
  #Then unzip it
  
  # Return the shapefile
  require(rgdal)
  rgdal::readOGR(dsn = dir, layer = paste0("tl_",year,"_us_county"))
}