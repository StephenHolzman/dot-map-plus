getSTATEBOUNDARIES <- function(year="2016"){
  
  #Construct the census url
  url <- paste0("https://www2.census.gov/geo/tiger/TIGER2016/STATE/tl_",year,"_us_state.zip")
  
  #Create a 'FIPS' directory in the 'data' directory in case it doesn't exist
  dir <- file.path("input","shapefiles","states",year)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  
  localpath <- file.path(dir,paste0("tl_",year,"_us_state.zip"))
  
  #check if the file already exists
  if(!file.exists(localpath)){
    
    #If it doesn't exist, download it
    download.file(url,localpath,method="auto")
    unzip(localpath,exdir = dir)
  }
  #Then unzip it
  
  # Return the shapefile
  require(rgdal)
  readOGR(dsn = dir, layer = "tl_2016_us_state")
}