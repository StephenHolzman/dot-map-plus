getCOUNTYFIPS <- function(){
  url <- "http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt"
  
  #Create a 'FIPS' directory in the 'data' directory in case it doesn't exist
  dir <- file.path("input","data","uscensus","FIPS","COUNTY")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  
  localpath <- file.path(dir,"national_county.txt")
  
  #check if the file already exists
  if(!file.exists(localpath)){
    
    #If it doesn't exist, download it
    download.file(url,localpath,method="auto")
  }
  
  read.csv(localpath,col.names = c("STATE","STATEFP","COUNTYFP","COUNTYNAME","CLASSFP"),colClasses = c("factor","factor","character","character","factor"))
  
}