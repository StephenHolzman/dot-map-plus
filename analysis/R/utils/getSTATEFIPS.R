getSTATEFIPS <- function(){
  url <- "http://www2.census.gov/geo/docs/reference/state.txt"
  
  #Create a 'FIPS' directory in the 'data' directory in case it doesn't exist
  dir <- file.path("input","data","uscensus","FIPS","STATE")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  
  localpath <- file.path(dir,"state.txt")
  
  #check if the file already exists
  if(!file.exists(localpath)){
    
    #If it doesn't exist, download it
    download.file(url,localpath,method="auto")
  }
  
  read.delim(localpath,colClasses = c("character","character","character","character"),sep="|")
  
}