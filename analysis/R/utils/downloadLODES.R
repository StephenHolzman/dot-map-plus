downloadLODES <- function(st = "va",series="od", part = "main", type = "JT01", year = "2014"){
  
  #Create a LODES directory in the 'data' directory in case it doesn't exist
  #Make an exception for aux since it's a restricted folder name in OS X
  if(part=="aux"){
    dir <- file.path("input","data","uscensus","LODES",series,year,type,"auxillary")
  }else{
    dir <- file.path("input","data","uscensus","LODES",series,year,type,part)
  }
  
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  
  #Paste together the file name of what we want from the census site
  filename <- paste0(st,"_",series,"_",part,"_",type,"_",year,".csv.gz")
  
  #Build the url
  url <- paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",st,"/",series,"/",filename)
  
  #Build the local path
  localpath <- file.path(dir,filename)
  
  #check if the file already exists
  if(!file.exists(substr(localpath,start=1,nchar(localpath)-3))){
    
    #If it doesn't exist, download it
    download.file(url,localpath,method="auto")
    
    #Then unzip it
    gunzip(localpath)
  }
}