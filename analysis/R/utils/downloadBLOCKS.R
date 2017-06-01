downloadBLOCKS <- function(statefips,dir,year){
 
  #Paste together the file name of what we want from the census site
  filename <- paste0("tl_",year,"_",statefips,"_tabblock10.zip")
  
  url <- paste0("https://www2.census.gov/geo/tiger/TIGER",year,"/TABBLOCK/",filename)

  #local path to file
  localpath <- file.path(dir,filename)
  
  if(!file.exists(localpath)){
    download.file(url,localpath,method="auto")
  }
  
  unzip(localpath,exdir = dir)
}