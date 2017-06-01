getLODES <- function(st = "va", series="od",part = "main", type = "JT01", year = "2014"){
  
  downloadLODES(st=st, series=series, part=part, type=type, year=year)
  
  if(part=="aux"){
    dir <- file.path("input","data","uscensus","LODES",series,year,type,"auxillary")
  }else{
    dir <- file.path("input","data","uscensus","LODES",series,year,type,part)
  }
  filename <- paste0(st,"_",series,"_",part,"_",type,"_",year,".csv")
  
  if(series=="od"){
    col_types <- "cciiiiiiiiiii"
  }else{
    col_types <- paste0(c("c",rep("i",52)),collapse="")
  }
  
  
  read_csv(file.path(dir,filename),col_types = col_types)
  
}