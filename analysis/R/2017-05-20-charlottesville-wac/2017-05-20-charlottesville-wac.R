################ WARNING ###############
# This script will download lots of data and assumes you are running it from the project directory
# It assumes you cloned the dot-map-plus directory and have access to utility scripts in the "R" folder
# It will expect "input" and "output" folders in that directory and create them if they don't exist yet
# If you have already downloaded a file and have it in the expected subdirectory, it will skip downloading and simply read it into the environment
# Rec running with at least 16gb of RAM

library(tidyverse)
library(broom)
library(grid)
library(gridExtra)
library(rsvg)
library(rgdal)
library(geosphere)
library(R.utils)
#Load utility scripts
for(file in list.files("R")){
  source(file.path("R",file))
}
remove(file)

postname <- "2017-05-20-charlottesville-wac"

#Load relevant geography needed for maps and dot distribution
geographyInMemory <- list(
  blocks = list(
    virginia = getBLOCKS(51)
  ),
  states = getSTATEBOUNDARIES(),
  counties = getCOUNTYBOUNDARIES()
)

#Work area characteristics for Virginia.
lodesInMemory <- list(
  va_wac_S000_JT00_2014.csv = getLODES(st="va",part="S000",series="wac",type="JT00",year="2014")
)

##Dont need to keep the files in a list for this, bind them and create variables to filter on
lodesInMemory <- lodesInMemory %>%
  bind_rows() %>%
  mutate(wState = as.factor(substr(w_geocode,1,2)),
         wCounty = as.factor(substr(w_geocode,3,5)),
         wCombined = as.factor(paste0(wState,wCounty)))


actors.filedir <- file.path("output","posts",postname,"data")
actors.filepath <- file.path(actors.filedir,"actors.csv")

if(file.exists(actors.filepath)){
  actors <- read_csv(actors.filepath)
}else{
  #### Create the synthetic dot population so we can assign random home and work coordinates to each job in the dataset
  #### Filter for only workers living or working in the top counties and assign labels
  actors <- lodesInMemory %>%
    filter(wCombined %in% c("51540","51003"))

  tmpMatrix <- matrix(nrow = sum(actors$C000), ncol = 10)
  
  tmpRows <- NULL
  r <- 1
  for(i in 1:nrow(actors)){
    tmpRows <- genLodesPop.wac(actors[i,],c("w_geocode"))
    
    tmpMatrix[r:(r+actors$C000[i]-1),1:10] <- tmpRows
    r <- r + actors$C000[i]
  }
  remove(r)
  
  colnames(tmpMatrix) <- colnames(tmpRows)
  
  tmpActors <- as.data.frame(tmpMatrix)
  
  for(v in c("wLat","wLong")){
    tmpActors[[v]] <- numeric(nrow(tmpActors))
    remove(v)
  }
  
  for(i in 1:nrow(tmpActors)){
      tmpointWork <- randomPoint(as.character(substr(tmpActors$w_geocode[i],1,2)),geographyInMemory$states,idvar="GEOID")
      tmpActors$wLong[i] <- tmpointWork[,1]
      tmpActors$wLat[i] <- tmpointWork[,2]
    
    if(i %% 1000 == 0){
      #beep("treasure")
    }else if(i %% 100 == 0){
      print(i)
    }
  }
  remove(i)
  
  dir.create(actors.filedir, showWarnings = FALSE, recursive = TRUE)
  
  write_csv(tmpActors,path = actors.filepath)
  
  actors <- tmpActors
  remove(tmpActors,tmpMatrix,tmpointHome,tmpointWork,tmpRows)
}
