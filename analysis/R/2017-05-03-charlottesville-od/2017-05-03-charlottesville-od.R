################ WARNING ###############
# This script will download lots of data and assumes you are running it from the project directory
# It assumes you cloned the dot-map-plus directory and have access to utility scripts in the "helpers" folder
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
for(file in list.files(file.path("analysis","R","utils"))){
  
  source(file.path("analysis","R","utils",file))
  
}

remove(file)

#Set the name of the analysis
postname <- "2017-05-03-charlottesville-od"

#Load up styling
styling <- loadStyling()

#Load relevant geography needed for maps and dot distribution
geographyInMemory <- list(
  blocks = list(
    virginia = getBLOCKS(51)
  ),
  states = getSTATEBOUNDARIES(),
  counties = getCOUNTYBOUNDARIES()
)

#LODES main files have workers who are employed in the state and reside in the state
lodesInMemory <- list(
  va_od_main_JT01_2014.csv = getLODES(st="va",part="main",series="od",type="JT01",year="2014")
)

#Lodes aux files have workers who are employed in the state and reside out of the state
#To get anyone who lives or works in Virginia, we need all aux od files, with the exclusion of Wyoming because it doesn't exist for 2014
stateFIPS <- getSTATEFIPS()
for(abbreviation in tolower(stateFIPS$STUSAB[1:50])){
  
  filename <- paste0(abbreviation,"_od_aux_JT01_2014.csv")
  
  lodesInMemory[[filename]] <- getLODES(st=abbreviation,part="aux",series="od",type="JT01",year="2014")
  
  remove(filename,abbreviation)
  
}

##Dont need to keep the files in a list for this, bind them and create variables to filter on
lodesInMemory <- lodesInMemory %>%
  bind_rows() %>%
  mutate(hState = as.factor(substr(h_geocode,1,2)),
       wState = as.factor(substr(w_geocode,1,2)),
       hCounty = as.factor(substr(h_geocode,3,5)),
       wCounty = as.factor(substr(w_geocode,3,5)),
       hCombined = as.factor(paste0(hState,hCounty)),
       wCombined = as.factor(paste0(wState,wCounty)))

####Look at cville (combined fips is 51540)
charlottesvilleJobs <- lodesInMemory %>%
  filter(wCombined == "51540") 

###Find how many workers other localities are sending to Charlottesville and match named to fips codes
countyFIPS <- getCOUNTYFIPS() %>%
  mutate(combinedFIPS = paste0(STATEFP,COUNTYFP))

topCounties <- charlottesvilleJobs %>%
  group_by(hCombined) %>%
  summarise(S000 = sum(S000)) %>%
  arrange(desc(S000)) %>%
  left_join(countyFIPS,by=c("hCombined"="combinedFIPS"))

topCounties.fips <- topCounties$hCombined[1:7]

###Prepare to make the synthetic population from LODES origin-destination row pairs.
###Because it takes a while to create, I'll prep to save the population to disk so we only have to go through this once.

actors.filedir <- file.path("output",postname,"data")
actors.filepath <- file.path(actors.filedir,"actors.csv")

###Read the csv created if this script has been run befor.
if(file.exists(actors.filepath)){
  actors <- read_csv(actors.filepath)
}else{
  ### If not:
  ### Create the synthetic dot population so we can assign random home and work coordinates to each job in the dataset
  ### Filter for only workers living or working in the top counties and assign labels
  ### Labels are county name strings matched from the fips if a top county, else the "other" categories.
  actors <- lodesInMemory %>%
    filter(hCombined %in% topCounties.fips | wCombined %in% topCounties.fips) %>%
    mutate(hLabel = ifelse(hCombined %in% topCounties.fips,countyFIPS$COUNTYNAME[match(as.character(hCombined),countyFIPS$combinedFIPS)],ifelse(hState == "51","Other: In State","Other: Out of State")),
           wLabel = ifelse(wCombined %in% topCounties.fips,countyFIPS$COUNTYNAME[match(as.character(wCombined),countyFIPS$combinedFIPS)],ifelse(wState == "51","Other: In State","Other: Out of State")))
  
  ### Prepare matrix to receive synthetic rows
  tmpMatrix <- matrix(nrow = sum(actors$S000), ncol = 7)
  
  tmpRows <- NULL
  r <- 1
  for(i in 1:nrow(actors)){
    ### Generate X rows of population for each OD pair, where X is the total workers S000 in that row. See genLodesPop in the utils directory for details.
    tmpRows <- genLodesPop.od(actors[i,],c("hLabel","wLabel","w_geocode","h_geocode"))
    
    ### Assign to reserved rows in tmpMatrix and increase row count
    tmpMatrix[r:(r+actors$S000[i]-1),1:7] <- tmpRows
    r <- r + actors$S000[i]
  }
  remove(r)
  
  colnames(tmpMatrix) <- colnames(tmpRows)
  
  tmpActors <- as.data.frame(tmpMatrix)
  
  ### Setup columns in the matrix to receive random coordinates inside home and work blocks.
  for(v in c("hLat","hLong","wLat","wLong")){
    tmpActors[[v]] <- numeric(nrow(tmpActors))
    remove(v)
  }
  
  for(i in 1:nrow(tmpActors)){
    ### Assign the points based on block if in Virginia, points based on state if out of Virginia.
    if(tmpActors$hLabel[i] != "Other: Out of State"){
      tmpointHome <- randomPoint(as.character(tmpActors$h_geocode[i]),geographyInMemory$blocks$virginia)
      tmpActors$hLong[i] <- tmpointHome[,1]
      tmpActors$hLat[i] <- tmpointHome[,2]    
    }else{
      tmpointHome <- randomPoint(substr(tmpActors$h_geocode[i],1,2),geographyInMemory$states,idvar="GEOID")
      tmpActors$hLong[i] <- tmpointHome[,1]
      tmpActors$hLat[i] <- tmpointHome[,2]       
    }
    
    if(tmpActors$wLabel[i] != "Other: Out of State"){
      tmpointWork <- randomPoint(as.character(tmpActors$w_geocode[i]),geographyInMemory$blocks$virginia)
      tmpActors$wLong[i] <- tmpointWork[,1]
      tmpActors$wLat[i] <- tmpointWork[,2]    
    }else{
      tmpointWork <- randomPoint(as.character(substr(tmpActors$w_geocode[i],1,2)),geographyInMemory$states,idvar="GEOID")
      tmpActors$wLong[i] <- tmpointWork[,1]
      tmpActors$wLat[i] <- tmpointWork[,2]
    }
    
    if(i %% 1000 == 0){
      #beep("treasure")
    }else if(i %% 100 == 0){
      print(i)
    }
  }
  remove(i)
  
  ### Save the result so we don't have to run this again if we already have
  dir.create(actors.filedir, showWarnings = FALSE, recursive = TRUE)
  
  write_csv(tmpActors,path = actors.filepath)
  
  actors <- tmpActors
  #remove(tmpActors,tmpMatrix,tmpointHome,tmpointWork,tmpRows)
}

#### Done with the bigger lodesInMemory, remove it over memory concerns if dealing with a lot of states.
#remove(lodesInMemory)


################################ Visualization Prep


### Specify order of factor levels, hardcoded but eh
actors$Residence <- factor(actors$hLabel,levels = c("Charlottesville city","Albemarle County","Greene County","Fluvanna County","Nelson County","Louisa County","Augusta County","Other: In State","Other: Out of State"))

### Set storyboard to TRUE if you're getting the settings just right to only record the first and last frame of a "shot".
storyboard <- FALSE

### Because this is an animation, I'm using "shot" to describe different parts of the analysis. Generated frames will be organized by shot in the output folder.
shot <- "1-establishing-shot"
frames <- 50

### Set the block geography to display on the map, just the top localities
mapdf <- tidy(geographyInMemory$blocks$virginia[geographyInMemory$blocks$virginia$COUNTYFP10 %in% c("540","003","065","079","109","015","125"),])

### Set the county geography. Everywhere since this doesn't take up much resources
countydf <- tidy(geographyInMemory$counties[geographyInMemory$counties$STATEFP == "51",],group = group)

### Set the camera parameters to pass to the drawMap function (see the utils folder for details on drawMap)
camera <- list(
  lat = 38.031642,
  long = -78.480591,
  meters = 120000
)

camera$view <- calcView(latitude = camera$lat, longitude = camera$long, meters = camera$meters)

#Work to home

for(i in 1:frames){
  
  # Interpolate commute
  tmpData <- mutate(actors,
                    midLong = hLong + (wLong - hLong)*(i - 1)/(frames-1),
                    midLat = hLat + (wLat - hLat)*(i - 1)/(frames-1))
  
  # Prepare output directories
  dir.create(file.path("output",postname,"frames",shot),recursive = TRUE,showWarnings = FALSE)
  dir.create(file.path("output",postname,"frames","storyboard"),recursive = TRUE,showWarnings = FALSE)
  
  # Set dynamic titles based on frame
  if(i == 1){
    maintitle <- "At Residence"
  }else if(i == frames){
    maintitle <- "At Administrative Workplace"
  }else{
    maintitle <- "Interpolated Commute"
  }
  
  subtitle <- "Employed persons who live or work their primary job in \nCharlottesville, Albemarle, Augusta, Fluvanna,\nNelson, Greene, and Louisa"
  
  # Choose variable in the Actors df to color code by
  focusvar <- "Residence"
  
  #Record storyboard frames no matter what
  if(i == 1 | i == frames){
    drawMap(pointsize = 1.5,
            title=maintitle,
            subtitle=subtitle,
            countyoutlines = countydf,
            mapData = mapdf,
            pointdata = tmpData,
            colpal = styling$colors$main,
            colourvar = focusvar,
            pointx="midLong",
            pointy="midLat",
            camera = camera,
            path=file.path("output",postname,"frames","storyboard",paste0(shot,i,".png"))
            )  
  }
  if(!storyboard){
    drawMap(pointsize = 1.5,
            title=maintitle,
            subtitle=subtitle,
            countyoutlines = countydf,
            mapData = mapdf,
            pointdata = tmpData,
            colpal = styling$colors$main,
            colourvar = focusvar,
            pointx="midLong",
            pointy="midLat",
            camera = camera,
            path=file.path("output",postname,"frames",shot,paste0(shot,i,".png"))
            )
  }
  
  remove(tmpData)
}


shot <- "2-establishing-shot-close"
frames <- 50

mapdf <- tidy(geographyInMemory$blocks$virginia[geographyInMemory$blocks$virginia$COUNTYFP10 %in% c("540","003","065","079","109","015","125"),])

countydf <- tidy(geographyInMemory$counties[geographyInMemory$counties$STATEFP == "51",],group = group)

camera <- list(
  lat = 38.031642,
  long = -78.480591,
  meters = 12000
)
camera$view <- calcView(latitude = camera$lat, longitude = camera$long, meters = camera$meters)

#Work to home

for(i in 1:frames){
  
  tmpData <- mutate(actors,
                    midLong = hLong + (wLong - hLong)*(i - 1)/(frames-1),
                    midLat = hLat + (wLat - hLat)*(i - 1)/(frames-1))
  #filter(hCounty %in% c("51540","51820"))
  dir.create(file.path("output",postname,"frames",shot),recursive = TRUE,showWarnings = FALSE)
  dir.create(file.path("output",postname,"frames","storyboard"),recursive = TRUE,showWarnings = FALSE)
  
  if(i == 1){
    maintitle <- "At Residence"
  }else if(i == frames){
    maintitle <- "At Administrative Workplace"
  }else{
    maintitle <- "Interpolated Commute"
  }
  
  focusvar <- "Residence"
  
  if(i == 1 | i == frames){
    drawMap(pointsize = 1.5,
            title=maintitle,
            subtitle=subtitle,
            countyoutlines = countydf,
            mapData = mapdf,
            pointdata = tmpData,
            colpal = styling$colors$main,
            colourvar = focusvar,
            pointx="midLong",
            pointy="midLat",
            camera = camera,
            path=file.path("output",postname,"frames","storyboard",paste0(shot,i,".png"))
            )  
  }
  if(!storyboard){
    drawMap(pointsize = 1.5,
            title=maintitle,
            subtitle=subtitle,
            countyoutlines = countydf,
            mapData = mapdf,
            pointdata = tmpData,
            colpal = styling$colors$main,
            colourvar = focusvar,
            pointx="midLong",
            pointy="midLat",
            camera = camera,
            path=file.path("output",postname,"frames",shot,paste0(shot,i,".png"))
            )
  }
  
  remove(tmpData)
}
