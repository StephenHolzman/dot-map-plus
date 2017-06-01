genLodesPop.od <- function(odpair,rowsToKeep){
  
  #For a row of LODES origin-destination data, explode it so that each job gets one row and a age=industry-earnings category.
  rowsToAdd <- c("Age","Industry","Earnings")
  rows <- c(rowsToKeep,rowsToAdd)
  newSet <- matrix(nrow = as.numeric(odpair[,"S000"]),ncol = length(rows))
  colnames(newSet) <- c(rows)
  
  for(k in rowsToKeep){
    newSet[,k] <- odpair[[k]]
  }
  
  newSet[,"Age"] <- c(rep("Age 29 or Younger",odpair$SA01),rep("Between Ages 30 and 54",odpair$SA02),rep("Age 55 or Older",odpair$SA03))
  newSet[,"Industry"] <- c(rep("Goods Producing",odpair$SI01),rep("Trade, Transportation, and Utilities",odpair$SI02),rep("All Other Industries",odpair$SI03))
  newSet[,"Earnings"] <- c(rep("Less Than $1,251 Per Month",odpair$SE01),rep("$1,251 to $3,333 Per Month",odpair$SE02),rep("More Than $3,333 Per Month",odpair$SE03))
  
  newSet
}


genLodesPop.wac <- function(wacrow,rowsToKeep){
  
  rowsToAdd <- c("Age","Earnings","Industry","Race","Ethnicity","Education","Sex","Firm_Age","Firm_Size")
  rows <- c(rowsToKeep,rowsToAdd)
  newSet <- matrix(nrow = as.numeric(wacrow[,"C000"]),ncol = length(rows))
  colnames(newSet) <- c(rows)
  
  for(k in rowsToKeep){
    newSet[,k] <- wacrow[[k]]
  }
  
  newSet[,"Age"] <- c(rep("Age 29 or Younger",wacrow$CA01),
                      rep("Age 30 to 54",wacrow$CA02),
                      rep("Age 55 or Older",wacrow$CA03)
  )
  
  newSet[,"Earnings"] <- c(rep("$1250/month or less",wacrow$CE01),
                           rep("$1,251/month to $3,333/month",wacrow$CE02),
                           rep("$3,333/month or more",wacrow$CE03)
  )
  
  newSet[,"Industry"] <- c(rep("Agriculture, Forestry, Fishing, and Hunting",wacrow$CNS01),
                           rep("Mining, Quarrying, and Oil and Gas Extraction",wacrow$CNS02),
                           rep("Utilities",wacrow$CNS03),
                           rep("Construction",wacrow$CNS04),
                           rep("Manufacturing",wacrow$CNS05),
                           rep("Wholesale Trade",wacrow$CNS06),
                           rep("Retail Trade",wacrow$CNS07),
                           rep("Transportation and Warehousing",wacrow$CNS08),
                           rep("Information",wacrow$CNS09),
                           rep("Finance and Insurance",wacrow$CNS10),
                           rep("Real Estate and Rental and Leasing",wacrow$CNS11),
                           rep("Professional, Scientific, and Technical Services",wacrow$CNS12),
                           rep("Management of Companies and Enterprises",wacrow$CNS13),
                           rep("Administrative and Suppport and Waste Management and Remediation Services",wacrow$CNS14),
                           rep("Educational Services",wacrow$CNS15),
                           rep("Health Care and Social Assistance",wacrow$CNS16),
                           rep("Arts, Entertainment, and Recreation",wacrow$CNS17),
                           rep("Accommodation and Food Services",wacrow$CNS18),
                           rep("Other Services",wacrow$CNS19),
                           rep("Public Administration",wacrow$CNS20)
  )
  
  newSet[,"Race"] <- c(rep("White",wacrow$CR01),
                       rep("Black",wacrow$CR02),
                       rep("AIAN",wacrow$CR03),
                       rep("Asian",wacrow$CR04),
                       rep("NHPI",wacrow$CR05),
                       rep("Two or More",wacrow$CR07) #CR07 is correct, CR06 is skipped
  )
  
  newSet[,"Ethnicity"] <- c(rep("Not Hispanic or Latino",wacrow$CT01),
                            rep("Hispanic or Latino",wacrow$CT02),
                            rep("Not Reported",wacrow$C000 - wacrow$CT01 - wacrow$CT02)
  )
  
  newSet[,"Education"] <- c(rep("Less than High School",wacrow$CD01),
                            rep("High School",wacrow$CD02),
                            rep("Some College or Associate degree",wacrow$CD03),
                            rep("Bachelor's or Advanced",wacrow$CD04),
                            rep("Not Reported",wacrow$C000 - wacrow$CD01 - wacrow$CD02 - wacrow$CD03 - wacrow$CD04)
  )
  
  newSet[,"Sex"] <- c(rep("Male",wacrow$CS01),
                      rep("Female",wacrow$CS02))
  
  newSet[,"Firm_Age"] <- c(rep("0-1 Years",wacrow$CFA01),
                           rep("2-3 Years",wacrow$CFA02),
                           rep("4-5 Years",wacrow$CFA03),
                           rep("6-10 Years",wacrow$CFA04),
                           rep("11+ Years",wacrow$CFA05),
                           rep("Not Reported",wacrow$C000 - wacrow$CFA01 - wacrow$CFA02 - wacrow$CFA03 - wacrow$CFA04 - wacrow$CFA05)
  )
  
  newSet[,"Firm_Size"] <- c(rep("0-19 Employees",wacrow$CFS01),
                            rep("20-49 Employees",wacrow$CFS02),
                            rep("50-249 Employees",wacrow$CFS03),
                            rep("250-499 Employees",wacrow$CFS04),
                            rep("500+ Employees",wacrow$CFS05),
                            rep("Not Reported",wacrow$C000 - wacrow$CFS01 - wacrow$CFS02 - wacrow$CFS03 - wacrow$CFS04 - wacrow$CFS05)
  )
  
  
  newSet
}