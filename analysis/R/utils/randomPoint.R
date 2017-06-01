randomPoint <- function(polyid,polyframe,idvar="GEOID10"){
  focusPoly <- NULL
  focusPoly <- polyframe[polyframe[[idvar]] %in% c(polyid),]
  #borrowed logic from rgeos label function at https://github.com/cran/rgeos/blob/5fab5c53070e6ab3e9ff136418caebbec84ee082/R/labelpt.R
  repeat {
    coord.rand = tryCatch(spsample(focusPoly, n = 1, type = "random",pretty = FALSE), error = function(x) NULL)
    if(!is.null(coord.rand)) break
  }
  coordinates(coord.rand)
}