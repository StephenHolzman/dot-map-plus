loadStyling <- function(){
  
  styling <- list()
  
  #logos
  styling$logos$icon$svg <- rsvg("/Volumes/Storage/WCCPPDRGlogo.svg",height = 75)
  
  styling$colors$main <- c("#FF7003","#235281","#a8ad00","#9e2a2b","#00812e","salmon","blue","#993290","#3b1f60") #vector of hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$colors$political <- c("red","white","blue") #alternate colors for political spectrum
  styling$colors$financial <- c("dark green","green","light green") #alternate colors for less money to more money
  styling$colors$population <- c("dark orange","orange","light orange") #alternate colors for less population to more population
  
  #header
  styling$header$color <- "#F1EEDE" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$header$height <- 150 #given in pixels. suggest between 130-200
  
  #header$title
  styling$header$title$font$family <- "Adobe Caslon Pro" #Use strings (ex: "Adobe Myriad Pro"). Note users have to have font installed on personal machines.
  styling$header$title$font$color <- "#002654" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$header$title$font$face <- "bold" #options are "plain","bold","italic","bold.italic"
  styling$header$title$font$size <- 36 #suggest between 24-40
  styling$header$title$align <- "left" #options are "left", "right", and "center"
  
  #header$subtitle
  styling$header$subtitle$font$family <- "Adobe Caslon Pro" #Use strings (ex: "Adobe Myriad Pro"). Note users have to have font installed on personal machines.
  styling$header$subtitle$font$color <- "#002654" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$header$subtitle$font$face <- "bold" #options are bold, italic, bold-italic, or NULL for regular
  styling$header$subtitle$font$size <- 18 #suggest half the size of title
  styling$header$subtitle$align <- "left" #options are left, right, and center
  
  #footer
  styling$footer$color <- "#002654" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$footer$height <- 75 #given in pixels. suggest between 130-200
  
  #footer$cite
  styling$footer$cite$font$family <- "Adobe Caslon Pro" #Use strings (ex: "Adobe Myriad Pro"). Note users have to have font installed on personal machines.
  styling$footer$cite$font$color <- "white" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$footer$cite$font$face <- "plain" #options are bold, italic, bold-italic, or NULL for regular
  styling$footer$cite$font$size <- 12 #suggest 16 and match with author fontsize
  
  #footer$author
  styling$footer$author$font$family <- "Adobe Caslon Pro" #Use strings (ex: "Adobe Myriad Pro"). Note users have to have font installed on personal machines.
  styling$footer$author$font$color <- "white" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$footer$author$font$face <- "plain" #options are bold, italic, bold-italic, or NULL for regular
  styling$footer$author$font$size <- 16 #suggest 16 and match with cite fontsize
  
  #legend
  styling$legend$color <- "#F1EEDE" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  
  #legend$title
  styling$legend$title$font$family <- "Myriad Pro Condensed" #Use strings (ex: "Adobe Myriad Pro"). Note users have to have font installed on personal machines.
  styling$legend$title$font$color <- "#002654" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$legend$title$font$face <- "plain" #options are bold, italic, bold-italic, or NULL for regular
  styling$legend$title$font$size <- 22 #suggest 22
  
  #legend$labels		
  styling$legend$labels$font$family <- "Myriad Pro Condensed" #Use strings (ex: "Adobe Myriad Pro"). Note users have to have font installed on personal machines.
  styling$legend$labels$font$color <- "#002654" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$legend$labels$font$face <- "plain" #options are bold, italic, bold-italic, or NULL for regular
  styling$legend$labels$font$size <- 18 #suggest 18
  
  #facet$labels
  styling$facet$labels$font$family <- "Myriad Pro Condensed" #Use strings (ex: "Adobe Myriad Pro"). Note users have to have font installed on personal machines.
  styling$facet$labels$font$color <- "#002654" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$facet$labels$font$face <- "plain" #options are bold, italic, bold-italic, or NULL for regular
  styling$facet$labels$font$size <- 18 #suggest 18
  styling$facet$labels$color <- "#002654" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")		
  
  #axis$labels
  styling$axis$labels$font$family <- "Myriad Pro Condensed"#Use strings (ex: "Adobe Myriad Pro"). Note users have to have font installed on personal machines.
  styling$axis$labels$font$color <- "#002654" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$axis$labels$font$face <- "plain" #options are bold, italic, bold-italic, or NULL for regular
  styling$axis$labels$font$size <- 22 #suggest
  
  #axis$lines
  styling$axis$lines$color <- "#AAAAAA"
  styling$axis$lines$size <- 2
  
  #tick$labels
  styling$tick$labels$font$family <- "Myriad Pro Condensed" #Use strings (ex: "Adobe Myriad Pro"). Note users have to have font installed on personal machines.
  styling$tick$labels$font$color <- "#002654" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  styling$tick$labels$font$face <- "plain" #options are bold, italic, bold-italic, or NULL for regular
  styling$tick$labels$font$size <- 18
  
  #plotarea
  styling$plot$color <- "#F1EEDE" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  
  #plotmargins
  styling$margins$color <- "#F1EEDE" #Use hexadecimal strings (ex: "#1F8CD7") or recognized color word strings (ex: "Red")
  
  #grids
  styling$grid$lines$color <- "#AAAAAA"
  styling$grid$lines$size <- 2
  
  styling
}
