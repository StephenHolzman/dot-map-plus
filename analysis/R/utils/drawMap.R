drawMap <- function(mapData,countyoutlines,pointdata,pointx = "startLong",pointy = "startLat",colourvar,path,camera,title="",subtitle="",width=1920,height=1080,pointsize = .8,colpal = styling$colors$main, cite="Source: Census LODES Primary Jobs 2014",author="@UVAdemographics & @StephenHolz",kdalpha=0){
  
  #dotsInFrame <- filter(pointdata,
  #                      midLong > camera$view@bbox[1,1],
  #                      midLong < camera$view@bbox[1,2],
  #                      midLat > camera$view@bbox[2,1],
  #                      midLat < camera$view@bbox[2,2])
  
  dotsInFrame <- pointdata[pointdata[[pointx]] > camera$view@bbox[1,1] & pointdata[[pointx]] < camera$view@bbox[1,2] & pointdata[[pointy]] > camera$view@bbox[2,1] & pointdata[[pointy]] < camera$view@bbox[2,2], ]
  
  
  gg <- ggplot()
  gg <- gg + geom_polygon(data = mapData, aes_string(x = "long", y = "lat", group = "group"), fill= "white",color = "black", size = .1) + coord_equal()
  gg <- gg + geom_path(data = countyoutlines, aes_string(x = "long", y = "lat", group = "group"),color = "black", size = 2)
  gg <- gg + geom_point(data=dotsInFrame, aes_string(x=pointx,y=pointy,colour=colourvar), size=pointsize, alpha=0.6) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    scale_colour_manual(values = colpal) +
    labs(x = "Longitude", y = "Latitude") + 
    coord_map(xlim = camera$view@bbox[1,], ylim = camera$view@bbox[2,]) +
    theme(legend.position="none",
          plot.margin=unit(c(0,0,50,50),"points"),
          #panel.margin = unit(c(0,0,0,0),"points"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),          
          axis.ticks = element_blank(),
          axis.line = element_line(colour=styling$axis$lines$color,size=2),
          axis.text = element_blank(),
          axis.title = element_blank())
          #axis.text = element_text(family = "Myriad Pro Condensed",size = 16, colour = styling$axis$labels$font$color),
          #axis.title = element_text(family = "Myriad Pro Condensed",size = 24, colour = styling$axis$labels$font$color))
          #ylab("Latitude"))
  
  theme0 <- function(...) theme( legend.position = "none",
                                 panel.background = element_blank(),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 #panel.margin = unit(0,"null"),
                                 axis.ticks = element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_blank(),
                                 axis.ticks.length = unit(0,"null"),
                                 #axis.ticks.margin = unit(0,"null"),
                                 panel.border=element_rect(color=NA),...)
  
  
  p2 <- ggplot(dotsInFrame,aes_string(x=pointx,colour=colourvar,fill=colourvar)) + 
    geom_density(alpha=kdalpha,size=1.5) + 
    scale_x_continuous(breaks=NULL,expand=c(0,0),limits = camera$view@bbox[1,]) +
    scale_y_continuous(breaks=NULL,expand=c(0.02,0)) +
    scale_colour_manual(values = colpal) +
    scale_fill_manual(values = colpal) +
    theme_bw() +
    theme0(plot.margin = unit(c(0,0,0,50),"points")) 
  
  p3 <- ggplot(dotsInFrame,aes_string(x=pointy,colour=colourvar,fill=colourvar)) + 
    geom_density(alpha=kdalpha,size=1.5) + 
    coord_flip()  + 
    scale_x_continuous(labels = NULL,breaks=NULL,expand=c(0,0),limits = camera$view@bbox[2,]) +
    scale_y_continuous(labels = NULL,breaks=NULL,expand=c(.02,0)) +
    scale_colour_manual(values = colpal) +
    scale_fill_manual(values = colpal) +
    theme_bw() +
    theme0(plot.margin = unit(c(0,0,50,0),"points"))
  
  group_counts <- dotsInFrame %>%
    group_by_(colourvar) %>%
    summarise(Total = n()) %>%
    complete_(colourvar, fill = list(Total = 0)) %>%
    mutate(percent = Total / sum(Total),
           lab = paste0(round(100*Total / sum(Total),digits = 1),"%"),
           #label = paste0(round(100*totals / sum(totals),digits = 1),"%"),
           dummy = "")
  

  
  p4 <- ggplot(filter(group_counts,Total > 0), aes_string(x = "dummy",y = "percent",label = "lab", fill = colourvar)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colpal) + 
    geom_text(family = "Myriad Pro Condensed",position = position_stack(vjust = 0.5),colour = "white",size=9) +
    theme(legend.position = "none",
          panel.background = element_blank(), #element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.length = unit(0,"null"),
         # axis.ticks.margin = unit(0,"null"),
          plot.margin = unit(c(0,10,30,10),"points"))
  
  table_text <- select(group_counts, -percent,-lab,-dummy) %>%
    format(digits = 0, scientific=F,big.mark = ",")
  
  names(table_text)[2] <- paste0("Total (",format(sum(group_counts$Total),digits = 0, scientific=F,big.mark = ","),")")
  
  colvarlevellength <- length(levels(dotsInFrame[[colourvar]]))
  
  p5 <- tableGrob(table_text,rows = NULL,theme=ttheme_minimal(
    core=list(
      fg_params=list(fontfamily="Myriad Pro",fontsize=22,col="white",hjust = c(rep(0,colvarlevellength),rep(1,colvarlevellength)), x=c(rep(.05,colvarlevellength),rep(.95,colvarlevellength))),
      bg_params = list(fill=rep(colpal[1:nrow(table_text)],2))
    ),
    colhead=list(fg_params=list(col=colpal[2], fontsize=24, fontfamily="Myriad Pro Condensed"))
  ))
  #padding <- unit(15,'mm')
  #var_title <- textGrob("Viewing",gp=gpar(fontsize=50,col=colpal[2],fontfamily="Myriad Pro Condensed"))
  
  #p5 <- gtable_add_rows(
  #  p5,
  #  heights = grobHeight(var_title) + padding,
  #  pos = 0
  #)
  
  #p5 <- gtable_add_grob(
  #  p5,
  #  var_title,
  #  1,1,1, ncol(p5)
  #)
  
  #gg <- gg + facet_grid(. ~ age)
  png(path,width=1920,height=1080)
  grid.arrange(arrangeGrob(p2,p5,ncol=2,widths=c(1555,365)),
               arrangeGrob(gg,p3,p4,ncol=3,widths=c(1555,260,105)),
               heights=c(260,820))
  
  grid.text(cite, x=unit(width-8,"points"),y=unit(15,"points"),just="right",gp=gpar(fontsize=styling$footer$cite$font$size*1.5,fontfamily=styling$footer$cite$font$family,fontface=styling$footer$cite$font$face, col=styling$header$title$font$color))
  grid.text(author, x=unit(width-8,"points"),y=unit(35,"points"),just="right",gp=gpar(fontsize=styling$footer$author$font$size*1.5,fontfamily=styling$footer$author$font$family,fontface=styling$footer$author$font$face, col=styling$header$title$font$color))
  grid.text(title, x=unit(50,"points"),y=unit((height-32)/height,"npc"),just="left",gp=gpar(fontsize=styling$header$title$font$size*1.5,fontfamily=styling$header$title$font$family,fontface=styling$header$title$font$face, col=styling$header$title$font$color))
  grid.text(subtitle, x=unit(50,"points"),y=unit((height-114)/height,"npc"),just="left",gp=gpar(fontsize=styling$header$subtitle$font$size*1.5,fontfamily=styling$header$subtitle$font$family,fontface=styling$header$subtitle$font$face, col=styling$header$subtitle$font$color))
  img <- rsvg(file.path("input","logos","WCCPPDRGlogoBlueText.svg"),height = 75)
  g <- rasterGrob(img,x=unit(235,"points"),y=unit(26/height,"npc"),height=unit(50,"points"))
  print(grid.draw(g), newpage = FALSE)
  
  dev.off()
  
}
