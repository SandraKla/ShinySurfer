source("geom_flat_violin.R")

### get the facet name for four areas
facet.area1 <- function(x){
  return('ICV')
}

facet.area2 <- function(x){
  if(grepl("*left*",x)&&grepl("*perc*",x)){
    return("perc_left")
  }else if(grepl("*right*",x)&&grepl("*perc*",x)){
    return("perc_right")
  }else if(grepl("*left*",x)){
    return("left")
  }else if(grepl("*right*",x)){
    return("right")
  }else if(grepl("total",x)&&grepl("*perc*",x)){
    return("perc_total")
  }else if(grepl("total",x)){
    return("total")
  }else{
    return("asymmetry")
  }
}





facet.area3 <- function(x){
  if(grepl("*left*",x)&&grepl("*norm*",x)){
    return("norm_left")
  }else if(grepl("*right*",x)&&grepl("*norm*",x)){
    return("norm_right")
  }else if(grepl("*left*",x)){
    return("mean_left")
  }else if(grepl("*right*",x)){
    return("mean_right")
  }else if(grepl("norm",x)){
    return("norm")
  }else if(grepl("mean",x)){
    return("mean")
  }else if(grepl("asymmetry",x)){
    return("asymmetry")
  }else{
    return("mean")
  }
}

facet.area4 <- function(x){
  if(grepl("perc",x)&&grepl("left",x)){
    return("grey_perc_left")
  }else if (grepl("perc",x)&&grepl("right",x)){
    return("grey_perc_right")
  }else if(grepl("perc",x)){
    return("grey_perc")
  }else if(grepl("left",x)){
    return("grey_left")
  }else if(grepl("right",x)){
    return("grey_right")
  }else if(grepl("asymmetry",x)){
    return("grey_asymmetry")
  }else if (grepl("grey",x)){
    return("grey")
  }
}

## fix the facet name
get_area_name <- function(x){
  x <- gsub("_","",x)
  x <- sub("right","",x)
  x <- sub("left","",x)
  x <- sub("total","",x)
  x <- sub("asymmetry","",x)
  x <- sub("perc","",x)
  x <- sub("cm3","",x)
  x <- gsub("\\.","",x)
  x <- sub("norm","",x)
  x <- sub("mean","",x)
  x <- sub("thickness","",x)
  x <- sub("cortical","",x)
  x <- sub("grey","",x)
  x <- sub("matter","",x)
}



get_area_factor <- function(x){
  x <- as.character(x)
  name_level <- rev(unique(x))
  factor(x,levels = name_level,ordered = TRUE)
}


get_lr_factor <- function(x){
  x <- as.character(x)
  name_level <- unique(x)
  factor(x,levels = name_level,ordered = TRUE)
}




get.plot <- function(data,area.label=NULL,add.xlabel=FALSE,fill.col="red"){
  
  f.facet <- switch (area.label,
                     "area1" = "facet.area1",           
                     "area2" = "facet.area2",
                     "area3" = "facet.area3",
                     "area4" = "facet.area4"
  )
  
  
  
  
  # name_level.area1 <- names(data)
  data.melt <- melt(data)
  names(data.melt) <- c("area","thickness")
  data.melt$lr <- lapply(data.melt$area,f.facet)
  data.melt$area <- lapply(data.melt$area, get_area_name)
  data.melt$area <- get_area_factor(data.melt$area)
  data.melt$lr <- get_lr_factor(data.melt$lr)
  
  p <- ggplot(data.melt,aes(x=area,y=thickness))+
    geom_flat_violin(position=position_nudge(x=0.2,y=0),adjust=1,trim = TRUE,fill=fill.col)+
    geom_point(position = position_jitter(width=.1),size=.2,aes(color=area),show.legend = FALSE,color=fill.col)+
    geom_boxplot(aes(x=as.numeric(area)+0.2,y=thickness),outlier.shape = NA,alpha=0.3,width=0.1,color="BLACK")+
    coord_flip()+
    theme_cowplot()+
    ylab(data.melt$lr)+
    theme(axis.text.y=element_blank())+
    guides(fill=FALSE)
  
  if(add.xlabel){
    p <- p + xlab(data.melt$area)
  }else{
    p <- p+xlab("")
  }
  
  return(p)
}

get.plots <- function(data,area.label=NULL){
  gg.title <- switch (area.label,
                      "area1" = "ICV",           
                      "area2" = "thickness",
                      "area3" = "cortical_thickness",
                      "area4" = "grey_matter"
  )
  ps <- list()
  if(area.label!="area1"){
    for (i in seq(from=0,to=12)) {
      j=i*7
      h <- randomColor()
      for (x in seq(from=j+1,to=j+7)) {
        p <- get.plot(data[,x],area.label = area.label,x==j+1,fill.col = h)
        ps <- c(ps,list(p))
      }
    }
  }else{
    p <- get.plot(data,"area1")
    ps <- c(ps,list(p))
  }
  
  title <- ggdraw()+draw_label(gg.title,size = 20)
  
  plots <- plot_grid(plotlist = ps,ncol=7)
  if(area.label=="area1"){
    plots <- plot_grid(title,plots,ncol=1,rel_heights = c(0.3,1))
  }else{
    plots <- plot_grid(title,plots,ncol=1,rel_heights = c(0.3,13))
  }
  return(plots)
}




get.ceres.qc.plot <- function(data){
  area1 <- data[,1]
  area2 <- data[,2:92]
  area3 <- data[,93:183]
  area4 <- data[,184:274]
  
  area1.plot <- get.plots(area1,"area1")
  area2.plot <- get.plots(area2,"area2")
  area3.plot <- get.plots(area3,"area3")
  area4.plot <- get.plots(area4,"area4")

  creres.plot <- plot_grid(area1.plot,area2.plot,area3.plot,area4.plot,ncol=1,rel_heights = c(1.3,13.3,13.3,13.3))
  
  return(creres.plot)
}