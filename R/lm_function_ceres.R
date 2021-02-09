#' 
#' 
#' @param data
#' @param target.col
#' @param area.label
get.lm.plots <- function(data,target.col,area.label=NULL){
  # data <- area1
  gg.title <- switch (area.label,
                      "area1" = "ICV",           
                      "area2" = "thickness",
                      "area3" = "cortical_thickness",
                      "area4" = "grey_matter"
  )
  area.colnames <- names(data)
  
  plot.grid.list <-list()
  if(area.label!="area1"){
    for (i in seq(from=0,to=12)) {
      j=i*7
      ps <- list()
      plost_i <- NULL
      # h <- randomColor()
      for (x in seq(from=j+1,to=j+7)) {
        p <- get.lm.plot(data,target.col,area.colnames[[x+1]],area.label)
        ps <- c(ps,list(p))
        plots_i <- plot_grid(plotlist = ps,ncol=3)
      }
      plot.grid.list <- c(plot.grid.list,list(plots_i))
    }
  }else{
    p <- get.lm.plot(data,target.col,area.colnames[[2]],"area1")
    p <- plot_grid(p,ncol=3)
    plot.grid.list <- c(plot.grid.list,list(p))
  }
  
  plots <- plot_grid(plotlist= plot.grid.list,ncol = 1,labels = unique(get_area_name(names(data)))[-1])
  
  title <- ggdraw()+draw_label(gg.title,size = 20)
  
  if(area.label=="area1"){
    plots <- plot_grid(title,plots,ncol=1,rel_heights = c(0.3,1))
  }else{
    plots <- plot_grid(title,plots,ncol=1,rel_heights = c(0.3,39))
  }
  return(plots)
}

#' 
#' 
#' @param data
#' @param target.col
get.ceres.lm.plots <- function(data,target.col){
  data <- tibble(data,.name_repair = "universal")
  cols <- length(data)
  data <- select(data,target.col,(cols-273):cols)
  
  area1 <- select(data,target.col,2)
  area2 <- select(data,target.col,3:93)
  area3 <- select(data,target.col,94:184)
  area4 <- select(data,target.col,185:275)
  
  area1.plot <- get.lm.plots(area1,target.col,"area1")
  area2.plot <- get.lm.plots(area2,target.col,"area2")
  area3.plot <- get.lm.plots(area3,target.col,"area3")
  area4.plot <- get.lm.plots(area4,target.col,"area4")
  
  ceres.lm.plot <- plot_grid(area1.plot,area2.plot,area3.plot,area4.plot,ncol=1,rel_heights = c(1.3,39.3,39.3,39.3))
  # ceres.lm.plot <- plot_grid(area1.plot,area2.plot,ncol=1,rel_heights = c(1.3,39.3))
  return(ceres.lm.plot)
}