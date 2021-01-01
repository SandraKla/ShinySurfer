multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
} # Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/

add_lm_trace_freesurfer <- function(data,var_explan="reaction_time"){
  cols <- ncol(data)
  explan <- var_explan
  p <- list()
  for(i in seq(1:74)){
    data_thickness <- dplyr::select(data,(cols-148+i),(cols-74+i))
    data_thickness <- bind_cols(explan=data[[explan]],data_thickness)
    data_thickness <- melt(data_thickness,"explan")
    names(data_thickness) <- c(as.character(explan),"thickness","value")
    
    data_thickness$lr <- substr(data_thickness$thickness,1,1)
    data_thickness[which(data_thickness$lr=='l'|data_thickness$lr=="L"),]$lr <- "left"
    data_thickness[which(data_thickness$lr=='r'|data_thickness$lr=="R"),]$lr <- "right"
    data_thickness$lr <- as.factor(data_thickness$lr)
    data_thickness$thickness <- substring(data_thickness$thickness,4)
    data_thickness$thickness <- as.factor(data_thickness$thickness)
    p1 <- ggplot(data_thickness, aes_string(x=explan, y="value")) +
      geom_point() +
      facet_grid(thickness~lr,scales = "fixed")+
      stat_smooth(method=lm, level=0.95) +
      xlab(explan)+
      ylab("thickness (mm)")+
      theme_minimal()
    p <- c(p,list(p1))
  }
  
  return(multiplot(plotlist = p,cols = 1))
}
