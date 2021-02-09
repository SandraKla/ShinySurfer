#' 
#' 
#' @param data
#' @param col
get.other.qc.plot <- function(data,col){
  ps <- list()
  
  for (c in col) {
    cdata <- data[,c]
    data.melt <- melt(cdata)
    names(data.melt) <- c("area","thickness")
    
    h <- randomColor()
    print(h)
    p <- ggplot(data.melt,aes(x=area,y=thickness))+
      geom_flat_violin(position=position_nudge(x=0.2,y=0),adjust=1,trim = TRUE,fill=h)+
      geom_point(position = position_jitter(width=.1),size=.2,aes(color=area),show.legend = FALSE,color=h)+
      geom_boxplot(aes(x=as.numeric(area)+0.2,y=thickness),outlier.shape = NA,alpha=0.3,width=0.1,color="BLACK")+
      coord_flip()+
      theme_cowplot()+
      facet_wrap(~area,ncol = 2,scales = "free")+
      guides(fill=FALSE)
    
    ps <- c(ps,list(p))
  }
  
  plots <- plot_grid(plotlist = ps,ncol = 2)
  return(plots)
}