render.ui.output <- function(col.name,col,seletedValue=NULL,ui_type=NULL){
  toRenderUi <- if(is.na(as.numeric(col[[1]]))){
    selectInput(paste0(col.name,ui_type),paste(col.name),
                choices = c("All",unique(col)),
                selected = {
                  if (is.null(seletedValue)||"All"==seletedValue){
                    "All"
                  } else{
                    seletedValue
                  }
                })
  }else{
    col=as.numeric(col)
    col=na.omit(col)
    sliderInput(paste0(col.name,ui_type),paste(col.name),
                
                min = min(col),max=max(col),
                value = {
                  if(!is.null(seletedValue)){
                    seletedValue
                  }else{
                    c(min(col),max(col))
                  }
                })
  }
  
  return(toRenderUi)
}