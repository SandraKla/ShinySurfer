get.choice <- function(col,data.table,seletedValue=NULL,col.type=NULL){
  is.select=is.na(as.numeric(data.table[[col]]))
  if(is.select==TRUE){
    if((!is.null(seletedValue))&&seletedValue!="All"){
      data.table <- data.table[data.table[[col]]==seletedValue,]
    }
  }else if(!is.null(seletedValue)){
    v_min <- min(seletedValue)
    v_max <- max(seletedValue)
    data.table <- data.table[data.table[[col]]<=as.numeric(v_max),]
    data.table <- data.table[data.table[[col]]>=as.numeric(v_min),]}
  return(data.table)
  
}