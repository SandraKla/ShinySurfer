#' 
#' 
#' @param data
get.regression.col <- function(dat){
  not.regression.col <- NULL
  col.names <- names(dat)
  d.one <- dat[1,]

  col.regressable <- grep("^\\d+\\.?\\d*",d.one)
  dat <- select(dat,col.regressable)
  for (i in seq(1:length(dat))) {
      if(nrow(unique(dat[i]))<(nrow(dat)/50)){
        not.regression.col=c(not.regression.col,i)
        print(not.regression.col)
      }
  }
  dat <- select(dat,-not.regression.col)
  dat[dat != "ID"];
  return(names(dat))
}