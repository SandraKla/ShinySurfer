oasis.tidy <- function(session,area,OASIS){
  cols <- ncol(OASIS)
  
  # the to be changed names
  oasis_r <- OASIS[(cols-147):cols]
  n <- names(oasis_r)
  nt <- sub("lh","",n)
  nt <- sub("rh","",nt)
  nt <- sub("thickness","",nt)
  nt <- gsub("_","",nt)
  nt <- gsub("-","",nt)
  
  
  o_table <- tibble(names(oasis_r),nt,seq(1:length(names(oasis_r))))
  names(o_table) <- c("o_names","pattern","name_seq")
  
  #the replacement names
  an <- area[[2]]
  an <- gsub("_","",an)
  an <- gsub("-","",an)
  an <- sub("and","",an)
  area[[2]] <- an
  names(area) <- c("num","pattern","replacement")
  pattern <- merge(o_table,area)
  
  pattern <- pattern[c("o_names","name_seq","replacement")]%>%arrange(name_seq)
  
  get_name <- function(x){
    if(grepl("lh",x["o_names"])){
      x["o_names"] <- paste("L",x["replacement"],sep = " ")
    }else if(grepl("rh",x["o_names"])){
      x["o_names"] <- paste("R",x["replacement"],sep = " ")
    }
  }
  
  pattern <- apply(pattern,1,get_name)
  
  names(OASIS)[(cols-147):cols] <- pattern
  
  oasis_data <- OASIS
  # updateSelectInput(session,"qc_fil",choices = c("sex","age",pattern))
  # updateSelectInput(session,"fil",choices = c("ID","sex","age",pattern))
  # updateSelectInput(session,"fil_com",choices = c("sex","age",pattern))
  
  return(oasis_data)
}