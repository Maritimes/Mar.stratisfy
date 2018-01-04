getAlkTable<-function(alkTable=NULL){
  custAlkTable<-NA
  
  fn<-basename(alkTable)
  fn_dets = strsplit(fn, '[.]')[[1]]
  
  if (nchar(fn_dets[2]) %in% c(3,4)){
    #kinda looks like a file
    if (file.exists(alkTable)){
      if (fn_dets[2]=="csv")custAlkTable = read.csv(fn)
      if (fn_dets[2]=="xlsx")custAlkTable = openxlsx::read.xlsx(fn)
      if (fn_dets[2]=="txt")custAlkTable = read.delim(fn)
    }else{
      #maybe a bad path?
      print("check Path")
    }
    
  }else{
    #not a valid file - maybe an oracle object?
    sql <- paste0("select * from ",alkTable)
    custAlkTable<-oracle_cxn$thecmd(oracle_cxn$channel, sql)
    if (class(custAlkTable)=='character')return(NA)
    if (nrow(custAlkTable)<1)return(NA)
  }

  return(custAlkTable)
}