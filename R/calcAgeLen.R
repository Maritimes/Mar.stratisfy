#' @title calcAgeLen
#' @description This function calculates the ages and lengths.
#' @param requested  The default value is \code{NULL}. 
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom Mar.utils SQL_in
#' @export
calcAgeLen<-function(requested = NULL){
  eg<-function(){
    #remove records without weight or totalno
    #tmp <- .GlobalEnv$str_dfNWSets[.GlobalEnv$str_dfNWSets$TOTNO*.GlobalEnv$str_dfNWSets$TOTWGT!=0,]
    tmp <- .GlobalEnv$str_dfNWSets
    tmp <- merge(tmp, .GlobalEnv$str_dfRawDet, by=c("MISSION", "SETNO","SIZE_CLASS"), all.x=T) 
    tmp <- merge(tmp, .GlobalEnv$str_dfRawInf[,c("MISSION", "SETNO", "STRAT","DIST","DMIN","DMAX","DEPTH","TIME")], by=c("MISSION", "SETNO", "STRAT"), all.x=T) 
    #tmp <- merge(tmp, .GlobalEnv$str_dfStrata, all.x=T) 
    

    #tmp[which(is.na(tmp$BINWIDTH)), c('BINWIDTH')]<-as.numeric(.GlobalEnv$str_dfSpp$LGRP)
    if (nrow(tmp[is.na(tmp$BINWIDTH),])>0) tmp[is.na(tmp$BINWIDTH),]$BINWIDTH<-as.numeric(.GlobalEnv$str_dfSpp$LGRP)
    #tmp[which(is.na(tmp$FLEN)), c('FLEN')] <- 0
    if (nrow( tmp[is.na(tmp$FLEN),])>0) tmp[is.na(tmp$FLEN),]$FLEN <-0
    tmp$FLEN<-floor(tmp$FLEN/tmp$BINWIDTH)*tmp$BINWIDTH #changed FLEN
    tmp$CAGE<-NA
    
     if (.GlobalEnv$str_agency=="DFO"){
      #if sampwgt is 0 or NA and totwgt is not null or 0 
      #then replace sample weigt with total weight 
      if ( nrow(tmp[(is.na(tmp$SAMPWGT)|tmp$SAMPWGT==0) & (!is.na(tmp$TOTWGT) & tmp$TOTWGT !=0),])) {
        tmp[(is.na(tmp$SAMPWGT)|tmp$SAMPWGT==0) & (!is.na(tmp$TOTWGT) & tmp$TOTWGT !=0),]$SAMPWGT <-tmp[(is.na(tmp$SAMPWGT)|tmp$SAMPWGT==0) & (!is.na(tmp$TOTWGT) & tmp$TOTWGT !=0),]$TOTWGT
      }
     

       if (nrow(tmp[tmp$SAMPWGT!=0 | !is.na(tmp$SAMPWGT),])>0){
         tmp[tmp$SAMPWGT!=0 | !is.na(tmp$SAMPWGT),]$CAGE <- 
           tmp[tmp$SAMPWGT!=0 | !is.na(tmp$SAMPWGT),]$RAW_TOTWGT/
           tmp[tmp$SAMPWGT!=0 | !is.na(tmp$SAMPWGT),]$SAMPWGT*
                (.GlobalEnv$str_towDist/tmp[tmp$SAMPWGT!=0 | !is.na(tmp$SAMPWGT),]$DIST)*tmp[tmp$SAMPWGT!=0 | !is.na(tmp$SAMPWGT),]$CLEN
       }
       #if sampwgt ==0, cage ==0
       if (nrow(tmp[!is.na(tmp$SAMPWGT) & tmp$SAMPWGT==0,])>0) tmp[!is.na(tmp$SAMPWGT) & tmp$SAMPWGT==0,]$CAGE <-0
      #if sampwgt is na/null, cage is na/null
      if(nrow(tmp[is.na(tmp$SAMPWGT),])>0)tmp[is.na(tmp$SAMPWGT),]$CAGE<- NA
   }else if (.GlobalEnv$str_agency=="NMFS"){
     tmp$CAGE<-tmp$CLEN
   }
    
    tmp[is.na(tmp$DEPTH),'DEPTH']<- -99
    tmp[is.na(tmp$DMIN),'DMIN']<- -99
    tmp[is.na(tmp$DMAX),'DMAX']<- -99
    tmp[is.na(tmp$DEPTH),'DEPTH']<- -99
    tmp$FLEN<-tmp$FLEN+(tmp$BINWIDTH*.5)-.5
    tmp$LOCTIME<-tmp$TIME
    tmp$TIME<-NULL
    lgrpMissing <- as.character(sort(setdiff(seq(max(sort(unique(tmp$FLEN))), by = -str_dfSpp$LGRP), unique(tmp$FLEN))) )
    
   if (.GlobalEnv$str_sexed) {
    allfields <-c("STRAT","MISSION","SETNO", "FSEX", "FLEN","CAGE") #"SLAT","SLONG","UNIT_AREA",
   }else{
    allfields <-c("STRAT","MISSION","SETNO","FLEN","CAGE")
   }
    lset <- aggregate(tmp[,allfields]$CAGE,
                      tmp[,allfields][allfields[allfields !="CAGE"]],
                      FUN=sum)
    
    lset <- lset[order(lset$STRAT,lset$MISSION,lset$SETNO),]
    
    

    fakeRows = lset[1,]
    nums <- sapply(fakeRows, is.numeric)
    fakeRows[,nums]<- -1
    fakeRows[,!nums] <- "FAKE"
    fakeRows <- fakeRows[rep(rownames(fakeRows), length(lgrpMissing)), ]
    fakeRows$FLEN <- lgrpMissing

    if (.GlobalEnv$str_sexed) {
      fakeRows <- fakeRows[rep(rownames(fakeRows), 3), ] 
      fakeRows <- fakeRows[order(fakeRows$FLEN),]
      fakeRows$FSEX <- c(0,1,2)
      
      length_by_set <- reshape2::dcast(rbind(lset,fakeRows), STRAT + MISSION + SETNO ~ FSEX +FLEN  )
    }else{
      length_by_set <- reshape2::dcast(rbind(lset,fakeRows), STRAT + MISSION + SETNO ~ FLEN  )
    }
 length_by_set = length_by_set[!(length_by_set$STRAT=="FAKE"),]
 #Reorder the columns so that they go by sex, then every possible length
 imp <- c("STRAT","MISSION","SETNO")
 length_by_set_id = length_by_set[imp]
 length_by_set_dat = length_by_set[!colnames(length_by_set) %in% imp]
 if (.GlobalEnv$str_sexed) {
   length_by_set_dat = length_by_set_dat[order(as.numeric(substr(colnames(length_by_set_dat),1,1)),as.numeric(substr(colnames(length_by_set_dat),3,5)))]
 }else{
   length_by_set_dat = length_by_set_dat[order(as.numeric(colnames(length_by_set_dat)))]
 }
 
 length_by_set_dat <- Mar.utils::na_zero(length_by_set_dat)
 
  length_by_set = cbind(length_by_set_id,length_by_set_dat)

#   length_by_set$TOTAL<-rowSums(length_by_set[!colnames(length_by_set) %in% imp])

    length_by_strat_cnt<-aggregate(list(
      COUNT=length_by_set$STRAT), 
      by=list(STRAT=length_by_set$STRAT), 
      FUN=length)

    length_by_strat_mean<-Mar.utils::na_zero(setNames(aggregate(list(
      MEAN = length_by_set[!colnames(length_by_set) %in% imp]), 
      by=list(STRAT=length_by_set$STRAT), 
      FUN=mean), c("STRAT",colnames(length_by_set_dat))))
    
    length_by_strat_se<-Mar.utils::na_zero(setNames(aggregate(list(
      MEAN_SE=length_by_set[!colnames(length_by_set) %in% imp]), 
      by=list(STRAT=length_by_set$STRAT), 
      FUN=Mar.utils::st_err), c("STRAT",colnames(length_by_set_dat))))
    
    length_total =  merge(length_by_strat_mean, str_dfStrata[,c("STRAT","TUNITS")])
    length_total = cbind(length_total[1],length_total[2:(ncol(length_total)-1)]*length_total$TUNITS)    
    
    length_total_se =  merge(length_by_strat_se, str_dfStrata[,c("STRAT","TUNITS")])
    length_total_se = cbind(length_total_se[1],length_total_se[2:(ncol(length_total_se)-1)]*length_total_se$TUNITS)
    results=list(length_by_set= length_by_set,
                 #length_by_strat_cnt,
                 length_by_strat_mean = length_by_strat_mean,
                 length_by_strat_se = length_by_strat_se,
                 length_total = length_total,
                 length_total_se = length_total_se)
    return(results)
  }


  
  switch(requested, 
         "doEg" = eg()
  )
}