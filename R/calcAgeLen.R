#' @title calcAgeLen
#' @description This function calculates the ages and lengths.
#' @param requested  The default value is \code{NULL}. This determines which 
#' analytic will be performed.
#' @param agency  The default value is \code{NULL}. 
#' @param dfNWSets  The default value is \code{NULL}.
#' @param dfRawDet  The default value is \code{NULL}.
#' @param dfRawInf  The default value is \code{NULL}.
#' @param dfStrata  The default value is \code{NULL}.
#' @param dfSpp  The default value is \code{NULL}.
#' @param towDist  The default value is \code{NULL}.
#' @param sexed  The default value is \code{NULL}.
#' @param agelen  The default value is \code{NULL}.
#' @param lengthsTotals  The default value is \code{NULL}.
#' @param lset  The default value is \code{NULL}.
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom Mar.utils SQL_in
#' @importFrom stats setNames
#' @importFrom stats reshape
#' @importFrom reshape2 dcast
#' @export
calcAgeLen<-function(requested = NULL, agency = NULL, dfNWSets = NULL, 
                     dfRawDet=NULL, dfRawInf = NULL, dfStrata = NULL, 
                     dfSpp=NULL, towDist=NULL,  sexed = NULL,
                     agelen = NULL, lengthsTotals = NULL, lset=NULL){
  calcLengths<-function( agency, dfNWSets,dfRawDet, dfRawInf, dfStrata, dfSpp, 
                         towDist, sexed){
    sppLgrp = dfSpp$LGRP
    #remove records without weight or totalno
    agelen <- dfNWSets
    agelen <- merge(agelen, dfRawDet, by=c("MISSION", "SETNO","SIZE_CLASS"), all.x=T) 
    agelen <- merge(agelen, dfRawInf[,c("MISSION", "SETNO", "STRAT",
                                  "DIST","DMIN","DMAX","DEPTH","TIME")], 
                 by=c("MISSION", "SETNO", "STRAT"), all.x=T) 
    if (nrow(agelen[is.na(agelen$BINWIDTH),])>0) 
      agelen[is.na(agelen$BINWIDTH),]$BINWIDTH <- as.numeric(sppLgrp)
    if (nrow( agelen[is.na(agelen$FLEN),])>0) agelen[is.na(agelen$FLEN),]$FLEN <- 0
    agelen$FLEN<-floor(agelen$FLEN/agelen$BINWIDTH)*agelen$BINWIDTH 
    agelen$CAGE<-NA
    if (agency=="DFO"){
      #if sampwgt is 0 or NA and totwgt is not null or 0 
      #then replace sample weigt with total weight 
      if ( nrow(agelen[(is.na(agelen$SAMPWGT)|agelen$SAMPWGT==0) & 
                    (!is.na(agelen$TOTWGT) & agelen$TOTWGT !=0),])>0) 
      {
        agelen[(is.na(agelen$SAMPWGT)|agelen$SAMPWGT==0) & 
              (!is.na(agelen$TOTWGT) & agelen$TOTWGT !=0),]$SAMPWGT <- 
          agelen[(is.na(agelen$SAMPWGT)|agelen$SAMPWGT==0) & 
                (!is.na(agelen$TOTWGT) & agelen$TOTWGT !=0),]$TOTWGT
      }
      if (nrow(agelen[agelen$SAMPWGT!=0 | !is.na(agelen$SAMPWGT),])>0){
        agelen[agelen$SAMPWGT!=0 | !is.na(agelen$SAMPWGT),]$CAGE <- 
          agelen[agelen$SAMPWGT!=0 | !is.na(agelen$SAMPWGT),]$RAW_TOTWGT/
          agelen[agelen$SAMPWGT!=0 | !is.na(agelen$SAMPWGT),]$SAMPWGT*
          (towDist/agelen[agelen$SAMPWGT!=0 | !is.na(agelen$SAMPWGT),]$DIST)*
          agelen[agelen$SAMPWGT!=0 | !is.na(agelen$SAMPWGT),]$CLEN
      }
      #if sampwgt ==0, cage ==0
      if (nrow(agelen[!is.na(agelen$SAMPWGT) & agelen$SAMPWGT==0,])>0) 
        agelen[!is.na(agelen$SAMPWGT) & agelen$SAMPWGT==0,]$CAGE <-0
      #if sampwgt is na/null, cage is na/null
      if(nrow(agelen[is.na(agelen$SAMPWGT),])>0)
        agelen[is.na(agelen$SAMPWGT),]$CAGE<- NA
      if(nrow(agelen[agelen$CAGE == 0 & agelen$TOTNO !=0,])>0) 
        agelen[agelen$CAGE == 0 & agelen$TOTNO !=0,]$CAGE<-
        agelen[agelen$CAGE == 0 & agelen$TOTNO !=0,]$TOTNO*agelen[agelen$CAGE == 0 & agelen$TOTNO !=0,]$CLEN/agelen[agelen$CAGE == 0 & agelen$TOTNO !=0,]$RAW_TOTNO

    }else if (agency=="NMFS"){
      agelen$CAGE<-agelen$CLEN
    }
    agelen$FLEN<-agelen$FLEN+(agelen$BINWIDTH*.5)-.5
    
    ##
    
    ##
    if (sexed) {
      allfields <-c("STRAT","MISSION","SETNO", "FSEX", "FLEN","CAGE") 
    }else{
      allfields <-c("STRAT","MISSION","SETNO","FLEN","CAGE")
    }
    #---
    lset=agelen[,allfields]
    
    # li = which(lset$CAGE==0) 
    # lset$FLEN[li] = unique(lset$FLEN)[1]

    #----
    lset <- aggregate(lset[,allfields]$CAGE,
                      lset[allfields !="CAGE"],
                      FUN=sum)
    #rm(agelen)
    #lset = lset[lset$x>0,]
    lset <- lset[order(lset$STRAT,lset$MISSION,lset$SETNO),]
    #the following fills out the lset data with all possible length groups 
    #for all sexes
    rng = range(lset$FLEN, na.rm = T)
    allLength = seq(min(rng),max(rng), by=sppLgrp)

    if (sexed){
      allSex = unique(lset$FSEX)
      fakeRows = expand.grid(FSEX = allSex, FLEN = allLength)
      fakeRows=fakeRows[,c("FSEX","FLEN")]
    }else{
      fakeRows = expand.grid(FLEN = allLength)
    }
    fakeRows$STRAT <- "FAKE"
    fakeRows$MISSION <- "FAKE"
    fakeRows$SETNO <- -1
    fakeRows$x <- -1
    lset=rbind(lset,fakeRows)
    
    if (sexed) {
      length_by_set <- dcast(lset, STRAT + MISSION + SETNO ~ 
                                         FSEX +FLEN, value.var = "x"   )
    }else{
      length_by_set <- dcast(lset, STRAT + MISSION + SETNO ~ 
                                         FLEN, value.var = "x"  )
    }
    #remove the evidence of the fakeRows
    length_by_set = length_by_set[length_by_set$STRAT!="FAKE",]
    
    #ensure all strata and sets still present so their zeroes get included
    length_by_set=merge(dfNWSets[,c("MISSION", "STRAT", "SETNO")],length_by_set, 
                          all.x=T)
    length_by_set=length_by_set[order(length_by_set$STRAT,
                                      length_by_set$MISSION,
                                      length_by_set$SETNO),]
    
    #Reorder the columns so that they go by sex, then every possible length
    imp <- c("STRAT","MISSION","SETNO")
    length_by_set_id = length_by_set[imp]
    length_by_set_dat = length_by_set[!colnames(length_by_set) %in% imp]
    if (sexed) {
      length_by_set_dat = length_by_set_dat[order(
                      as.numeric(substr(colnames(length_by_set_dat),1,1)),
                      as.numeric(substr(colnames(length_by_set_dat),3,5)))]
    }else{
      length_by_set_dat = length_by_set_dat[order(
                      as.numeric(colnames(length_by_set_dat)))]
    }
    
    length_by_set_dat[is.na(length_by_set_dat)]<-0
    length_by_set_dat = cbind(length_by_set_dat, TOTAL=rowSums(length_by_set_dat))
    length_by_set = cbind(length_by_set_id,length_by_set_dat)
    
    length_by_strat_mean<-setNames(aggregate(list(
      MEAN = length_by_set[!colnames(length_by_set) %in% imp]), 
      by=list(STRAT=length_by_set$STRAT), 
      FUN=mean), c("STRAT",colnames(length_by_set_dat)))
    length_by_strat_mean[is.na(length_by_strat_mean)]<-0
    
    length_by_strat_se<-setNames(aggregate(list(
      MEAN_SE=length_by_set[!colnames(length_by_set) %in% imp]), 
      by=list(STRAT=length_by_set$STRAT), 
      FUN=Mar.utils::st_err), c("STRAT",colnames(length_by_set_dat)))
    length_by_strat_se[is.na(length_by_strat_se)]<-0
    
    length_total =  merge(length_by_strat_mean, dfStrata[,c("STRAT","TUNITS")])
    length_total = cbind(length_total[1],
                         length_total[2:(ncol(length_total)-1)]*
                            length_total$TUNITS)    
    
    length_total_se =  merge(length_by_strat_se, dfStrata[,c("STRAT","TUNITS")])
    length_total_se = cbind(length_total_se[1],
                            length_total_se[2:(ncol(length_total_se)-1)]*
                              length_total_se$TUNITS)
    results=list(agelen = agelen,
                 length_by_set= length_by_set,
                 length_by_strat_mean = length_by_strat_mean,
                 length_by_strat_se = length_by_strat_se,
                 length_total = length_total,
                 length_total_se = length_total_se,
                 lset = lset)
    return(results)
  }
  calcAgeKey<-function(agelen, dfSpp, lengthsTotals, lset){

    sppLgrp = dfSpp$LGRP
    
    # Age Length Key -----------------------------------------------------------
    alk<-agelen[,c("AGE","FLEN","CAGE","SETNO")]
    alk<-alk[!is.na(alk$AGE), ]
    if (nrow(alk)<1){     #only try age calculations if we have ages
      res = list(alk="No ages in data", alw="No ages in data")
    }else{
      all.ages = seq(min(alk$AGE),max(alk$AGE)) 
      rng = range(alk$FLEN, na.rm = T)
      allLength = seq(min(rng),max(rng), by=sppLgrp)
      
      al = expand.grid(all.ages,allLength)
      names(al) = c('AGE','FLEN')
      al$SETNO = al$CAGE = 0
      alk = rbind(alk,al)
      alk$SETNO = ifelse(alk$SETNO>0,1,NA)
      
      alk<-
        t(
          tapply(
            alk$SETNO,
            list(alk$AGE, alk$FLEN),
            function(x) length(x[!is.na(x)])
          )
        )
      colnames(alk) <- paste("AGE", colnames(alk), sep = "_")
      alk<-cbind(alk,"TOTAL"=rowSums(alk)) 
      alk<-rbind(alk, "TOTAL"=colSums(alk))
      # Age Length Weight ------------------------------------------------------
      alw<-aggregate(FWT~AGE+FLEN,data=agelen,FUN=mean)
      alw<-alw[order(alw$AGE,alw$FLEN),]
      alw$FWT = alw$FWT / 1000
      alw = reshape(
        alw,idvar='FLEN',timevar='AGE',direction='wide')
      names(alw) <- gsub("FWT.", "AGE_", names(alw))
      rownames(alw)<-alw$FLEN
      alw$FLEN<-NULL
      alw<-alw[order(rownames(alw)),]
      alw[is.na(alw)]<-0
      #The means below don't match APL - STRANAL
        # alw<-cbind(alw,"MEAN"=rowMeans(alw, na.rm = TRUE))
        # alw<-rbind(alw, "MEAN"=colMeans(alw, na.rm = TRUE))

      # Age Table --------------------------------------------------------------    

      alk_ap<-as.data.frame(alk)
      alk_ap$TOTAL<-NULL
      alk_ap<-alk_ap[!rownames(alk_ap) %in% "TOTAL",]
      
      ages_prop<-prop.table(as.matrix(alk_ap),1) 
      ages_prop<-ifelse(is.nan(ages_prop),0,ages_prop)
      ages_prop<-as.data.frame(ages_prop)
      theseages<-gsub(pattern = "AGE_", replacement = "", x = c(names(ages_prop)))
      
      lengths = colSums(lengthsTotals[,2:ncol(lengthsTotals)])
      lengths = lengths[lengths>0]
      lengths1<-as.data.frame(lengths)
      lengths1$FLEN<-names(lengths)
      
      lengths1<-merge(ages_prop,lengths1, by.x="row.names", by.y="FLEN", all.x=T)
      lengths1[is.na(lengths1)]<-0
      age_table<-ages_prop*lengths1$lengths
      
      # ages_prop$FLEN<-as.numeric(rownames(ages_prop))
      # ageset<-lset[lset$STRAT!="FAKE",]
      # colnames(ageset)[which(names(ageset) == "x")] <- "CAGE"
      # ageset<-ageset[order(ageset$STRAT,ageset$MISSION, ageset$SETNO),]
      # 
      # ages_pre<-merge(ageset,ages_prop, by="FLEN")
      # ages_pre<-as.data.table(ages_pre)
      # browser()
      # theseages<-names(alk_ap)
      # ages_pre[, (theseages) := lapply(.SD,function(x) x * ages_pre[['CAGE']] ), 
      #          .SDcols = theseages]
      # age_by_set<-aggregate(.~STRAT + MISSION + SETNO, data=ages_pre, sum)
      # age_by_set<-age_by_set[order(age_by_set$STRAT,age_by_set$SETNO),]
      # age_by_set$FLEN<-NULL 
      # age_by_set$CAGE<-NULL
      # 
      # age_mean<-age_by_set
      # age_mean$SETNO<-NULL
      # age_mean$MISSION<-NULL
      # #age_mean = age_mean[rowSums(age_mean[2:ncol(age_mean)])>0,]
      # age_mean<-aggregate(.~STRAT, data=age_mean, mean)
      # setnames(age_mean,
      #          old=names(age_mean[,2:ncol(age_mean)]), 
      #          new=c(paste0("age_",theseages,"_mean")))
      # 
      # 
      # 
      # 
      # #colSums(lengthsData$length_total[2:ncol(lengthsData$length_total)])
      # crap = as.data.frame(cbind(STRAT = lengthsData$length_total$STRAT,TOTAL = rowSums(lengthsData$length_total[2:ncol(lengthsData$length_total)])))
      # theseages<-gsub(pattern = "AGE_", replacement = "", x = c(names(alk_ap)))
      # 
      
      
      res = list(alk=alk, alw=alw, age_table=age_table)
    }
    return(res)
  }
  switch(requested, 
         "lengths" = calcLengths( agency, dfNWSets,dfRawDet, dfRawInf, dfStrata, dfSpp, towDist, sexed),
         "ageKey" = calcAgeKey(agelen, dfSpp, lengthsTotals, lset)
  )
}


