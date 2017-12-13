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
#' @param bySex  The default value is \code{NULL}.
#' @param agelen  The default value is \code{NULL}.
#' @param lengthsTotals  The default value is \code{NULL}.
#' @param lset  The default value is \code{NULL}.
#' @param output  The default value is \code{NULL}.
#' @param ageBySex  The default value is \code{NULL}.
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom Mar.utils SQL_in
#' @importFrom Mar.utils st_err
#' @importFrom stats setNames
#' @importFrom stats reshape
#' @importFrom reshape2 dcast
#' @importFrom reshape2 colsplit
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
calcAgeLen<-function(requested = NULL, agency = NULL, dfNWSets = NULL, 
                     dfRawDet=NULL, dfRawInf = NULL, dfStrata = NULL, 
                     dfSpp=NULL, towDist=NULL,  bySex = NULL,
                     agelen = NULL, lengthsTotals = NULL, lset=NULL,
                     output = NULL, ageBySex = NULL){
  calcLengths<-function( agency, dfNWSets,dfRawDet, dfRawInf, dfStrata, dfSpp, 
                         towDist, bySex){
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
    
    if (bySex) {
      allfields <-c("STRAT","MISSION","SETNO", "FSEX", "FLEN","CAGE") 
    }else{
      allfields <-c("STRAT","MISSION","SETNO","FLEN","CAGE")
    }

    lset=agelen[,allfields]
    lset <- aggregate(lset[,allfields]$CAGE,
                      lset[allfields !="CAGE"],
                      FUN=sum)
    lset <- lset[order(lset$STRAT,lset$MISSION,lset$SETNO),]
    #get lset data with all possible length groups for all sexes
    rng = range(lset$FLEN, na.rm = T)
    allLength = seq(min(rng),max(rng), by=sppLgrp)

    if (bySex){
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
    
    if (bySex) {
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
    if (bySex) {
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
  calcAgeKey<-function(agelen, dfSpp, lengthsTotals, lset, dfStrata, bySex, 
                       output, ageBySex){
    if (nrow(agelen[!is.na(agelen$AGE),])==0)return(-1)
    theSexes <- unique(lset$FSEX)
    if (length(theSexes)==1){
      if(ageBySex==TRUE)cat(paste0("All individuals are FSEX = ",theSexes,"
Reverting to ageBySex=FALSE"))
      ageBySex == FALSE
      bySex == FALSE
    }
    
    
    sppLgrp = dfSpp$LGRP
    
    setID=unique(lset[lset$STRAT!="FAKE",c("STRAT", "MISSION", "SETNO")])
    setID = setID[order(setID$STRAT,setID$MISSION,setID$SETNO),]
    
    stratID=dfStrata[,c("STRAT", "TUNITS")]
    
    # Age Length Key -----------------------------------------------------------
    alk<-agelen[!is.na(agelen$AGE), ]
    all.ages = seq(min(alk$AGE),max(alk$AGE)) 
    rng = range(alk$FLEN, na.rm = T)
    allLength = seq(min(rng),max(rng), by=sppLgrp)
    if (bySex){
      alk<-alk[,c("AGE","FLEN","FSEX","CAGE","SETNO")]
      allSex=c(0,1,2)
      al = expand.grid(all.ages,allLength, allSex)
      names(al) = c('AGE','FLEN','FSEX')
      al$SETNO = al$CAGE = 0
      alk = rbind(alk,al)
      alk$SETNO = ifelse(alk$SETNO>0,1,NA)
      alkPre<-
        tapply(
            alk$SETNO,
            list(alk$AGE, alk$FLEN,alk$FSEX),
            function(x) length(x[!is.na(x)]), simplify=TRUE
        )
      
      unkn<-t(alkPre[,,1])
      rownames(unkn)<-paste0("UNKN ", rownames(unkn) )
      male<-t(alkPre[,,2])
      rownames(male)<-paste0("MALE ", rownames(male) )
      female<-t(alkPre[,,3])
      rownames(female)<-paste0("FEMALE ", rownames(female) )
      
      alkThis = rbind(unkn,male, female)
    }else{
      alk<-alk[,c("AGE","FLEN","CAGE","SETNO")]
      al = expand.grid(all.ages,allLength)
      names(al) = c('AGE','FLEN')
      al$SETNO = al$CAGE = 0
      alk = rbind(alk,al)
      alk$SETNO = ifelse(alk$SETNO>0,1,NA)
      alkThis<-
        t(
          tapply(
            alk$SETNO,
            list(alk$AGE, alk$FLEN),
            function(x) length(x[!is.na(x)])
          )
        )
    }
      alk<-alkThis
      
      colnames(alk) <- paste("AGE", colnames(alk), sep = "_")
      alk<-cbind(alk,"TOTAL"=rowSums(alk)) 
      alk<-rbind(alk, "TOTAL"=colSums(alk))
      # Age Length Weight ------------------------------------------------------
      
      if (bySex){
        alw<-  aggregate(FWT~AGE+FLEN+FSEX,data=agelen,FUN=mean)
        alw<-alw[order(alw$FSEX,alw$AGE,alw$FLEN),]
        alw$FWT = alw$FWT / 1000
        alw = reshape(alw,idvar=c('FSEX','FLEN'),timevar='AGE',direction='wide')
        alw$FLEN <-paste0(alw$FSEX,"_",alw$FLEN)
        alw$FSEX<-NULL
        alw$FLEN<-sub(pattern="0_",replacement = "UNKN_", alw$FLEN)
        alw$FLEN<-sub(pattern="1_",replacement = "MALE_", alw$FLEN)
        alw$FLEN<-sub(pattern="2_",replacement = "FEMALE_", alw$FLEN)
      }else{
        alw<-aggregate(FWT~AGE+FLEN,data=agelen,FUN=mean)
        alw<-alw[order(alw$AGE,alw$FLEN),]
        alw$FWT = alw$FWT / 1000
        alw = reshape(alw,idvar='FLEN',timevar='AGE',direction='wide')
      }
      names(alw) <- gsub("FWT.", "AGE_", names(alw))
      rownames(alw)<-alw$FLEN
      alw$FLEN<-NULL
      alw<-alw[order(rownames(alw)),]
      alw[is.na(alw)]<-0

      # Age Table --------------------------------------------------------------    

      alk_ap<-as.data.frame(alk)
      alk_ap$TOTAL<-NULL
      alk_ap<-alk_ap[!rownames(alk_ap) %in% "TOTAL",]
      ages_prop<-prop.table(as.matrix(alk_ap),1) 
      ages_prop<-ifelse(is.nan(ages_prop),0,ages_prop)
      ages_prop<-as.data.frame(ages_prop)
      theseages<-gsub(pattern = "AGE_", replacement = "", x = c(names(ages_prop)))
      if (bySex){
        
        lengthsTotals_u <- lengthsTotals[,grep(pattern="0_",colnames(lengthsTotals))]
        totals_u<-colSums(lengthsTotals_u)
        totals_u<-as.data.frame(totals_u)
        colnames(totals_u)<-"lengths"
        rownames(totals_u)<- gsub("0_","UNKN ", rownames(totals_u))
        # colnames(lengthsTotals_u)<-gsub("0_",replacement = "", colnames(lengthsTotals_u))
        # rownames(lengthsTotals_u)<-paste0("UNKN ", rownames(lengthsTotals_u))
        
        lengthsTotals_m <- lengthsTotals[,grep(pattern="1_",colnames(lengthsTotals))]
        totals_m<-colSums(lengthsTotals_m)
        totals_m<-as.data.frame(totals_m)
        colnames(totals_m)<-"lengths"
        rownames(totals_m)<- gsub("1_","MALE ", rownames(totals_m))
        
        lengthsTotals_f <- lengthsTotals[,grep(pattern="2_",colnames(lengthsTotals))]
        totals_f<-colSums(lengthsTotals_f)
        totals_f = as.data.frame(totals_f)
        colnames(totals_f)<-"lengths"
        rownames(totals_f)<-gsub("2_","FEMALE ", rownames(totals_f))
        lengths = rbind(totals_u, totals_m,totals_f)
        
      }else{
        lengths = colSums(lengthsTotals[,2:ncol(lengthsTotals)])   
        lengths = lengths[lengths>0]
        lengths<-as.data.frame(lengths)
      }
      
      ages_prop_l<-merge(ages_prop,lengths, by="row.names")
      rownames(ages_prop_l)<-ages_prop_l$Row.names
      ages_prop_l$Row.names<-NULL
      ages_prop_l[, -which(names(ages_prop_l) == "lengths")]<-ages_prop_l[, -which(names(ages_prop_l) == "lengths")] * ages_prop_l[["lengths"]]
      age_table<-ages_prop_l[, -which(names(ages_prop_l) == "lengths")]
      age_table[is.na(age_table)]<-0
      # Age by Set -------------------------------------------------------------  
      if (bySex){
        colNs=c("FSEX", "FLEN")
        cols=colsplit(rownames(ages_prop)," ", colNs)
        
        if (nrow(cols[cols$FSEX=='UNKN',])>0) cols[cols$FSEX=='UNKN',]$FSEX<-0
        if (nrow(cols[cols$FSEX=='MALE',])>0) cols[cols$FSEX=='MALE',]$FSEX<-1
        if (nrow(cols[cols$FSEX=='FEMALE',])>0) cols[cols$FSEX=='FEMALE',]$FSEX<-2
        
        ages_prop <- cbind(ages_prop,cols) 
        rownames(ages_prop)<-NULL
        ageset<-lset[lset$STRAT!="FAKE",]
        colnames(ageset)[which(names(ageset) == "x")] <- "CAGE"
        ageset<-ageset[order(ageset$STRAT,ageset$MISSION, ageset$SETNO),]
        ages_pre<-merge(ageset,ages_prop) 
        ages_pre<-as.data.table(ages_pre)
        theseages<-names(alk_ap)
        ages_pre[, (theseages) := lapply(.SD,function(x) x * ages_pre[['CAGE']] ), 
                 .SDcols = theseages]
        age_by_set<-aggregate(.~STRAT + MISSION + SETNO + FSEX, data=ages_pre, sum)
        age_by_set$FLEN<-NULL 
        age_by_set$CAGE<-NULL
        age_by_set=merge(setID,age_by_set, all.x=TRUE)
        age_by_set[is.na(age_by_set)]<-0
        age_by_set<-age_by_set[order(age_by_set$FSEX, age_by_set$STRAT,age_by_set$SETNO),]
        
      }else{
        ages_prop$FLEN<-as.numeric(rownames(ages_prop))
        ageset<-lset[lset$STRAT!="FAKE",]
        colnames(ageset)[which(names(ageset) == "x")] <- "CAGE"
        ageset<-ageset[order(ageset$STRAT,ageset$MISSION, ageset$SETNO),]
        ages_pre<-merge(ageset,ages_prop, by="FLEN") 
        ages_pre<-as.data.table(ages_pre)
        theseages<-names(alk_ap)
        ages_pre[, (theseages) := lapply(.SD,function(x) x * ages_pre[['CAGE']] ), 
                 .SDcols = theseages]
        age_by_set<-aggregate(.~STRAT + MISSION + SETNO, data=ages_pre, sum)
         age_by_set$FLEN<-NULL 
        age_by_set$CAGE<-NULL
        age_by_set=merge(setID,age_by_set, all.x=TRUE)
        age_by_set[is.na(age_by_set)]<-0
        age_by_set<-age_by_set[order(age_by_set$STRAT,age_by_set$SETNO),]
      }

      if (ageBySex == FALSE | output=="classic"){
        age_by_set$FSEX<-NULL
        age_by_set<-aggregate(.~STRAT + MISSION + SETNO, data=age_by_set, sum)
        age_by_set<-age_by_set[order(age_by_set$STRAT,age_by_set$SETNO),]
      }else{
        age_by_set<-aggregate(.~STRAT + MISSION + SETNO + FSEX, data=age_by_set, sum)
        age_by_set<-age_by_set[order(age_by_set$FSEX, age_by_set$STRAT,age_by_set$SETNO),]
      }
     

       # Age Mean ---------------------------------------------------------------  
      age_mean<-age_by_set
      age_mean$SETNO<-NULL
      age_mean$MISSION<-NULL
      if (ageBySex == FALSE | output=="classic"){
        age_mean<-aggregate(.~STRAT, data=age_mean, mean)
        age_mean=merge(stratID,age_mean, all.x=TRUE)
        age_mean<-age_mean[order(age_mean$STRAT),]
      }else{
        age_mean<-aggregate(.~STRAT+FSEX, data=age_mean, mean)
        #make empty dataframe
        age_mean_all<-age_mean[FALSE,]
        for (i in 1:length(theSexes)){
          this=age_mean[age_mean$FSEX==i,]
          this=merge(stratID,this, all.x=TRUE)
          this$FSEX<-i
          this<-this[order(this$STRAT),]
          age_mean_all = rbind(age_mean_all, this)
        }
        age_mean<-age_mean_all
      }
      
      age_mean$TUNITS<-NULL
      age_mean[is.na(age_mean)]<-0
      
      # Age Mean SE ------------------------------------------------------------ 
      age_mean_se<-age_by_set
      age_mean_se$SETNO<-NULL
      age_mean_se$MISSION<-NULL
      if (ageBySex == FALSE | output=="classic"){
        age_mean_se<-aggregate(.~STRAT, data=age_mean_se, st_err)
        age_mean_se=merge(stratID,age_mean_se, all.x=TRUE)
        age_mean_se<-age_mean_se[order(age_mean_se$STRAT),]
      }else{
        age_mean_se<-aggregate(.~STRAT+FSEX, data=age_mean_se, st_err)
        #make empty dataframe
        age_mean_se_all<-age_mean_se[FALSE,]
        for (i in 1:length(theSexes)){
          this=age_mean_se[age_mean_se$FSEX==i,]
          this=merge(stratID,this, all.x=TRUE)
          this$FSEX<-i
          this<-this[order(this$STRAT),]
          age_mean_se_all = rbind(age_mean_se_all, this)
        }
        age_mean_se<-age_mean_se_all
      }
      
      age_mean_se$TUNITS<-NULL
      age_mean_se[is.na(age_mean_se)]<-0
      
      # Age Totals ------------------------------------------------------------- 
      age_pretotal<-as.data.table(merge(stratID,age_by_set, by="STRAT"))
      age_pretotal[, (theseages) := 
                  lapply(.SD,function(x) x * age_pretotal[['TUNITS']] ),
                    .SDcols = theseages]
      age_pretotal$TUNITS<-NULL
      age_pretotal$MISSION<-NULL
      age_pretotal$SETNO<-NULL
      
      if (ageBySex == FALSE | output=="classic"){
        age_total<-aggregate(.~STRAT, data=age_pretotal, mean)
        age_total=merge(stratID,age_total, all.x=TRUE)
        age_total<-age_total[order(age_total$STRAT),]
      }else{
        age_total<-aggregate(.~STRAT+FSEX, data=age_pretotal, st_err)
        #make empty dataframe
        age_total_all<-age_total[FALSE,]
        for (i in 1:length(theSexes)){
          this=age_total[age_total$FSEX==i,]
          this=merge(stratID,this, all.x=TRUE)
          this$FSEX<-i
          this<-this[order(this$STRAT),]
          age_total_all = rbind(age_total_all, this)
        }
        age_total<-age_total_all
      }
      
      age_total$TUNITS<-NULL
      age_total[is.na(age_total)]<-0
      

      if (ageBySex == FALSE | output=="classic"){
        age_total_se<-aggregate(.~STRAT, data=age_pretotal, st_err)
        age_total_se=merge(stratID,age_total_se, all.x=TRUE)
        age_total_se<-age_total_se[order(age_total_se$STRAT),]
      }else{
        age_total_se<-aggregate(.~STRAT+FSEX, data=age_pretotal, st_err) #make empty dataframe
        age_total_se_all<-age_total_se[FALSE,]
        for (i in 1:length(theSexes)){
          this=age_total_se[age_total_se$FSEX==i,]
          this=merge(stratID,this, all.x=TRUE)
          this$FSEX<-i
          this<-this[order(this$STRAT),]
          age_total_se_all = rbind(age_total_se_all, this)
        }
        age_total_se<-age_total_se_all
      }
      
      age_total_se$TUNITS<-NULL
      age_total_se[is.na(age_total_se)]<-0
      
      res = list(alk=alk, alw=alw, age_table=age_table, age_mean=age_mean,
                 age_by_set = age_by_set, age_mean_se= age_mean_se, 
                 age_total=age_total, age_total_se = age_total_se)
    return(res)
  }
  switch(requested, 
         "lengths" = calcLengths( agency, dfNWSets,dfRawDet, dfRawInf, dfStrata, 
                                  dfSpp, towDist, bySex),
         "ageKey" = calcAgeKey(agelen, dfSpp, lengthsTotals, lset, dfStrata, 
                               bySex, output, ageBySex)
  )
}


