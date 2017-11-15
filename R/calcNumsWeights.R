#' @title calcNumsWeights
#' @description This function calculates the numbers and weights.
#' @param requested  The default value is \code{NULL}. 
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom Mar.utils SQL_in
#' @export
calcNumsWeights<-function(requested = NULL){
  calcNWSets<-function(){
    # numbers and weights ----
    str_dfNWSets<-merge(.GlobalEnv$str_dfRawInf, .GlobalEnv$str_dfRawCatch, all.x=T)
    str_dfNWSets[which(is.na(str_dfNWSets$TOTWGT)),c('SAMPWGT','TOTWGT','TOTNO','CALWT')]<- 0
    str_dfNWSets$SIZE_CLASS[which(is.na(str_dfNWSets$SIZE_CLASS))] <- 1
    str_dfNWSets$DIST[which(is.na(str_dfNWSets$DIST)|(str_dfNWSets$DIST==0))] <-.GlobalEnv$str_towDist
    str_dfNWSets$RAW_TOTWGT <-str_dfNWSets$TOTWGT
    str_dfNWSets$TOTWGT <- (str_dfNWSets$TOTWGT*.GlobalEnv$str_towDist)/str_dfNWSets$DIST
    str_dfNWSets$TOTWGT[which(!is.finite(str_dfNWSets$TOTWGT))] <-1
    str_dfNWSets$RAW_TOTNO <-str_dfNWSets$TOTNO 
    str_dfNWSets$TOTNO <- (str_dfNWSets$TOTNO*.GlobalEnv$str_towDist)/str_dfNWSets$DIST
    str_dfNWSets$TOTNO[which(!is.finite(str_dfNWSets$TOTNO))] <- 1
    str_dfNWSets<-str_dfNWSets[order(str_dfNWSets$STRAT,str_dfNWSets$SETNO),]
    #drop things that are already in inf (except for join conveniences)
    str_dfNWSets <- str_dfNWSets[c('MISSION','SETNO','STRAT',setdiff(colnames(str_dfNWSets), colnames(.GlobalEnv$str_dfRawInf)))]
    
    names(str_dfNWSets)[names(str_dfNWSets) == "AREA"] <- "UNIT_AREA"
    return(str_dfNWSets)
  }

  calcNWAgg<-function(){
    tmp <- merge(str_dfNWSets,str_dfStrata, by = 'STRAT')
    tmp$BIOMASS<-tmp$TOTWGT*tmp$TUNITS
    tmp$ABUND<-tmp$TOTNO*tmp$TUNITS
    tmp.cnt<-aggregate(list(COUNT=tmp$STRAT),
                                by=list(STRAT=tmp$STRAT),
                                FUN=length)

    tmp.sum<-aggregate(list(TOT_WGT=tmp$TOTWGT,
                                     TOT_NO=tmp$TOTNO),
                                by=list(STRAT=tmp$STRAT),
                                FUN=sum)

    tmp.mean<-aggregate(list(MEAN_WGT=tmp$TOTWGT,
                                      MEAN_NO=tmp$TOTNO,
                                      BIOMASS=tmp$BIOMASS,
                                      ABUND=tmp$ABUND),
                                 by=list(STRAT=tmp$STRAT),
                                 FUN=mean)

    tmp.sterr<-aggregate(list(ST_ERR_WGT=tmp$TOTWGT,
                                       ST_ERR_NO=tmp$TOTNO,
                                       ST_ERR_BIOMASS=tmp$BIOMASS,
                                       ST_ERR_ABUND=tmp$ABUND),
                                  by=list(STRAT=tmp$STRAT),
                                  FUN=Mar.utils::st_err)
    nw<-merge(tmp.cnt,tmp.sum,by="STRAT")
    nw<-merge(nw, tmp.mean,by="STRAT")
    nw<-Mar.utils::na_zero(merge(nw, tmp.sterr,by="STRAT"))
    return(nw)
    }
  calcStrataProp<-function(){
      tmp = str_dfNWSets[c("STRAT","TOTWGT","TOTNO")]
      tmp$SOMECATCH[tmp$TOTWGT!=0 | tmp$TOTNO!=0]<-1
      tmp$SOMECATCH[is.na(tmp$SOMECATCH)]<-0
      tmp<-merge(str_dfStrata,tmp,by="STRAT",all.y=T)
      
      tmp<-merge(tmp,str_dfNWAgg[c("STRAT","COUNT")],by="STRAT",all.x=T)
      tmp$AREA_CALC<-tmp$SQNM*tmp$SOMECATCH
      
      tmp.AreaProp<-aggregate(list(AREAPROP=tmp$SOMECATCH), 
                                               by=list(STRAT=tmp$STRAT), 
                                               FUN=mean)
      tmp.LoneStrat<-aggregate(list(AREACNT=tmp$SOMECATCH), 
                               by=list(STRAT=tmp$STRAT), 
                               FUN=length)
      tmp.AreaPropStErr<-aggregate(list(AREAPROPSTERR=tmp$SOMECATCH),
                                         by=list(STRAT=tmp$STRAT),
                                         FUN=Mar.utils::st_err)
      tmp.AreaTot<-aggregate(list(AREATOT=tmp$AREA_CALC),
                                   by=list(STRAT=tmp$STRAT),
                                   FUN=mean)
      tmp.AreaTotStErr<-aggregate(list(AREATOTSTERR=tmp$AREA_CALC),
                                        by=list(STRAT=tmp$STRAT),
                                        FUN=Mar.utils::st_err)
      str_stratAreaDet<-merge(str_dfStrata,tmp.AreaProp,by="STRAT",all.x=T)
      str_stratAreaDet<-merge(str_stratAreaDet,tmp.LoneStrat,by="STRAT",all.x=T)
      str_stratAreaDet<-merge(str_stratAreaDet,tmp.AreaPropStErr,by="STRAT",all.x=T)
      str_stratAreaDet<-merge(str_stratAreaDet,tmp.AreaTot,by="STRAT",all.x=T)
      str_stratAreaDet<-Mar.utils::na_zero(merge(str_stratAreaDet,tmp.AreaTotStErr,by="STRAT",all.x=T))
      str_stratAreaDet <- str_stratAreaDet[c('STRAT',setdiff(colnames(str_stratAreaDet), colnames(str_dfStrata)))]
      if(nrow(str_stratAreaDet[str_stratAreaDet$AREACNT==1,])>0){
        #If there's a single record within a strata APL doesn't give 0 as the STERRs
        str_stratAreaDet[str_stratAreaDet$AREACNT==1,]$AREAPROPSTERR<- str_stratAreaDet[str_stratAreaDet$AREACNT==1,]$AREAPROP
        str_stratAreaDet[str_stratAreaDet$AREACNT==1,]$AREATOTSTERR <- str_stratAreaDet[str_stratAreaDet$AREACNT==1,]$AREATOT
      }
      #APL does not provide the strata with no catches.  Could remove them here.if(nrow(str_stratAreaDet[str_stratAreaDet$AREACNT==1,])>0){
      #If there's a single record within a strata APL doesn't give 0 as the STERRs
        str_stratAreaDet = str_stratAreaDet[str_stratAreaDet$AREACNT>0,] 
        
      str_stratAreaDet$AREACNT<-NULL
      return(str_stratAreaDet)
    }
  
  switch(requested, 
         "sets" = calcNWSets(),
         "setsAgg" = calcNWAgg(),
         "strataProp" = calcStrataProp()
  )
}