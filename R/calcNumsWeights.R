# @title calcNumsWeights
# @description This function calculates the numbers and weights.
# @param requested  The default value is \code{NULL}. This determines which 
# analytic will be performed.
# @param dfRawCatch  The default value is \code{NULL}. 
# @param dfRawInf  The default value is \code{NULL}.
# @param dfNWSets  The default value is \code{NULL}.
# @param dfStrata  The default value is \code{NULL}.
# @param towDist  The default value is \code{NULL}.
# @param dfNWAgg  The default value is \code{NULL}.
# @family Gale-force
# @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @keywords internal
calcNumsWeights<-function(requested = NULL, dfRawCatch = NULL, dfRawInf = NULL, 
                          dfNWSets = NULL, dfStrata = NULL, towDist = NULL,
                          dfNWAgg = NULL){
  calcNWSets<-function(dfRawCatch, dfRawInf,towDist){
    str_dfNWSets<-merge(dfRawInf, dfRawCatch, all.x=T)
    str_dfNWSets[which(is.na(str_dfNWSets$TOTWGT)),c('SAMPWGT','TOTWGT','TOTNO','CALWT')]<- 0
    str_dfNWSets$SIZE_CLASS[which(is.na(str_dfNWSets$SIZE_CLASS))] <- 1
    str_dfNWSets$DIST[which(is.na(str_dfNWSets$DIST)|(str_dfNWSets$DIST==0))] <-towDist
    str_dfNWSets$RAW_TOTWGT <-str_dfNWSets$TOTWGT
    str_dfNWSets$TOTWGT <- (str_dfNWSets$TOTWGT*towDist)/str_dfNWSets$DIST
    str_dfNWSets$TOTWGT[which(!is.finite(str_dfNWSets$TOTWGT))] <-1
    str_dfNWSets$RAW_TOTNO <-str_dfNWSets$TOTNO 
    str_dfNWSets$TOTNO <- (str_dfNWSets$TOTNO*towDist)/str_dfNWSets$DIST
    str_dfNWSets$TOTNO[which(!is.finite(str_dfNWSets$TOTNO))] <- 1
    str_dfNWSets<-str_dfNWSets[order(str_dfNWSets$STRAT,str_dfNWSets$SETNO),]
    #drop things that are already in inf (except for join conveniences)
    str_dfNWSets <- str_dfNWSets[c('MISSION','SETNO','STRAT',setdiff(colnames(str_dfNWSets), colnames(dfRawInf)))]
    
    names(str_dfNWSets)[names(str_dfNWSets) == "AREA"] <- "UNIT_AREA"
    return(str_dfNWSets)
  }
  
  calcNWAgg<-function(dfNWSets, dfStrata){
    tmp <- merge(dfNWSets,dfStrata, by = 'STRAT')
    tmp$BIOMASS<-tmp$TOTWGT*tmp$TUNITS
    tmp$ABUND<-tmp$TOTNO*tmp$TUNITS
    tmp.cnt<-stats::aggregate(list(COUNT=tmp$STRAT),
                              by=list(STRAT=tmp$STRAT),
                              FUN=length)
    
    tmp.sum<-stats::aggregate(list(TOT_WGT=tmp$TOTWGT,
                                   TOT_NO=tmp$TOTNO),
                              by=list(STRAT=tmp$STRAT),
                              FUN=sum)
    
    tmp.mean<-stats::aggregate(list(MEAN_WGT=tmp$TOTWGT,
                                    MEAN_NO=tmp$TOTNO,
                                    BIOMASS=tmp$BIOMASS,
                                    ABUND=tmp$ABUND),
                               by=list(STRAT=tmp$STRAT),
                               FUN=mean)
    
    tmp.sterr<-stats::aggregate(list(ST_ERR_WGT=tmp$TOTWGT,
                                     ST_ERR_NO=tmp$TOTNO,
                                     ST_ERR_BIOMASS=tmp$BIOMASS,
                                     ST_ERR_ABUND=tmp$ABUND),
                                by=list(STRAT=tmp$STRAT),
                                FUN=Mar.utils::st_err)
    nw<-merge(tmp.cnt,tmp.sum,by="STRAT")
    nw<-merge(nw, tmp.mean,by="STRAT")
    nw<-merge(nw, tmp.sterr,by="STRAT")
    nw[is.na(nw)]<-0
    return(nw)
  }
  calcStrataProp<-function(dfNWSets,dfStrata,dfNWAgg){
    tmp = dfNWSets[c("STRAT","TOTWGT","TOTNO")]
    #seems correct, but APL STRANAL only considers TOTWGT
    #tmp$SOMECATCH[tmp$TOTWGT!=0 | tmp$TOTNO!=0]<-1
    tmp$SOMECATCH[tmp$TOTWGT!=0]<-1
    
    tmp$SOMECATCH[is.na(tmp$SOMECATCH)]<-0
    tmp<-merge(dfStrata,tmp,by="STRAT",all.y=T)
    
    tmp<-merge(tmp,dfNWAgg[c("STRAT","COUNT")],by="STRAT",all.x=T)
    tmp$AREA_CALC<-tmp$SQNM*tmp$SOMECATCH
    tmp.AreaProp<-stats::aggregate(list(AREAPROP=tmp$SOMECATCH), 
                                   by=list(STRAT=tmp$STRAT), 
                                   FUN=mean)
    tmp.LoneStrat<-stats::aggregate(list(AREACNT=tmp$SOMECATCH), 
                                    by=list(STRAT=tmp$STRAT), 
                                    FUN=length)
    tmp.AreaPropStErr<-stats::aggregate(list(AREAPROPSTERR=tmp$SOMECATCH),
                                        by=list(STRAT=tmp$STRAT),
                                        FUN=Mar.utils::st_err)
    tmp.AreaTot<-stats::aggregate(list(AREATOT=tmp$AREA_CALC),
                                  by=list(STRAT=tmp$STRAT),
                                  FUN=mean)
    tmp.AreaTotStErr<-stats::aggregate(list(AREATOTSTERR=tmp$AREA_CALC),
                                       by=list(STRAT=tmp$STRAT),
                                       FUN=Mar.utils::st_err)
    str_stratAreaDet<-merge(dfStrata,tmp.AreaProp,by="STRAT",all.x=T)
    str_stratAreaDet<-merge(str_stratAreaDet,tmp.LoneStrat,by="STRAT",all.x=T)
    str_stratAreaDet<-merge(str_stratAreaDet,tmp.AreaPropStErr,by="STRAT",all.x=T)
    str_stratAreaDet<-merge(str_stratAreaDet,tmp.AreaTot,by="STRAT",all.x=T)
    str_stratAreaDet<-merge(str_stratAreaDet,tmp.AreaTotStErr,by="STRAT",all.x=T)
    str_stratAreaDet[is.na(str_stratAreaDet)]<-0
    str_stratAreaDet <- str_stratAreaDet[c('STRAT',setdiff(colnames(str_stratAreaDet), colnames(dfStrata)))]
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
         "sets" = calcNWSets(dfRawCatch, dfRawInf, towDist),
         "setsAgg" = calcNWAgg(dfNWSets, dfStrata),
         "strataProp" = calcStrataProp(dfNWSets, dfStrata, dfNWAgg)
  )
}