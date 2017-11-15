#' @title stranal
#' @description Stuff.  
#' @param usepkg  The default value is \code{'roracle'}, but \code{'rodbc'} is works as well.
#' This describes the R package you use to connect to Oracle.  
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}l
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom stats aggregate
#' @importFrom Mar.utils make_oracle_cxn
#' @importFrom Mar.utils na_zero
#' @importFrom Mar.utils SQL_in
#' @importFrom Mar.utils st_err
#' @export

stranal<-function(usepkg = 'roracle', 
                  str_agency = 'DFO',
                  str_type = 1,
                  str_wingspread = 41,
                  str_towDist = 1.75
                  ){
  assign("oracle_cxn", Mar.utils::make_oracle_cxn(usepkg), envir = .GlobalEnv )
  if (is.null(str_agency)) assign("str_agency", getUserInput("agency"), envir = .GlobalEnv )
  if (is.null(str_type)) assign("str_type", getUserInput("type"), envir = .GlobalEnv )
  missionsAndStrata = getUserInput("missionsAndStrata")
    assign("str_dfMissions", missionsAndStrata[[1]], envir = .GlobalEnv )
    assign("str_dfMissionsStrata", missionsAndStrata[[2]], envir = .GlobalEnv )
    rm(missionsAndStrata)

  assign("str_strataTable", getUserInput("strataTable"), envir = .GlobalEnv )
  if (is.null(str_wingspread)) assign("str_wingspread", getUserInput("wingspread"), envir = .GlobalEnv )
  if (is.null(str_towDist)) assign("str_towDist", getUserInput("towDist"), envir = .GlobalEnv )
  assign("str_dfStrata", getUserInput("strata"), envir = .GlobalEnv )
  
  spp = getUserInput("spp")
    assign("str_sexed", spp[[1]], envir = .GlobalEnv )
    assign("str_dfSpp", spp[[2]], envir = .GlobalEnv )
    rm(spp)

  assign("str_dfRawCatch", extractData('catch'), envir = .GlobalEnv )
  assign("str_dfRawInf", extractData('inf'), envir = .GlobalEnv )
  assign("str_dfRawDet", extractData('det'), envir = .GlobalEnv )

  assign("str_dfNWSets", calcNumsWeights('sets'), envir = .GlobalEnv )
  
  assign("str_dfNWAgg", calcNumsWeights('setsAgg'), envir = .GlobalEnv )

  assign("str_dfStrata", merge(str_dfStrata, calcNumsWeights('strataProp')), envir = .GlobalEnv )
  #perfect till here (though this R version gives strata that no sets)
  
  tt=calcAgeLen('doEg')
  assign('tt', tt, envir = .GlobalEnv)

  # ################################################################################
  # ###                          LENGTH ANALYTICS                                   
  # ###    LENGTH BY SET
  # ###    LENGTH MEAN
  # ###    LENGTH MEAN STANDARD ERROR 
  # ###    LENGTH TOTAL
  # ###    LENGTH TOTAL STANDARD ERROR
  # all.c<-c("STRAT", "MISSION", "SETNO")
  # if (isTRUE(by.sex)){
  #   order.c<-c("STRAT","MISSION","SETNO","FSEX")
  #   sex.c<-c("STRAT","SLAT","SLONG","AREA", "MISSION","SETNO","FLEN","CAGE","FSEX") 
  #   fields<-c("STRAT","SLAT","SLONG","AREA", "MISSION","SETNO","FLEN","FSEX")
  # }else{
  #   order.c<-c("STRAT","MISSION","SETNO")
  #   sex.c<-c("STRAT","SLAT","SLONG","AREA", "MISSION","SETNO","FLEN","CAGE")
  #   fields<-c("STRAT","SLAT","SLONG","AREA", "MISSION","SETNO","FLEN")
  # }
  # 
  # lset<-na.zero(agelen[,sex.c])
  # li = which(lset$CAGE==0) 
  # lset$FLEN[li] = unique(lset$FLEN)[1]
  # lset<-aggregate(lset$CAGE,
  #                 lset[fields],
  #                 FUN=sum)
  # 
  # lset<-lset[with(lset,order(get(order.c))),]
  # lset<-melt(lset,id.vars=fields)
  # #not very slick - would like to be able to dynamically send columns to dcast
  # if (isTRUE(by.sex)){  
  #   
  #   lset$FSEX[is.na(lset$FSEX)]<-'UNK' #MMM Oct 28, 2015
  #   lset$FSEX[lset$FSEX==0]<-'UNK'
  #   lset$FSEX[lset$FSEX==1]<-'MALE'
  #   lset$FSEX[lset$FSEX==2]<-'FEMALE'
  #   length_by_set <- na.zero(dcast(lset, STRAT + MISSION + SETNO ~ FSEX +FLEN  ))
  # }else{
  #   #following gives - Aggregation function missing: defaulting to length
  #   length_by_set <- na.zero(dcast(lset, STRAT + MISSION + SETNO ~ FLEN ))
  # }
  # length_by_set<-length_by_set[order(length_by_set$STRAT,length_by_set$SETNO),]
  # length_total<-merge(subset(strata.area,select=-c(SQNM)),length_by_set)
  # #unexpected sqnm
  # #add row_tots to length_by_set
  # length_by_set$TOTAL<-rowSums(length_by_set[,4:length(length_by_set)])
  # 
  # #separate the length and non-length-related data for the sets of the dataframe
  # length_total_pre  <-length_total[,c("STRAT","TUNITS")]
  # length_total_strat_data       <-length_total[,c(1,5:ncol(length_total))]
  # #capture all of the column names of the data
  # colNames<-names(length_total_strat_data)
  # #count sets/strata
  # length_total_strat_data.cnt<-aggregate(list(
  #   COUNT=length_total_strat_data$STRAT), 
  #   by=list(STRAT=length_total_strat_data$STRAT), 
  #   FUN=length)
  # 
  # #add an additional rowname for the column totals 
  # length_rownames<-c(length_total_strat_data.cnt[,1],'Total')
  # length_rownames_noTotal<-c(length_total_strat_data.cnt[,1])
  # #mean length
  # length_mean<-setNames(aggregate(
  #   list(length_total_strat_data[,2:ncol(length_total_strat_data)]), 
  #   by=list(STRAT=length_total_strat_data$STRAT), 
  #   FUN=mean), colNames)
  # #drop strata column
  # length_mean<-length_mean[,-1]
  # #add rowsums
  # length_mean<-cbind(length_mean,RowTotals=rowSums(length_mean)) 
  # #add rowname
  # length_mean<-cbind(STRAT=length_rownames_noTotal,length_mean)        
  # 
  # #mean length std error 
  # length_mean_se<-setNames(aggregate(
  #   list(length_total_strat_data[,c(2:ncol(length_total_strat_data))]), 
  #   by=list(STRAT=length_total_strat_data$STRAT), 
  #   FUN=st.err), colNames)
  # #drop strata column
  # length_mean_se<-length_mean_se[,-1] 
  # #add rowname
  # length_mean_se<-na.zero(cbind(STRAT=length_rownames_noTotal,length_mean_se))            
  # 
  # #multiply all length data by tunits for "total length"
  # length_total_strat_data_tunits<-cbind(STRAT=length_total_pre$STRAT,
  #                                       length_total[,c(5:ncol(length_total))]*
  #                                         length_total_pre$TUNITS)
  # 
  # #total length
  # length_total<-setNames(aggregate(list(
  #   length_total_strat_data_tunits[,2:ncol(length_total_strat_data_tunits)]), 
  #   by=list(STRAT=length_total_strat_data_tunits$STRAT), 
  #   FUN=mean), colNames)
  # #drop strata column
  # length_total<-length_total[,-1] 
  # #add rowsums
  # length_total<-cbind(length_total,RowTotals=rowSums(length_total)) 
  # #separated this because I use it in age calculations
  # ColTotalsLength=colSums(length_total) 
  # #add colsums
  # length_total<-rbind(length_total,ColTotals=ColTotalsLength) 
  # #add rowname
  # length_total<-na.zero(cbind(STRAT=length_rownames,length_total))          
  # 
  # #total length std error 
  # length_total_se<-setNames(aggregate(list(
  #   length_total_strat_data_tunits[,2:ncol(length_total_strat_data_tunits)]), 
  #   by=list(STRAT=length_total_strat_data_tunits$STRAT), 
  #   FUN=st.err), colNames)
  # #drop strata column
  # length_total_se<-length_total_se[,-1] 
  # #add rowname
  # length_total_se<-na.zero(cbind(STRAT=length_rownames_noTotal,length_total_se))          
  # 
  # 
  # ################################################################################
  # ###                          AGE LENGTH KEY                                     
  # 
  # alk<-agelen[,c("AGE","FLEN","CAGE","SETNO")]
  # alk<-alk[!is.na(alk$AGE), ]
  # if (nrow(alk)<1){     #only try age calculations if we have ages
  #   print("age calculations unavailable - no ages in data")
  #   age_by_set<-"can't do age_by_set - no ages in data"
  #   ages<-"can't do ages - no ages in data"
  #   age.length.key.totals<-"can't do age.length.key.totals - no ages in data"
  #   age_table<-"can't do age_table - no ages in data"
  #   age_length_weight<-"can't do age_length_weight - no ages in data"
  # }else{
  #   all.ages = seq(min(alk$AGE),max(alk$AGE)) 
  #   all.lengths = seq(min(alk$FLEN),max(alk$FLEN),by=as.numeric(species.lgrp.gui )) 
  #   al = expand.grid(all.ages,all.lengths)
  #   names(al) = c('AGE','FLEN')
  #   al$SETNO = al$CAGE = 0
  #   alk = rbind(alk,al)
  #   alk$SETNO = ifelse(alk$SETNO>0,1,NA)
  #   
  #   age.length.key<-na.zero(
  #     t(
  #       tapply(
  #         alk$SETNO,
  #         list(alk$AGE, alk$FLEN),
  #         function(x) length(x[!is.na(x)])
  #       )
  #     )
  #   )
  #   Length_Totals<-rowSums(age.length.key, dims = 1)
  #   
  #   age.length.key.totals<-cbind(age.length.key,Length_Totals) 
  #   
  #   Age_Totals<-colSums(age.length.key.totals, dims = 1)
  #   age.length.key.totals<-rbind(age.length.key.totals,Age_Totals)
  #   
  #   alw<-aggregate(FWT~AGE+FLEN,data=agelen,FUN=mean)
  #   alw<-alw[order(alw$AGE,alw$FLEN),]
  #   alw$FWT = alw$FWT / 1000
  #   age_length_weight = na.zero(reshape(
  #     alw,idvar='FLEN',timevar='AGE',direction='wide'))
  #   age_length_weight<-age_length_weight[order(age_length_weight$FLEN),]
  #   rownames(age_length_weight)<-age_length_weight[,1]
  #   age_length_weight$FLEN<-NULL
  #   ################################################################################
  #   ###                          AGE CALCULATIONS                                   
  #   
  #   
  #   
  #   ages_prop<-prop.table(as.matrix(age.length.key),1) 
  #   ages_prop<-ifelse(is.nan(ages_prop),0,ages_prop)
  #   ages_prop<-as.data.frame(ages_prop)
  #   theseages<-c(names(ages_prop))
  #   
  #   lengths<-ColTotalsLength[1:length(ColTotalsLength)-1] 
  #   lengths1<-as.data.frame(lengths)
  #   lengths1$FLEN<-names(lengths)
  #   lengths1<-merge(ages_prop,lengths1,all.x=T)
  #   
  #   age_table<-na.zero(ages_prop*lengths1$lengths)
  #   
  #   ages_prop$FLEN<-as.numeric(rownames(ages_prop))
  #   ageset<-lset[,c("STRAT","MISSION","SETNO","FLEN","value")]
  #   colnames(ageset)[which(names(ageset) == "value")] <- "CAGE"
  #   ageset<-ageset[order(ageset$STRAT,ageset$MISSION, ageset$SETNO),]
  #   
  #   ages_pre<-merge(ageset,ages_prop, by="FLEN")
  #   ages_pre<-as.data.table(ages_pre)
  #   ages_pre[, (theseages) := lapply(.SD, 
  #                                    function(x) x * ages_pre[['CAGE']] ), 
  #            .SDcols = theseages]
  #   age_by_set<-aggregate(.~STRAT + MISSION + SETNO, data=ages_pre, sum)
  #   age_by_set<-age_by_set[order(age_by_set$STRAT,age_by_set$SETNO),]
  #   age_by_set$FLEN<-NULL 
  #   age_by_set$CAGE<-NULL
  #   
  #   age_mean<-age_by_set
  #   age_mean$SETNO<-NULL
  #   age_mean$MISSION<-NULL
  #   age_mean<-aggregate(.~STRAT, data=age_mean, mean)
  #   setnames(age_mean,
  #            old=names(age_mean[,2:ncol(age_mean)]), 
  #            new=c(paste0("age_",theseages,"_mean")))
  #   
  #   age_mean_se<-age_by_set
  #   age_mean_se$SETNO<-NULL
  #   age_mean_se$MISSION<-NULL
  #   age_mean_se<-aggregate(.~STRAT, data=age_mean_se, st.err)
  #   setnames(age_mean_se,
  #            old=names(age_mean_se[,2:ncol(age_mean_se)]), 
  #            new=c(paste0("age_",theseages,"_se")))
  #   
  #   age_pretotal<-as.data.table(merge(strata.area,age_by_set, by="STRAT"))
  #   age_pretotal$SQNM<-NULL
  #   age_pretotal[, (theseages) := lapply(.SD, 
  #                                        function(x) x * age_pretotal[['TUNITS']] ), 
  #                .SDcols = theseages]
  #   age_pretotal$TUNITS<-NULL
  #   age_total<-age_pretotal
  #   age_total$MISSION<-NULL
  #   age_total$SETNO<-NULL
  #   age_total<-aggregate(.~STRAT, data=age_total, mean)
  #   setnames(age_total,
  #            old=names(age_total[,2:ncol(age_total)]), 
  #            new=c(paste0("age_",theseages,"_tot")))
  #   age_total_se<-age_pretotal
  #   age_total_se$MISSION<-NULL
  #   age_total_se$SETNO<-NULL
  #   age_total_se<-aggregate(.~STRAT, data=age_total_se, st.err)
  #   setnames(age_total_se,
  #            old=names(age_total_se[,2:ncol(age_total_se)]), 
  #            new=c(paste0("age_",theseages,"_tot_se")))
  #   age_by_set.cnt<-aggregate(list(
  #     COUNT=age_by_set$STRAT), 
  #     by=list(STRAT=age_by_set$STRAT), 
  #     FUN=length)
  #   ages<-merge(age_by_set.cnt,age_mean)
  #   ages<-merge(ages,age_mean_se)
  #   ages<-merge(ages,age_total)
  #   ages<-na.zero(merge(ages,age_total_se))
  # } #end of age calculations
  # 
  # input_parameters<-list("stranal version"=paste0( stranal.ver ),
  #                        "Analysis Date"=Sys.time(),
  #                        "Experiment Type"=these.type, 
  #                        "Strata"=these.strat, 
  #                        "Missions"=these.missions,
  #                        "Year"=year,
  #                        "By.Sex"=by.sex,
  #                        "Species"=paste0(species.code, " (",species.name,")"),
  #                        "Wingspread"=wingspread, 
  #                        "Distance"=towdist,
  #                        "Data Source"=agency.gui,
  #                        "ALK Modifications"="No")
  # ################################################################################
  # ###                          RESULTS                                            
  # results<-list(
  #   input_parameters=input_parameters,
  #   strata.areas=strata.areas, 
  #   set_info=set_info,
  #   length_by_set=length_by_set, 
  #   length_mean=length_mean, 
  #   length_mean_se=length_mean_se, 
  #   length_total=length_total, 
  #   length_total_se=length_total_se,
  #   nw=nw,   
  #   weights=weights, #kg
  #   numbers=numbers, 
  #   age.length.key.totals=age.length.key.totals,
  #   age_table=age_table, 
  #   age_length_weight=age_length_weight,
  #   age_by_set=age_by_set,  
  #   ages=ages)
  # return(results)
  }
