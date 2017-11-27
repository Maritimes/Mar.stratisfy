#' @title extractData
#' @description This function doesn't do anything yet.
#' @param requested  The default value is \code{NULL}.  
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom utils select.list
#' @export
extractData<-function(requested = NULL, agency = NULL, spp = NULL, missions = NULL, strata = NULL){
  getCatch<-function(agency, spp, missions, strata){
    if (agency =="DFO"){
        sql <-
          paste("select C.mission,C.setno,C.size_class,C.totwgt,C.sampwgt,C.totno,C.calwt
                from
                groundfish.gscat c,
                groundfish.gsinf i
                where
                C.MISSION = I.Mission AND
                C.SETNO = I.SETNO AND
                C.SPEC =", spp," AND
                C.mission IN (",Mar.utils::SQL_in(missions),") AND 
                I.STRAT IN (",Mar.utils::SQL_in(strata),")
                ORDER BY C.mission,C.setno
                ", sep="")
    }else if (agency=="NMFS"){
        #adding fake values for calwt(0), size_class(1), and sampwgt(0) so data format matches CDN data
        sql <-
          paste("select cruise6 mission,to_number(station) setno, 1 size_class, sum(expcatchwt) totwgt, 0 sampwgt, sum(expcatchnum) totno, 0 calwt
                from
                usnefsc.uss_catch
                WHERE
                to_number(svspp)=", spp,"
                and cruise6 IN (",Mar.utils::SQL_in(missions),")
                AND STRATUM   IN (",Mar.utils::SQL_in(strata),")
                group by
                cruise6, to_number(station)
                ORDER BY cruise6, to_number(station)", sep="")
    }
    raw.gscat<-oracle_cxn$thecmd(oracle_cxn$channel, sql)
    if (nrow(raw.gscat)<1) stop("Error: No catch data can be found for your selection")
    return(raw.gscat)
  }
   getInf<-function(agency){
     if (agency=='DFO'){
       sql<-
         #no area filter??
         paste("select i.mission, i.setno,sdate,time,strat,
          area unit_area,slat,slong,dmin,dmax,depth, dur,dist
          from 
          groundfish.gsinf i 
          where 
          i.MISSION IN (",Mar.utils::SQL_in(.GlobalEnv$str_dfMissions[,1]),")
          AND strat IN (",Mar.utils::SQL_in(.GlobalEnv$str_dfStrata[,1]),")
          AND type IN (", .GlobalEnv$str_type,")
          ORDER BY i.mission, i.setno", sep="")
     }else if (agency=="NMFS"){
       #distance was assumed to be 1.75, but appears to be dopdistb
       sql<-
         paste("SELECT CRUISE6 mission,to_number(station) setno, begin_est_towdate sdate, est_time time, STRATUM strat, 
          area unit_area,  BEGLAT slat, BEGLON slong, mindepth dmin, maxdepth dmax, avgdepth depth, towdur dur, dopdistb dist 
          FROM USNEFSC.USS_STATION 
          WHERE CRUISE6 IN (",Mar.utils::SQL_in(.GlobalEnv$str_dfMissions[,1]),")
          AND STRATUM IN (",Mar.utils::SQL_in(.GlobalEnv$str_dfStrata[,1]),")
          AND to_number(SHG) <= ",.GlobalEnv$str_type," 
          ORDER BY CRUISE6,to_number(station)", sep="")
     }
     raw.gsinf<-oracle_cxn$thecmd(oracle_cxn$channel, sql )
     #if (agency.gui=="NMFS") raw.gsinf$STRAT<-sprintf("%05d", raw.gsinf$STRAT)
      return(raw.gsinf)
      }
   getDet<-function(agency){
     if (agency=='DFO'){
       #       AND i.STRAT IN (",Mar.utils::SQL_in(.GlobalEnv$str_dfStrata[,1]),")
       sql<-paste("select d.mission,d.setno,d.size_class,d.fsex,d.age,d.fwt,         
       decode(",.GlobalEnv$str_dfSpp$LGRP,",1,d.flen,2,.5+2*floor(d.flen/2),3,1+3*floor(d.flen/3),flen) flen,          
       s.lgrp binwidth,s.lfsexed bysex, d.clen          
       from          
       groundfish.gsdet d, groundfish.gsspec s, groundfish.gsinf i 
       WHERE 
       s.spec=d.spec AND  
       d.MISSION = i.MISSION AND
       d.SETNO = i.SETNO AND
       d.MISSION IN (",Mar.utils::SQL_in(.GlobalEnv$str_dfMissions[,1]),") AND 
       d.SPEC=",.GlobalEnv$str_dfSpp[,1]," 
       ORDER BY d.mission,d.setno", sep="")
     raw.gsdet<-oracle_cxn$thecmd(oracle_cxn$channel, sql )
     }else if (agency=="NMFS"){
       #missing fsex, sizeclass,clen
       sql1<- paste("select cruise6 mission, to_number(station) setno, age, length, avg(indwt) fwt,
        decode(", .GlobalEnv$str_dfSpp[,3],",1,length,2,.5+2*floor(length/2),3,1+3*floor(length/3),length) flen
        from usnefsc.uss_detail
        where to_number(svspp)=",.GlobalEnv$str_dfSpp[,1],"
        AND CRUISE6 IN (",Mar.utils::SQL_in(.GlobalEnv$str_dfMissions[,1]),")
        AND STRATUM IN (",Mar.utils::SQL_in(.GlobalEnv$str_dfStrata[,1]),")
        group by cruise6,station,age,length
        ORDER BY cruise6, to_number(station)", sep="")
       raw.gsdet1<-oracle_cxn$thecmd(oracle_cxn$channel, sql1 )
       
       sql2<- paste("select cruise6 mission, catchsex fsex, station setno,length, 
        sum(expnumlen) clen, 1 size_class
        from usnefsc.uss_lengths
        where to_number(svspp)=",.GlobalEnv$str_dfSpp[,1],"
        and cruise6 in (",Mar.utils::SQL_in(.GlobalEnv$str_dfMissions[,1]),")
        and catchsex in ('0','1','2')
        group by cruise6,station,length, catchsex
        ORDER BY cruise6, to_number(station)",sep="")
       raw.gsdet2<-oracle_cxn$thecmd(oracle_cxn$channel, sql2 )
       raw.gsdet<-merge(raw.gsdet1,raw.gsdet2, all.x=T) 
       raw.gsdet$FLEN[is.na(raw.gsdet$FLEN)] <- raw.gsdet$LENGTH[is.na(raw.gsdet$FLEN)]
     }
     return(raw.gsdet)
   }

  switch(requested, 
         "catch" = getCatch(agency,spp,missions, strata), 
         "inf" = getInf(agency),
         "det" = getDet(agency)
  )
}