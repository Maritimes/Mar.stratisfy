#' @title getUserInput
#' @description This function can present the user with a variety of select 
#' boxes from which they can select appropriate options.  The select box that is
#' shown is determined by the value of the paramter \code{"requested"}.
#' Some of the options that are presented are dependent on the existence of 
#' values written to the global environment by the stranal function.
#' @param requested  The default value is \code{NULL}.  
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom utils select.list
#' @importFrom Mar.utils SQL_in
#' @export
getUserInput <-function(requested = NULL){
  getAgency<-function(){
    choice<-select.list(c("DFO","NMFS"), multiple=F, graphics=T, title='Please choose an agency:')
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    return(choice)
  }
  
  getType<-function(){
    if (.GlobalEnv$str_agency =="DFO"){
      choice <- oracle_cxn$thecmd(oracle_cxn$channel, paste("select XTYPEDESC, XTYPE
                                         from groundfish.GSXTYPE
                                          WHERE XTYPE IN (1,5)
                                         ORDER BY XTYPE",sep=""))
      choice<-paste( choice[,1], " (", choice[,2],")",sep="")

      choice<-select.list(choice,
                          multiple=F, graphics=T,
                          title='Please select the type of trawl:')
      choice<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', choice) )
      choice = 1
    }else if (.GlobalEnv$str_agency=="NMFS"){
      choice<-136 
    }
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    return(choice)
  }
  
  getSpp<-function(){
    sex.choice <- select.list(c("Unsexed Analysis","Sexed Analysis"),
                              multiple=F, graphics=T, 
                              title='Analysis by Sex?')
    sex.choice = switch(sex.choice, "Sexed Analysis" = TRUE, "Unsexed Analysis" = FALSE)
    if ((sex.choice=="" || is.na(sex.choice)))stop("\n\nNo selection made - Aborting.")
    
    if (.GlobalEnv$str_agency =="DFO"){
      if (isTRUE(.GlobalEnv$str_sexed)){
        species.query.tweak<-"AND LFSEXED = 'Y' "
      } else{
        species.query.tweak<-""
      }
      species.query=paste("SELECT DISTINCT(SPEC), initcap(CNAME) CNAME, 
                                    LGRP, LFSEXED 
                                    FROM GROUNDFISH.GSSPEC 
                                    WHERE SPEC <> 9999 
                                    ", species.query.tweak, " 
                                    ORDER BY initcap(CNAME)",sep="")
    }else if (.GlobalEnv$str_agency=="NMFS"){
      #not ideal - hard coded spp from uss_catch table with more than 1 catchsex
      if (isTRUE(.GlobalEnv$str_sexed)){
        species.query.tweak<-"AND TO_NUMBER(SPEC) IN (15,22,26,75,108)"
      } else{
        species.query.tweak<-""
      }
      species.query=paste("SELECT TO_NUMBER(US.SPEC) SPEC, INITCAP(US.CNAME) CNAME, 
                                    US.LGRP, US.LFSEXED 
                                    FROM USNEFSC.USSPEC US
                                    WHERE  
                                    US.SPEC <> 9999
                                    ", species.query.tweak,"
                                    ORDER BY initcap(US.CNAME)",sep="")
    }
    allSpp = oracle_cxn$thecmd(oracle_cxn$channel, species.query)
    choiceSpp = select.list(paste( allSpp$CNAME, " (", allSpp$SPEC,")",sep=""),
                multiple=F, graphics=T, 
                title=ifelse(isTRUE(.GlobalEnv$str_sexed),
                             "Choose a (sexed) species:",
                             "Choose a species"))
    #got species selection - extract code
    choiceCode<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', choiceSpp)) 
    spp.choice = allSpp[allSpp$SPEC==choiceCode,]
    if ((spp.choice=="" || is.na(spp.choice)))stop("\n\nNo selection made - Aborting.")
    
    res= list(sex.choice,spp.choice)
    
    return(res)
  }
  getMissionsAndStrata<-function(){
  #1) MISSIONS
    season<-switch(.GlobalEnv$str_agency,
                   "DFO" = c("SPRING","SUMMER", "FALL"),
                   "NMFS"=c("SPRING","SUMMER", "FALL", "WINTER"))
    season <- select.list(season,
                          multiple=F, graphics=T, 
                          title='Season?')
    if (.GlobalEnv$str_agency=="DFO"){
      mission.query = paste0("select DISTINCT M.MISSION
                              from GROUNDFISH.GSMISSIONS M, GROUNDFISH.GSINF I 
                              WHERE 
                              M.MISSION = I.MISSION AND 
                              I.TYPE = ",.GlobalEnv$str_type," AND 
                              M.SEASON = '",season, "'
                              ORDER BY SUBSTR(M.MISSION, 4) DESC")
    } else if (.GlobalEnv$str_agency == "NMFS"){
      mission.query = paste0("SELECT 
      DISTINCT(S.CRUISE6) MISSION, M.SEASON
      FROM 
      USNEFSC.USS_STATION S,
      USNEFSC.USS_MSTR_CRUISE M
      WHERE 
      S.STRATUM IN (
        SELECT DISTINCT STRAT FROM GROUNDFISH.GSSTRATUM
        UNION
        SELECT DISTINCT STRAT FROM USNEFSC.DFO5ZJM
        UNION
        SELECT DISTINCT STRAT FROM USNEFSC.DFO5ZGHNO
        UNION
        SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZJM
        UNION
        SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZGHNO
        UNION
        SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZJMC
        UNION
        SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZJMU
        UNION
        SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZU
      ) AND 
      S.CRUISE6 = M.CRUISE6 AND
      S.SHG <= ",.GlobalEnv$str_type," AND 
      M.SEASON = '",season, "'
      ORDER BY S.CRUISE6")
    }
    cat("\n Looking for Missions meeting your criteria...\n")
    choice = oracle_cxn$thecmd(oracle_cxn$channel, mission.query)
    if (NROW(choice)==0) stop("\n\n!!!ABORTING!!!\nNo Missions can be found where the requested Experiment Type and Season can be found")
    cat("\n Done, Continuing...\n")
    choice = select.list(choice[,1],
                         multiple=T, graphics=T, 
                         title='Please select the desired missions')
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    if (.GlobalEnv$str_agency=="DFO"){
      mission.df.sql=paste("select MISSION, VESEL, CRUNO, YEAR
                                    from groundfish.gsmissions 
                                     WHERE MISSION IN (",Mar.utils::SQL_in(choice),")
                                    ORDER BY MISSION", sep="") 
    } else if (.GlobalEnv$str_agency == "NMFS"){
      mission.df.sql=paste("SELECT CRUISE6 AS MISSION, SVVESSEL AS VESEL, 
                                      CRUISE AS CRUNO, YEAR
                                      FROM USNEFSC.USS_MSTR_CRUISE
                                      WHERE CRUISE6 IN (",Mar.utils::SQL_in(choice),")
                                      ORDER BY CRUISE6", sep="")
    }
    str_dfMissions<-oracle_cxn$thecmd(oracle_cxn$channel,mission.df.sql)
    #1) STRATA
    if (.GlobalEnv$str_agency=="DFO"){
      sql1 = paste0("SELECT DISTINCT STRAT FROM GROUNDFISH.GSINF WHERE MISSION IN (",Mar.utils::SQL_in(str_dfMissions[,1]),") ORDER BY STRAT")
    } else if (.GlobalEnv$str_agency == "NMFS"){
      sql1 = paste0("SELECT DISTINCT STRATUM STRAT FROM USNEFSC.USS_STATION WHERE CRUISE6 IN (",Mar.utils::SQL_in(str_dfMissions[,1]),") ORDER BY STRATUM")
    }
    availStrata = oracle_cxn$thecmd(oracle_cxn$channel,  sql1)
    
    res = list(str_dfMissions, availStrata)
    return(res)
  }
  getStrataTable<-function(){
    #check which tables contain strata like the ones we got from the mission
    strataTableSql = paste0("
    SELECT SRC, cnt FROM (
      SELECT 'GROUNDFISH.GSSTRATUM' SRC, COUNT(*) cnt FROM GROUNDFISH.GSSTRATUM WHERE STRAT IN (",Mar.utils::SQL_in(.GlobalEnv$str_dfMissionsStrata[,1]),")
      UNION
      SELECT 'USNEFSC.DFO5ZJM' SRC, COUNT(*) cnt FROM USNEFSC.DFO5ZJM WHERE STRAT IN (",Mar.utils::SQL_in(str_dfMissionsStrata[,1]),")
      UNION
      SELECT 'USNEFSC.DFO5ZGHNO' SRC, COUNT(*) cnt FROM USNEFSC.DFO5ZGHNO WHERE STRAT IN (",Mar.utils::SQL_in(str_dfMissionsStrata[,1]),")
      UNION
      SELECT 'USNEFSC.NMFS5ZJM' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZJM WHERE STRAT IN (",Mar.utils::SQL_in(str_dfMissionsStrata[,1]),")
      UNION
      SELECT 'USNEFSC.NMFS5ZGHNO' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZGHNO WHERE STRAT IN (",Mar.utils::SQL_in(str_dfMissionsStrata[,1]),")
      UNION
      SELECT 'USNEFSC.NMFS5ZJMC' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZJMC WHERE STRAT IN (",Mar.utils::SQL_in(str_dfMissionsStrata[,1]),")
      UNION
      SELECT 'USNEFSC.NMFS5ZJMU' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZJMU WHERE STRAT IN (",Mar.utils::SQL_in(str_dfMissionsStrata[,1]),")
      UNION
      SELECT 'USNEFSC.NMFS5ZU' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZU WHERE STRAT IN (",Mar.utils::SQL_in(str_dfMissionsStrata[,1]),")
    )
    WHERE cnt >0 ORDER BY CNT DESC") 
    missionsStrataTables<-oracle_cxn$thecmd(oracle_cxn$channel, strataTableSql)[,1]
    if (length(missionsStrataTables)>1){
      choice<-select.list(missionsStrataTables, 
                          multiple=F, graphics=T,
                          title='Please choose a stratum table:')
    } else if (length(missionsStrataTables)==1) {
      choice<-missionsStrataTables
    }else{
      stop("\n\nNo strata tables work with your selections")
    }
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    return(choice)
  }
  
  getStrata<-function(){
    
    if (all(nchar(.GlobalEnv$str_dfMissionsStrata[,1])==5)){
       strata.tweak = "AND LENGTH(STRAT)=5"
       strata.preselect=""
     }else if (all(nchar(.GlobalEnv$str_dfMissionsStrata[,1])==3)){
       strata.tweak = paste0("AND LENGTH(STRAT) = 3 AND SUBSTR(STRAT,1,1) IN ('3','4','5')")
       strata.preselect=as.character(c(440:495))
     }else{
       print("\nWeird strata encountered please report your selections to Mike.McMahon@dfo-mpo.gc.ca")
     }
    sql = paste0("SELECT * FROM ",.GlobalEnv$str_strataTable, " WHERE 1=1 ", strata.tweak, " ORDER BY STRAT")
    all.strat<-oracle_cxn$thecmd(oracle_cxn$channel, sql)
    choice<-select.list(all.strat$STRAT,
                        multiple=T, graphics=T, preselect = strata.preselect,
                        title='Please choose the strata:')
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    all.strat = all.strat[all.strat$STRAT %in% choice,]
    
    #calculate strat areas into tunits; US nautical mile is 6080.2ft
    sql2<- paste("SELECT strat, area SQNM, 
                  nvl(area,0)/(",.GlobalEnv$str_towDist,"*(",.GlobalEnv$str_wingspread,"/6080.2)) tunits 
          FROM ",.GlobalEnv$str_strataTable,"
          WHERE 
          strat IN (",Mar.utils::SQL_in(all.strat[,1]),")
           ORDER BY strat", sep="")
    all.strat.det<-oracle_cxn$thecmd(oracle_cxn$channel, sql2)
    if (.GlobalEnv$str_agency=="NMFS") all.strat.det$STRAT<-sprintf("%05d", all.strat.det$STRAT)
    all.strat.det= merge(all.strat, all.strat.det)
    all.strat.det<-all.strat.det[order(all.strat.det$STRAT),] 
    return(all.strat.det)
  }
  getWingspread<-function(){
    wingspread<-switch(.GlobalEnv$str_agency,
                   "DFO" = c("41","34"),
                   "NMFS"=c("34","36","41"))
    wingspread.pre<-switch(.GlobalEnv$str_agency,
                       "DFO" = c("41"),
                       "NMFS"=c("34"))
    choice =  as.numeric(select.list(wingspread,
                                     preselect=wingspread.pre,
                                     multiple=F, graphics=T, 
                                     title='Select the Wingspread (ft)'))
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    return (choice)
  }
  getTowDist<-function(){
    choice = select.list(c("1.75","other"),
                                         preselect=c("1.75"),
                                         multiple=F, graphics=T, 
                                         title='Tow Distance (NM)')
    if (choice != "1.75") {
      choice = as.numeric(readline(prompt =
                                  "Please enter the tow distance you want (NM):"))
      print(choice)
    }
    choice=as.numeric(choice)
    if (choice=="" || is.na(choice))stop("\n\nNo selection made - Aborting.")
    return(choice)
  }
  
  switch(requested, 
                    "agency" = getAgency(), 
                    "type" = getType(),
                    "spp" = getSpp(),
                    "missionsAndStrata" = getMissionsAndStrata(),
                    "strataTable" = getStrataTable(),
                    "strata" = getStrata(),
                    "wingspread" = getWingspread(),
                    "towDist" = getTowDist()
                    )
}