#' @title getUserInput
#' @description This function can present the user with a variety of select 
#' boxes from which they can select appropriate options.  The select box that is
#' shown is determined by the value of the paramter \code{"requested"}.
#' Some of the options that are presented are dependent on the existence of 
#' values written to the global environment by the stranal function.
#' @param requested   The default value is \code{NULL}. This determines which 
#' picklist will be invoked.
#' @param agency   The default value is \code{NULL}.  Valid values are 'DFO' and
#'  'NMFS'.
#' @param type   The default value is \code{NULL}.  Setting to \code{NULL} will 
#' result in a pick list.
#' @param strataTable   The default value is \code{NULL}.  Setting to 
#' \code{NULL} will result in a pick list.
#' @param year   The default value is \code{NULL}.  Setting to \code{NULL} will 
#' result in a pick list.
#' @param season   The default value is \code{NULL}.  Setting to \code{NULL} 
#' will result in a pick list.
#' @param wingspread   The default value is \code{NULL}.  Setting to \code{NULL} 
#' will result in a pick list.
#' @param towDist  The default value is \code{NULL}.  Setting to \code{NULL} 
#' will result in a pick list.
#' @param spp   The default value is \code{NULL}.  Setting to \code{NULL} will 
#' result in a pick list.
#' @param bySex   The default value is \code{NULL}.  Setting to \code{NULL} will 
#' result in a pick list.
#' @param strata   The default value is \code{NULL}.  Setting to \code{NULL} 
#' will result in a pick list.
#' @param dfMissionsStrata   The default value is \code{NULL}.  Setting to 
#' \code{NULL} will result in a pick list.
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom utils select.list
#' @importFrom Mar.utils SQL_in
getUserInput <-function(requested = NULL, agency = NULL, type = NULL, 
                        strataTable = NULL, year = NULL, season = NULL, 
                        wingspread = NULL, towDist = NULL, spp = NULL, 
                        bySex = NULL, strata = NULL, dfMissionsStrata = NULL){
  getAgency<-function(agency){
    if(agency %in% c("DFO","NMFS"))return(agency)
    choice<-select.list(c("DFO","NMFS"), multiple=F, graphics=T, title='Please choose an agency:')
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    return(choice)
  }
  
  getWingspread<-function(agency, wingspread){
    availWingspread<-switch(agency,
                            "DFO" = c("41","34"),
                            "NMFS"=c("34","36","41"))
    availWingspreadPre<-switch(agency,
                            "DFO" = c("41"),
                            "NMFS"=c("34"))
    if (wingspread %in% availWingspread){
      return(as.numeric(wingspread)) 
    }else{
      choice<-NA
      while(is.na(choice)){
        choice =  as.numeric(select.list(availWingspread,
                                         preselect=availWingspreadPre,
                                         multiple=F, graphics=T, 
                                         title='Select the Wingspread (ft)'))
        if (is.na(choice)) print("You must select a wingspread")
        return (choice)
      }
    }
  }
  
  getTowDist<-function(towDist){
    if (nchar(towDist)>0){
     if (as.numeric(towDist)>0 & as.numeric(towDist)<100 ) return(towDist) 
    }
    towDistPick<-NA
    while(is.na(towDistPick)){
    towDistPick = select.list(c("1.75","other"),
                         preselect=c("1.75"),
                         multiple=F, graphics=T, 
                         title='Tow Distance (NM)')
    if (is.na(towDistPick)) print("You must select one of provided options")
    }
    if (towDistPick == "other") {
      towDistPick = as.numeric(readline(prompt =
                                     "Please enter the tow distance you want (NM):"))
      print(towDistPick)
    }
    towDistPick=as.numeric(towDistPick)
    return(towDistPick)
    }

  
  
  
  getType<-function(agency, type){
    if (agency == "NMFS"){
      typePick<-NA
      if (is.numeric(type))typePick<-type
      while(is.na(typePick)){
        typePick = select.list(c("136","other"),
                                  preselect=c("136"),
                                  multiple=F, graphics=T, 
                                  title='Set Type')
        if (is.na(typePick)) print("You must select one of provided options")
      }
      if (typePick == "other") {
        typePick = as.numeric(readline(prompt =
"Any experiment type greater than or equal to the value will be included in 
the analysis. The default is <= 136. This may be changed as required. Used in 
the queries as: *.SHG <= 136

Please enter the survey type:"))
        print(typePick)
      }
      typePick=as.numeric(typePick)
      return(typePick)
    }else if (agency == "DFO"){
      if (type %in% c(1,5)){
        return(as.numeric(type)) 
      }else {
        choice <-NA
        while(is.na(choice)){
          choice <- oracle_cxn$thecmd(oracle_cxn$channel, paste("select XTYPEDESC, XTYPE
                                             from groundfish.GSXTYPE
                                              WHERE XTYPE IN (1,5)
                                             ORDER BY XTYPE",sep=""))
          choice<-paste( choice[,1], " (", choice[,2],")",sep="")
          
          choice<-select.list(choice,
                              multiple=F, graphics=T,
                              title='Please select the type of trawl:')
          choice<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', choice) )
          if (is.na(choice)) print("You must select a survey type")
        }
        return(choice)
      }
    }
  }
  #agency, type, year, season
  getMissionsAndStrata<-function(agency=NULL, type=NULL, year=NULL, season=NULL){
    if (!is.null(year)) year = as.character(year)
    if (is.null(season))season = ""
    seasonpick <-NA
    if (agency == "DFO"){
      availSeasons = c("SPRING","SUMMER", "FALL")
      if (season %in% availSeasons){
        seasonpick = season
      }
    }else if (agency == "NMFS"){
      availSeasons = c("SPRING","SUMMER", "FALL", "WINTER")
      if (season %in% availSeasons){
        seasonpick = season
      }
    }
    while(is.na(seasonpick) | !(seasonpick %in% availSeasons)){
      seasonpick <- select.list(availSeasons,
                                multiple=F, graphics=T, 
                                title='Season?')
      if (!seasonpick %in% availSeasons) print("You must select a season")
    }
    
    yearpick <-NA
    if (agency == "DFO"){
      year.query = paste0("select DISTINCT M.YEAR
                            from GROUNDFISH.GSMISSIONS M, GROUNDFISH.GSINF I 
                            WHERE 
                            M.MISSION = I.MISSION AND 
                            I.TYPE = ",type," AND
                            M.SEASON = '",seasonpick, "'")
    } else if (agency == "NMFS"){
      year.query = paste0("SELECT 
                             DISTINCT M.YEAR
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
                             S.SHG <= 136 AND 
                             M.SEASON = 'SUMMER'")
      
    }
    cat("\n Hmmm. Let me see what years are available that match your criteria...\n")
    availYears = oracle_cxn$thecmd(oracle_cxn$channel, year.query)
    availYears = as.character(availYears[order(availYears$YEAR),1])
    if (nchar(year)>0){
      if (year %in% availYears){
        yearpick = year
      }else {
        cat("\n***\nYou specified a year that is not available given your other criteria\
Please choose a different year, or check your parameters\n***\n")
      }
    }
    
    while(is.na(yearpick) | !(yearpick %in% availYears)){
      yearpick <- select.list(availYears,
                              multiple=F, graphics=T, 
                              title='Year?')
      if (!yearpick %in% availYears) print("You must select a year")
    }
    
    if (agency == "DFO"){
      mission.query = paste0("select DISTINCT M.MISSION
                             from GROUNDFISH.GSMISSIONS M, GROUNDFISH.GSINF I 
                             WHERE 
                             M.MISSION = I.MISSION AND 
                             I.TYPE = ",type," AND 
                             M.YEAR = ",yearpick," AND
                             M.SEASON = '",seasonpick, "'
                             ORDER BY SUBSTR(M.MISSION, 4) DESC")
    } else if (agency == "NMFS"){
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
                             S.SHG <= ",type," AND 
                             M.SEASON = '",seasonpick, "' AND 
                             YEAR = '",yearpick, "'
                             ORDER BY S.CRUISE6")
    }
    cat("\n Looking for Missions meeting your criteria...\n")
    availMissions = oracle_cxn$thecmd(oracle_cxn$channel, mission.query)
    
    missionPick<-NA 
    if (NROW(availMissions)==0){
      stop("\n\n!!!ABORTING!!!\nNo Missions can be found where the requested Experiment Type and Season can be found")
    } else if (NROW(availMissions)==1){
      missionPick=availMissions[,1]
      cat(paste0("\nUsing ",missionPick," - the only  mission matching your criteria\n")) 
    }else {
      while(is.na(missionPick[1])){
        missionPick <- select.list(availMissions[,1],preselect=availMissions[,1],
                                   multiple=T, graphics=T, 
                                   title='Mission?')
        if (!all(missionPick %in% availMissions$MISSION)) {
          cat("You must select a mission")
          missionPick<-NA
        }
      }
    }
    #1) STRATA
    if (agency == "DFO"){
      sql1 = paste0("SELECT DISTINCT STRAT FROM GROUNDFISH.GSINF WHERE MISSION IN (",Mar.utils::SQL_in(missionPick),") ORDER BY STRAT")
    } else if (agency == "NMFS"){
      sql1 = paste0("SELECT DISTINCT STRATUM STRAT FROM USNEFSC.USS_STATION WHERE CRUISE6 IN (",Mar.utils::SQL_in(missionPick),") ORDER BY STRATUM")
    }
    availStrata = oracle_cxn$thecmd(oracle_cxn$channel,  sql1)
    res = list(missionPick, availStrata)
    return(res)
  }
  
  getStrataTable<-function(strataTable, dfMissionsStrata){
    strataTablePick<-NA 
    availStrataTables<- c("GROUNDFISH.GSSTRATUM","USNEFSC.DFO5ZJM",
                          "USNEFSC.DFO5ZGHNO","USNEFSC.NMFS5ZJM",
                          "USNEFSC.NMFS5ZGHNO","USNEFSC.NMFS5ZJMC",
                          "USNEFSC.NMFS5ZJMU","USNEFSC.NMFS5ZU")
    if (nchar(strataTable)>0){
      if (strataTable %in% availStrataTables){
        strataTablePick = strataTable
      }
      } else {
        stratList = Mar.utils::SQL_in(dfMissionsStrata[,1])
        #check which tables contain strata like the ones we got from the mission
        strataTableSql = paste0("
                            SELECT SRC, cnt FROM (
                            SELECT 'GROUNDFISH.GSSTRATUM' SRC, COUNT(*) cnt FROM GROUNDFISH.GSSTRATUM WHERE STRAT IN (",stratList,")
                            UNION
                            SELECT 'USNEFSC.DFO5ZJM' SRC, COUNT(*) cnt FROM USNEFSC.DFO5ZJM WHERE STRAT IN (",stratList,")
                            UNION
                            SELECT 'USNEFSC.DFO5ZGHNO' SRC, COUNT(*) cnt FROM USNEFSC.DFO5ZGHNO WHERE STRAT IN (",stratList,")
                            UNION
                            SELECT 'USNEFSC.NMFS5ZJM' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZJM WHERE STRAT IN (",stratList,")
                            UNION
                            SELECT 'USNEFSC.NMFS5ZGHNO' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZGHNO WHERE STRAT IN (",stratList,")
                            UNION
                            SELECT 'USNEFSC.NMFS5ZJMC' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZJMC WHERE STRAT IN (",stratList,")
                            UNION
                            SELECT 'USNEFSC.NMFS5ZJMU' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZJMU WHERE STRAT IN (",stratList,")
                            UNION
                            SELECT 'USNEFSC.NMFS5ZU' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZU WHERE STRAT IN (",stratList,")
                            )
                            WHERE cnt >0 ORDER BY CNT DESC") 
        availStrataTables<-oracle_cxn$thecmd(oracle_cxn$channel, strataTableSql)
        
        
        if (NROW(availStrataTables)==0){
          stop("\n\n!!!ABORTING!!!\nNo Strata Tables meet your criteria")
        } else if (NROW(availStrataTables)==1){
          strataTablePick=availStrataTables[,1]
          cat(paste0("\nUsing ",strataTablePick," - the only  strata table matching your criteria\n")) 
        }else {
          while(is.na(strataTablePick)){
            if (!strataTablePick %in% availStrataTables) print("You must select a strata table (the number in brackets shows how many of your strata are present in the table)")
            
            strataTablePick <- select.list(paste0(availStrataTables[,1]," (",availStrataTables[,2],")"),multiple=F, graphics=T, 
                                           title='Strata Table?')
            strataTablePick = gsub( " *\\(\\d{1,3}\\) *", "", strataTablePick)
          }
        }
    }
    return(strataTablePick)
  }
  
  
  getSpp<-function(agency, spp, bySex){
    sexChoice<-NA
    sppChoice<-NA
    if (nchar(bySex)>0){
      sexChoice <- bySex
    }else{
    while (is.na(sexChoice)){
      sexChoice <- select.list(c("Unsexed Analysis","Sexed Analysis"),
                              multiple=F, graphics=T, 
                              title='Analysis by Sex?')
    }
      sexChoice = switch(sexChoice, "Sexed Analysis" = TRUE, "Unsexed Analysis" = FALSE)
    }
    rm(bySex)
    sexAgePick<-NA
    if (agency =="DFO"){
      if (isTRUE(sexChoice)){
            while (is.na(sexAgePick)){
              sexAgePick1 = select.list(c("Show Age Results By Sex",
                                          "Combine Sexes in Age Results"),
                                        multiple=F, graphics=T, 
                                        title="How to Handle Sex In Age Results?")
              sexAgePick <- switch(sexAgePick1,
                                   "Show Age Results By Sex" = TRUE, 
                                   "Combine Sexes in Age Results" = FALSE)
            }
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
    }else if (agency=="NMFS"){
      #not ideal - hard coded spp from uss_catch table with more than 1 catchsex
      if (isTRUE(sexChoice)){
        while (is.na(sexAgePick)){
          sexAgePick1 = select.list(c("Show Age Results By Sex",
                                      "Combine Sexes in Age Results"),
                                    multiple=F, graphics=T, 
                                    title="How to Handle Sex In Age Results?")
          sexAgePick <- switch(sexAgePick1,
                               "Show Age Results By Sex" = TRUE, 
                               "Combine Sexes in Age Results" = FALSE)
        }
        
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
    availSpp = oracle_cxn$thecmd(oracle_cxn$channel, species.query)
    if (nchar(spp)>0){
      if (spp %in% availSpp$SPEC){
        sppChoice <- spp
      }else{
        cat("\nThe provided species code is unknown, or not available for the selected 
sex option.  Please select one from the list.\n")
        sppChoice <-NA
      }
    } 

    while (is.na(sppChoice)){
      sppChoice = select.list(paste( availSpp$CNAME, " (", availSpp$SPEC,")",sep=""),
                            multiple=F, graphics=T, 
                            title=ifelse(isTRUE(sexChoice),
                                         "Choose a (sexed) species:",
                                         "Choose a species"))
    #got species selection - extract code
      sppChoice<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', sppChoice)) 

    }
    sppChoice = availSpp[availSpp$SPEC %in% sppChoice,]
    res= list(sexChoice,sppChoice,sexAgePick)
    return(res)
  }
 
  getStrata<-function(agency, strataTable, strata, towDist, wingspread, dfMissionsStrata){

    strataPick<-NA
    if (!is.null(strata)) strataPick <-strata
    if (all(nchar(dfMissionsStrata[,1])==5)){
      strata.tweak = "AND LENGTH(STRAT)=5"
      strata.preselect=""
    }else if (all(nchar(dfMissionsStrata[,1])==3)){
      strata.tweak = paste0("AND LENGTH(STRAT) = 3 AND SUBSTR(STRAT,1,1) IN ('3','4','5')")
      strata.preselect=as.character(c(440:495))
    }else{
      print("\nWeird strata encountered please report your selections to Mike.McMahon@dfo-mpo.gc.ca")
    }
    sql = paste0("SELECT * FROM ",strataTable, " WHERE 1=1 ", strata.tweak, " ORDER BY STRAT")
    availStrat<-oracle_cxn$thecmd(oracle_cxn$channel, sql)
    while(all(!strataPick %in% availStrat$STRAT)){
      strataPick <- select.list(availStrat$STRAT,
                                multiple=T, graphics=T, preselect = strata.preselect,
                                title='Please choose the strata:')
      if (all(!strataPick %in% availStrat$STRAT)) print("You must select the strata")
    }

    dfStrata = availStrat[availStrat$STRAT %in% strataPick,]
    #calculate strat areas into tunits; US nautical mile is 6080.2ft
    sql2<- paste("SELECT strat, area SQNM, 
                  nvl(area,0)/(",towDist,"*(",wingspread,"/6080.2)) tunits 
          FROM ",strataTable,"
          WHERE 
          strat IN (",Mar.utils::SQL_in(dfStrata[,1]),")
           ORDER BY strat", sep="")
    dfStrata.det<-oracle_cxn$thecmd(oracle_cxn$channel, sql2)
    if (agency=="NMFS") dfStrata.det$STRAT<-sprintf("%05s", dfStrata.det$STRAT)
    dfStrata.det= merge(dfStrata, dfStrata.det)
    dfStrata.det<-dfStrata.det[order(dfStrata.det$STRAT),] 
    return(dfStrata.det)
  }
 
  switch(requested, 
         "agency" = getAgency(agency), 
         "type" = getType(agency, type),
         "spp" = getSpp(agency, spp, bySex),
         "sexAge"= getSexAge(bySex),
         "missionsAndStrata" = getMissionsAndStrata(agency, type, year, season),
         "strataTable" = getStrataTable(strataTable, dfMissionsStrata),
         "strata" = getStrata(agency, strataTable, strata, towDist, wingspread, dfMissionsStrata),
         "wingspread" = getWingspread(agency,wingspread),
         "towDist" = getTowDist(towDist)
  )
}