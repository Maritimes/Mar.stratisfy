#' @title stranal
#' @description STRANAL is a program capable of Stratified Analysis of both DFO 
#' and NMFS survey data. STRANAL exports all results to Excel. 
#' 
#' STRANAL works by opening a connection to the PTRAN database. It receives the 
#' requested information through standard SQL queries that are generated from 
#' user selections through the User Interface. That information is then 
#' processed and a Stratified Analysis is created, which is exported to Excel. 
#' 
#' STRANAL requires an Oracle account on the PTRAN database. New Oracle accounts 
#' are requested by filling out a computer account request form. These forms may 
#' be obtained from IM&TS. Select access to the DFO and NMFS data is provided by 
#' the respective datasets manager.
#' @param usepkg  The default value is \code{'roracle'}, but \code{'rodbc'} is works as well.
#' This describes the R package you use to connect to Oracle.  
#' @param agency  The default value is \code{DFO}, the other option is 
#' \code{'NMFS'}.  Setting to \code{NULL} will result in a pick list.
#' @param type The default value is \code{1}.  For agency=DFO, 5 is also 
#' acceptable. For agency = NMFS, 136 is probable, but you can type any integer.
#' The typed number will be embedded in \code{USNEFSC.USS_STATION.SHG <= type}.
#' Setting to \code{NULL} will result in a pick list.
#' @param year The default value is \code{2017}. Setting to \code{NULL} will 
#' result in a pick list.
#' @param season The default value is \code{"SUMMER"}. Setting to \code{NULL} 
#' will result in a pick list.
#' @param strataTable The default value is \code{"GROUNDFISH.GSSTRATUM"}. 
#' Setting to \code{NULL} will result in a pick list.
#' @param wingspread The default value is \code{41}. Setting to \code{NULL} will 
#' result in a pick list.
#' @param towDist The default value is \code{1.75}. Setting to \code{NULL} will 
#' result in a pick list.
#' @param strata The default value is \code{c(440:495)}. Setting to \code{NULL} 
#' will result in a pick list.
#' @param spp The default value is \code{2526}. Setting to \code{NULL} will 
#' result in a pick list.
#' @param bySex The default value is \code{TRUE}. Setting to \code{NULL} will 
#' result in a pick list.
#' @param ageBySex The default value is \code{TRUE}. Note that APL Stranal 
#' ignored sex for the results of 'age by set', 'age mean', 'age total' etc.  
#' Setting this to FALSE will emulate the APL stranal results.
#' @param output The default value is \code{'new'}.  This determines the format of 
#' the output Excel file. Setting to \code{'classic'} will emulate the
#' original APL STRANAL results, including overriding your parameter for 
#' \code{ageBySex} and forcing it to FALSE.  If no excel output is desired, set
#' this parameter to an empty string \code{''}
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}l
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom stats aggregate
#' @importFrom Mar.utils make_oracle_cxn
#' @importFrom Mar.utils SQL_in
#' @importFrom Mar.utils st_err
#' @importFrom openxlsx createWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeDataTable
#' @importFrom openxlsx saveWorkbook
#' @export
#' @note Reports of errors and/or recommendations for improvements should be 
#' addressed to:
#' Mike McMahon (Mike.McMahon@dfo-mpo.gc.ca)
#' Fisheries and Oceans Canada
#'
#' @note Fun Fact!  Stranal does funny things with berried females - when 
#' analyses are done by sex, berried females (i.e. 3), are ignored; and when 
#' analysis is not done by sex, berried females are included. Such inconsistency 
#' seems suboptimal, and is retained for now such that APL and R versions of the 
#' application can be compared. 

stranal<-function(usepkg = "roracle", 
                  agency = "",
                  type = "",
                  year = "",
                  season = "",
                  strataTable = "",
                  wingspread = "",
                  towDist = "",
                  strata = "",
                  spp = "",
                  bySex = "",
                  ageBySex = "",
                  output = "new"
                  ){
  
  assign("oracle_cxn", Mar.utils::make_oracle_cxn(usepkg), envir = .GlobalEnv )
  
  agency = getUserInput("agency",agency=agency)
  type = getUserInput("type", agency=agency, type=type)
  missionsAndStrata = getUserInput("missionsAndStrata", agency=agency,type=type, year=year, season=season)
    dfMissions = missionsAndStrata[[1]]
    dfMissionsStrata = missionsAndStrata[[2]]
    rm(missionsAndStrata)

  strataTable = getUserInput("strataTable", strataTable=strataTable, dfMissionsStrata=dfMissionsStrata)

  wingspread = getUserInput("wingspread", agency=agency, wingspread=wingspread)
  towDist = getUserInput("towDist", towDist=towDist)

  dfStrata = getUserInput("strata",agency=agency, strataTable=strataTable, strata = strata, 
                        dfMissionsStrata=dfMissionsStrata, towDist=towDist, 
                        wingspread=wingspread)

  spp = getUserInput("spp", agency = agency, spp=spp, bySex = bySex, 
                     ageBySex = ageBySex)
  bySex = spp[[1]]
  dfSpp = spp[[2]]
  ageBySex = spp[[3]]
    rm(spp)

  dfRawCatch <- extractData('catch', agency=agency, dfSpp=dfSpp, missions=dfMissions, strata = dfStrata$STRAT)
  dfRawInf <- extractData('inf', agency=agency, missions=dfMissions, strata = dfStrata$STRAT, type=type)
  dfRawDet <- extractData('det', agency=agency, missions=dfMissions, strata = dfStrata$STRAT, dfSpp = dfSpp, bySex = bySex, type=type)
  

  dfNWSets <- calcNumsWeights('sets',dfRawCatch=dfRawCatch,dfRawInf=dfRawInf, towDist=towDist)
  dfNWAgg <- calcNumsWeights('setsAgg', dfNWSets=dfNWSets, dfStrata=dfStrata)
  

  dfStrata <- merge(dfStrata, calcNumsWeights('strataProp', dfNWSets=dfNWSets, 
                                             dfStrata=dfStrata, dfNWAgg=dfNWAgg), all.x=T)
    allStrat = as.data.frame(dfStrata[,"STRAT"])
    colnames(allStrat)<-"STRAT"
  
  lengthsData <-calcAgeLen('lengths', agency = agency, dfNWSets=dfNWSets, dfRawDet=dfRawDet, 
                dfRawInf=dfRawInf, dfStrata=dfStrata, dfSpp=dfSpp, 
                towDist=towDist, bySex = bySex)
    agelen<-lengthsData$agelen
    lengthsTotals<-lengthsData$length_total
    lset = lengthsData$lset
  
  ageLengthKey <-calcAgeLen('ageKey', agelen=agelen, dfSpp=dfSpp, lengthsTotals 
                             = lengthsTotals, lset = lset, dfStrata=dfStrata, 
                            bySex = bySex, output = output, ageBySex = ageBySex)
  lengthsData$agelen<-NULL
  lengthsData$lset<-NULL
  metadata=list(
                "Mar.stranal" = utils::packageDescription('Mar.stranal')$Version,
                "Date" = as.character(Sys.time()),
                "Data Source" = agency,
                "Missions" = paste("'", paste(dfMissions, collapse="','"),"'", sep=""),
                "Strata" = paste("'", paste(allStrat[,1], collapse="','"),"'", sep=""),
                "Species" = paste0(dfSpp$CNAME, " (", dfSpp$SPEC ,")"),
                "By Sex" = bySex,
                "Distance" = towDist,
                "Spread" = wingspread,
                "Stratum Area Table" =  strataTable,
                "Experiment Type" = type,
                "ALK Modifications" = 'Not implemented yet')
  
  nwData <- merge(allStrat, dfNWAgg, all.x = TRUE)
    nwData[is.na(nwData)]<-0

    nwData = rbind(nwData,c("STRAT"="TOTAL",colSums(nwData[,!(colnames(nwData) =="STRAT")])))
    
  dfNWSets = merge(cbind("STRAT"= dfStrata$STRAT),dfNWSets,all.x=T)
    dfNWSets[is.na(dfNWSets)]<-0
    
  length_by_strat_mean = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_by_strat_mean,all.x=T)
    length_by_strat_mean[is.na(length_by_strat_mean)]<-0
    lengthsData$length_by_strat_mean<-length_by_strat_mean
    
  length_by_strat_se = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_by_strat_se,all.x=T)
    length_by_strat_se[is.na(length_by_strat_se)]<-0
    lengthsData$length_by_strat_se<-length_by_strat_se
    
  length_total = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_total,all.x=T)
    length_total[is.na(length_total)]<-0
    #add Column Totals
    length_total = rbind(length_total,c("STRAT"="TOTAL",colSums(length_total[,!(colnames(length_total) =="STRAT")])))
    lengthsData$length_total<-length_total
  length_total_se = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_total_se,all.x=T)
    length_total_se[is.na(length_total_se)]<-0
    lengthsData$length_total_se<-length_total_se
    
  weight_by_set = merge(dfRawInf[,c("STRAT","MISSION","SETNO")], dfNWSets[,c("STRAT","MISSION","SETNO", "TOTWGT")], all.x=TRUE)

  if (class(ageLengthKey) == "list"){
  age_total = ageLengthKey$age_total
    age_total = rbind(age_total,c("STRAT"="TOTAL",colSums(age_total[,!(colnames(age_total) =="STRAT")])))
    ageLengthKey$age_total<-age_total
  }  
  #Add the totals to Strata for approp columns
  dfStrataDataCols =c("SQNM","TUNITS","AREAPROP","AREAPROPSTERR","AREATOT","AREATOTSTERR")
  dfStrataDataTots = colSums(dfStrata[,dfStrataDataCols],na.rm = TRUE)
  names(dfStrataDataTots) <-dfStrataDataCols
  dfStrataOthCols = rep(NA, length(dfStrata[,!(colnames(dfStrata) %in% dfStrataDataCols)]))
  names(dfStrataOthCols) <- colnames(dfStrata[,!(colnames(dfStrata) %in% dfStrataDataCols)])
  totals= data.frame(t(c(dfStrataDataTots,dfStrataOthCols)))
  dfStrata = rbind(dfStrata,totals)  

  
  res=list(metadata = metadata,
           strataInfo = dfStrata,
           nwInfo = nwData,
           lengthInfo = lengthsData,
           ageInfo = ageLengthKey
  )
  wbName = "Mar_stranal.xlsx"
  md = data.frame(unlist(metadata))
  colnames(md)<-"Value"
  wb<-createWorkbook(creator = paste0("Mar.stranal v.",metadata$Mar.stranal))
  sheet1 <- addWorksheet(wb, sheetName = "QUERY")
  writeDataTable(wb, x=data.frame(md), rowNames = TRUE, sheet = sheet1, withFilter = FALSE)
  if (output=="classic"){
   
    
  
      sheet2 <- addWorksheet(wb, sheetName = "Strata Area")
        writeDataTable(wb, x=dfStrata[,c("STRAT","TUNITS","SQNM")], rowNames = FALSE, sheet = sheet2)
      sheet3 <- addWorksheet(wb, sheetName = "Prop Area")
        writeDataTable(wb, x=dfStrata[,c("STRAT","AREAPROP")], rowNames = FALSE, sheet = sheet3)
      sheet4 <- addWorksheet(wb, sheetName = "Prop Area Standard Error")
        writeDataTable(wb, x=dfStrata[,c("STRAT","AREAPROPSTERR")], rowNames = FALSE, sheet = sheet4)
      sheet5 <- addWorksheet(wb, sheetName = "Total Area")
        writeDataTable(wb, x=dfStrata[,c("STRAT","AREATOT")], rowNames = FALSE, sheet = sheet5)
      sheet6 <- addWorksheet(wb, sheetName = "Total Area Standard Error")
        writeDataTable(wb, x=dfStrata[,c("STRAT","AREATOTSTERR")], sheet = sheet6)
        if (class(ageLengthKey) == "list"){
          sheet7 <- addWorksheet(wb, sheetName = "Age Length Key")
            writeDataTable(wb, x=data.frame(ageLengthKey$alk), rowNames = TRUE, sheet = sheet7)
          sheet8 <- addWorksheet(wb, sheetName = "Age Table")
            writeDataTable(wb, x=ageLengthKey$age_table, rowNames = TRUE, sheet = sheet8)
          sheet9 <- addWorksheet(wb, sheetName = "Age Length Weight")
            writeDataTable(wb, x=ageLengthKey$alw, rowNames = TRUE, sheet = sheet9)
          sheet15 <- addWorksheet(wb, sheetName = "Age By Set")
            writeDataTable(wb, x=ageLengthKey$age_by_set, rowNames = FALSE, sheet = sheet15)
          sheet16 <- addWorksheet(wb, sheetName = "Age Mean")
            writeDataTable(wb, x=ageLengthKey$age_mean, rowNames = FALSE, sheet = sheet16)
          sheet17 <- addWorksheet(wb, sheetName = "Age Mean Standard Error")
            writeDataTable(wb, x=ageLengthKey$age_mean_se, rowNames = FALSE, sheet = sheet17)
          sheet18 <- addWorksheet(wb, sheetName = "Age Total")
            writeDataTable(wb, x=age_total, rowNames = FALSE, sheet = sheet18)
          sheet19 <- addWorksheet(wb, sheetName = "Age Total Standard Error")
            writeDataTable(wb, x=ageLengthKey$age_total_se, rowNames = FALSE, sheet = sheet19)
        } 
      sheet10 <- addWorksheet(wb, sheetName = "Length By Set")
        writeDataTable(wb, x=lengthsData$length_by_set, rowNames = FALSE, sheet = sheet10)
      sheet11 <- addWorksheet(wb, sheetName = "Length Mean")
        writeDataTable(wb, x=length_by_strat_mean, rowNames = FALSE, sheet = sheet11)
      sheet12 <- addWorksheet(wb, sheetName = "Length Mean Standard Error")
        writeDataTable(wb, x=length_by_strat_se, rowNames = FALSE, sheet = sheet12)
      sheet13 <- addWorksheet(wb, sheetName = "Length Total")
        writeDataTable(wb, x=length_total, rowNames = FALSE, sheet = sheet13)
      sheet14 <- addWorksheet(wb, sheetName = "Length Total Standard Error")
        writeDataTable(wb, x=length_total_se, rowNames = FALSE, sheet = sheet14)
      sheet20 <- addWorksheet(wb, sheetName = "Weight by Set")
        writeDataTable(wb, x=weight_by_set, rowNames = FALSE, sheet = sheet20)
      sheet21 <- addWorksheet(wb, sheetName = "Weight Mean")
        writeDataTable(wb, x=nwData[,c("STRAT","MEAN_WGT")], rowNames = FALSE, sheet = sheet21)
      sheet22 <- addWorksheet(wb, sheetName = "Weight Mean Standard Error")
        writeDataTable(wb, x=nwData[,c("STRAT","ST_ERR_WGT")], rowNames = FALSE, sheet = sheet22)
      sheet23 <- addWorksheet(wb, sheetName = "Weight Total")
        writeDataTable(wb, x=nwData[,c("STRAT","BIOMASS")], rowNames = FALSE, sheet = sheet23)
      sheet24 <- addWorksheet(wb, sheetName = "Weight Total Standard Error")
        writeDataTable(wb, x=nwData[,c("STRAT","ST_ERR_BIOMASS")], rowNames = FALSE, sheet = sheet24)
  }else if (nchar(output)>0){
    sheet2 <- addWorksheet(wb, sheetName = "Strata Info")
      writeDataTable(wb, x=dfStrata, rowNames = FALSE, sheet = sheet2)
    sheet3 <- addWorksheet(wb, sheetName = "Numbers and Weights by Strata")
      writeDataTable(wb, x=nwData, rowNames = FALSE, sheet = sheet3)
    sheet3a <- addWorksheet(wb, sheetName = "Numbers and Weights by Set")
      writeDataTable(wb, x=dfNWSets, rowNames = FALSE, sheet = sheet3a)
    if (class(ageLengthKey) == "list"){
      sheet4 <- addWorksheet(wb, sheetName = "Age Length Key")
        writeDataTable(wb, x=data.frame(ageLengthKey$alk), rowNames = TRUE, sheet = sheet4)
      sheet5 <- addWorksheet(wb, sheetName = "Age Table")
        writeDataTable(wb, x=ageLengthKey$age_table, rowNames = TRUE, sheet = sheet5)
      sheet6 <- addWorksheet(wb, sheetName = "Age Length Weight")
        writeDataTable(wb, x=ageLengthKey$alw, rowNames = TRUE, sheet = sheet6)
      sheet12 <- addWorksheet(wb, sheetName = "Age By Set")
        writeDataTable(wb, x=ageLengthKey$age_by_set, rowNames = FALSE, sheet = sheet12)
      sheet13 <- addWorksheet(wb, sheetName = "Age Mean")
        writeDataTable(wb, x=ageLengthKey$age_mean, rowNames = FALSE, sheet = sheet13)
      sheet14 <- addWorksheet(wb, sheetName = "Age Mean Standard Error")
        writeDataTable(wb, x=ageLengthKey$age_mean_se, rowNames = FALSE, sheet = sheet14)
      sheet15 <- addWorksheet(wb, sheetName = "Age Total")
        writeDataTable(wb, x=age_total, rowNames = FALSE, sheet = sheet15)
      sheet16 <- addWorksheet(wb, sheetName = "Age Total Standard Error")
        writeDataTable(wb, x=ageLengthKey$age_total_se, rowNames = FALSE, sheet = sheet16)
    }
    sheet7 <- addWorksheet(wb, sheetName = "Length By Set")
      writeDataTable(wb, x=lengthsData$length_by_set, rowNames = FALSE, sheet = sheet7)
    sheet8 <- addWorksheet(wb, sheetName = "Length Mean")
      writeDataTable(wb, x=length_by_strat_mean, rowNames = FALSE, sheet = sheet8)
    sheet9 <- addWorksheet(wb, sheetName = "Length Mean Standard Error")
      writeDataTable(wb, x=length_by_strat_se, rowNames = FALSE, sheet = sheet9)
    sheet10 <- addWorksheet(wb, sheetName = "Length Total")
      writeDataTable(wb, x=length_total, rowNames = FALSE, sheet = sheet10)
    sheet11 <- addWorksheet(wb, sheetName = "Length Total Standard Error")
    writeDataTable(wb, x=length_total_se, rowNames = FALSE, sheet = sheet11)
  }
  
  saveWorkbook(wb, file = wbName, overwrite=TRUE)
  cat(paste0("\n\nWrote your excel file to ",file.path(getwd(),wbName),""))
  return(res)
  }
