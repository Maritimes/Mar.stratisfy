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
#' @param output The default value is \code{new}.  This determines the format of 
#' the output Excel file. Setting to \code{classic} will try to emulate the
#' original APL STRANAL results
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}l
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom stats aggregate
#' @importFrom Mar.utils make_oracle_cxn
#' @importFrom Mar.utils SQL_in
#' @importFrom Mar.utils st_err
#' @importFrom xlsx createWorkbook
#' @importFrom xlsx createSheet
#' @importFrom xlsx addDataFrame
#' @importFrom xlsx saveWorkbook
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

stranal<-function(usepkg = 'roracle', 
                  agency = 'DFO',
                  type = 1,
                  year = 2017,
                  season = "SUMMER",
                  strataTable = "GROUNDFISH.GSSTRATUM",
                  wingspread = 41,
                  towDist = 1.75,
                  strata = c(440:495),
                  spp = 2526,
                  bySex = F,
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

  spp = getUserInput("spp", agency = agency, spp=spp, bySex = bySex)
  sexed = spp[[1]]
  dfSpp = spp[[2]]
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
  
  nwData <- merge(allStrat, dfNWAgg, all.x = TRUE)
  nwData[is.na(nwData)]<-0
  
  
  lengthsData <-calcAgeLen('lengths', agency = agency, dfNWSets=dfNWSets, dfRawDet=dfRawDet, 
                dfRawInf=dfRawInf, dfStrata=dfStrata, dfSpp=dfSpp, 
                towDist=towDist, sexed=sexed)
    agelen<-lengthsData$agelen
    lengthsTotals<-lengthsData$length_total
    lset = lengthsData$lset
  ageLengthKey <-calcAgeLen('ageKey', agelen=agelen, dfSpp=dfSpp, lengthsTotals 
                            = lengthsTotals, lset = lset)
  metadata=list("Mar.stranal" = utils::packageDescription('Mar.stranal')$Version,
                "Date" = Sys.time(),
                "Data Source" = agency,
                "Strata" = paste("'", paste(strata, collapse="','"),"'", sep=""),
                "Species" = paste0(dfSpp$CNAME, " (", dfSpp$SPEC ,")"),
                "By Sex" = bySex,
                "Distance" = towDist,
                "Spread" = wingspread,
                "Stratum Area Table" =  strataTable,
                "Experiment Type" = type,
                "ALK Modifications" = 'Not implemented yet')
  res=list(metadata = metadata,
           strataInfo = dfStrata,
           nwInfo = nwData,
           lengthInfo = lengthsData,
           ageInfo = ageLengthKey
  )
  if (output=="classic"){
    wbName = "Mar.stranal.xlsx"
    md = data.frame(unlist(metadata))
    colnames(md)<-"Value"
    wb<-createWorkbook(type="xlsx")
      sheet1 <- createSheet(wb, sheetName = "QUERY")
        addDataFrame(md, row.names = TRUE, sheet1)
      sheet2 <- createSheet(wb, sheetName = "Strata Area")
        addDataFrame(dfStrata[,c("STRAT","TUNITS","SQNM")], row.names = FALSE, sheet2)
      sheet3 <- createSheet(wb, sheetName = "Prop Area")
        addDataFrame(dfStrata[,c("STRAT","AREAPROP")], row.names = FALSE, sheet3)
      sheet4 <- createSheet(wb, sheetName = "Prop Area Standard Error")
        addDataFrame(dfStrata[,c("STRAT","AREAPROPSTERR")], row.names = FALSE, sheet4)
      sheet5 <- createSheet(wb, sheetName = "Total Area")
        addDataFrame(dfStrata[,c("STRAT","AREATOT")], row.names = FALSE, sheet5)
      sheet6 <- createSheet(wb, sheetName = "Total Area Standard Error")
        addDataFrame(dfStrata[,c("STRAT","AREATOTSTERR")], sheet6)
      sheet7 <- createSheet(wb, sheetName = "Age Length Key")
        addDataFrame(ageLengthKey$alk, row.names = TRUE, sheet7)
      sheet8 <- createSheet(wb, sheetName = "Age Table")
        addDataFrame(ageLengthKey$age_table, row.names = TRUE, sheet8)
      sheet9 <- createSheet(wb, sheetName = "Age Length Weight")
        addDataFrame(ageLengthKey$alw, row.names = TRUE, sheet9)
      sheet10 <- createSheet(wb, sheetName = "Length By Set")
        addDataFrame(lengthsData$length_by_set, row.names = FALSE, sheet10)
      sheet11 <- createSheet(wb, sheetName = "Length Mean")
        addDataFrame(lengthsData$length_by_strat_mean, row.names = FALSE, sheet11)
      sheet12 <- createSheet(wb, sheetName = "Length Mean Standard Error")
        addDataFrame(lengthsData$length_by_strat_se, row.names = FALSE, sheet12)
      sheet13 <- createSheet(wb, sheetName = "Length Total")
        addDataFrame(lengthsData$length_total, row.names = FALSE, sheet13)
      sheet14 <- createSheet(wb, sheetName = "Length Total Standard Error")
        addDataFrame(lengthsData$length_total_se, row.names = FALSE, sheet14)
      sheet15 <- createSheet(wb, sheetName = "Age By Set")
      sheet16 <- createSheet(wb, sheetName = "Age Mean")
      sheet17 <- createSheet(wb, sheetName = "Age Mean Standard Error")
      sheet18 <- createSheet(wb, sheetName = "Age Total")
      sheet19 <- createSheet(wb, sheetName = "Age Total Standard Error")
      sheet20 <- createSheet(wb, sheetName = "Weight by Set")
        #addDataFrame(nwData[,c("STRAT", "TOT_WGT")], row.names = FALSE, sheet20)
      sheet21 <- createSheet(wb, sheetName = "Weight Mean")
        addDataFrame(nwData[,c("STRAT","MEAN_WGT")], row.names = FALSE, sheet21)
      sheet22 <- createSheet(wb, sheetName = "Weight Mean Standard Error")
      addDataFrame(nwData[,c("STRAT","ST_ERR_WGT")], row.names = FALSE, sheet22)
      sheet23 <- createSheet(wb, sheetName = "Weight Total")
        addDataFrame(nwData[,c("STRAT","BIOMASS")], row.names = FALSE, sheet23)
      sheet24 <- createSheet(wb, sheetName = "Weight Total Standard Error")
        addDataFrame(nwData[,c("STRAT","ST_ERR_BIOMASS")], row.names = FALSE, sheet24)
    saveWorkbook(wb, "Mar.stranal.xlsx")
    
    cat(paste0("\n\nWrote your excel file to ",file.path(getwd(),wbName),""))
  }else{
    wbName = "Mar.stranal.xlsx"
    md = data.frame(unlist(metadata))
    colnames(md)<-"Value"
    wb<-createWorkbook(type="xlsx")
    sheet1 <- createSheet(wb, sheetName = "QUERY")
      addDataFrame(md, row.names = TRUE, sheet1)
    sheet2 <- createSheet(wb, sheetName = "Strata Info")
      addDataFrame(dfStrata, row.names = FALSE, sheet2)
    sheet3 <- createSheet(wb, sheetName = "Numbers and Weights by Strata")
      this = merge(cbind("STRAT"= dfStrata$STRAT),dfNWAgg,all.x=T)
      addDataFrame(this, row.names = FALSE, sheet3)
      rm(this)
    sheet3a <- createSheet(wb, sheetName = "Numbers and Weights by Set")
      this = merge(cbind("STRAT"= dfStrata$STRAT),dfNWSets,all.x=T)
      addDataFrame(this, row.names = FALSE, sheet3a)
      rm(this)
    sheet4 <- createSheet(wb, sheetName = "Age Length Key")
      addDataFrame(ageLengthKey$alk, row.names = TRUE, sheet4)
    sheet5 <- createSheet(wb, sheetName = "Age Table")
      addDataFrame(ageLengthKey$age_table, row.names = TRUE, sheet5)
    sheet6 <- createSheet(wb, sheetName = "Age Length Weight")
      addDataFrame(ageLengthKey$alw, row.names = TRUE, sheet6)
    sheet7 <- createSheet(wb, sheetName = "Length By Set")
      addDataFrame(lengthsData$length_by_set, row.names = FALSE, sheet7)
    sheet8 <- createSheet(wb, sheetName = "Length Mean")
      this = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_by_strat_mean,all.x=T)
      addDataFrame(this, row.names = FALSE, sheet8)
      rm(this)
    sheet9 <- createSheet(wb, sheetName = "Length Mean Standard Error")
      this = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_by_strat_se,all.x=T)
      addDataFrame(this, row.names = FALSE, sheet9)
      rm(this)
    sheet10 <- createSheet(wb, sheetName = "Length Total")
      this = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_total,all.x=T)
      addDataFrame(this, row.names = FALSE, sheet10)
      rm(this)
    sheet11 <- createSheet(wb, sheetName = "Length Total Standard Error")
      this = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_total_se,all.x=T)
      addDataFrame(this, row.names = FALSE, sheet11)
      rm(this)
    saveWorkbook(wb, "Mar.stranal.xlsx")
    cat(paste0("\n\nWrote your excel file to ",file.path(getwd(),wbName),""))
  }
  return(res)
  }
