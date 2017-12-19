#' @title stranal
#' @description stranal is a program capable of Stratified Analyses of both DFO 
#' and NMFS fisheries survey data. This package attempts to emulate the results
#' of the older, APL version which has been in use for many years.
#' 
#' Using stranal requires: 
#' \enumerate{
#' \item an Oracle account on the DFO PTRAN database;
#' \item access to PTRAN (i.e. either within the network, or via a VPN) 
#' \item appropriate select permissions on the 'groundfish' and/or 'usnefsc' 
#' schemas
#' }
#' New Oracle accounts are requested by filling out a computer account request 
#' form. These forms may be obtained from IM&TS. Select access to the DFO and 
#' NMFS data is provided by the respective datasets manager (checking with 
#' \email{Mike.McMahon@@dfo-mpo.gc.ca} is a good start).
#' 
#' Users can supply as much or as little information as they want when calling 
#' stranal(). If required information is not supplied, a select box will show 
#' the available options to the user. Please see below for examples of function 
#' calls providing both the minimum and the maximum number of parameters.
#' 
#' Fun Facts About Stranal
#' \enumerate{
#' \item Berried Females -- APL Stranal was inconsistent with species where 
#' females could be coded as 'berried' (i.e. 3).  When unsexed analyses were 
#' done, the females were included, but when sexed analyses were done, these 
#' females were not included in the results.  Such inconsistency seems 
#' suboptimal, and is retained for now such that APL and R versions of the 
#' application can be compared. 
#' \item Ages By Sex -- When APL Stranal did analyses by sex, it combined the 
#' sexes for the 'Age Mean', 'Age Total', (and standard errors of each).  This 
#' version adds the parameter \code{ageBySex} so that by setting it to FALSE, 
#' you can get the combined results, but you can also set it to TRUE, and get 
#' the values for each sex individually.
#' \item I liked the old way -- Some aspects of the 'classic' (i.e. APL) 
#' Stranal seemed inefficient.   For example, the Strata information was broken 
#' up over multiple worksheets, as was the weight information. By default, a new 
#' spreadsheet has been designed, but since there will always be those who 
#' prefer the old way, setting \code{output='classic'} will generate a 
#' spreadsheet almost indistinguishable from the APL version.
#' }
#' 
#' @param usepkg  The default value is \code{'rodbc'}, but \code{'roracle'} 
#' works as well. This describes the R package you use to connect to Oracle.  
#' @param agency  The default value is \code{'DFO'}, the other option is 
#' \code{'NMFS'}.  Setting to \code{NULL} will result in a pick list.
#' @param type This is the 'experiment type', and the default value is \code{1}.  
#' For \code{agency='DFO'}, \code{5} is also acceptable. For agency = NMFS, 136 
#' is probable, but you can type any integer, and the typed number will be 
#' embedded in\code{USNEFSC.USS_STATION.SHG <= type}. Setting to \code{NULL} 
#' will result in a pick list.
#' @param year The default value is \code{NULL}, which will result in a pick list.
#' An example of a valid, non-empty value is \code{2017}.
#' @param season The default value is \code{''}, which will result in a pick 
#' list. Valid values are \code{'SUMMER', 'SPRING', 'WINTER', and 'FALL'}, 
#' subject to the availability of data in that season for the selected agency, 
#' year, type, etc... 
#' @param missions The default value is \code{NULL}, which will result in a pick 
#' list. An example valid value is \code{c('NED2016016')}.  If your choice is 
#' not valid (given your other selections), you will be presented with a pick
#' list of valid options. 
#' @param strataTable  The default value is \code{'GROUNDFISH.GSSTRATUM'}.  
#' Depending on your data, other valid values may include: 
#' \code{'USNEFSC.DFO5ZJM','USNEFSC.DFO5ZGHNO','USNEFSC.NMFS5ZJM',
#' 'USNEFSC.NMFS5ZGHNO','USNEFSC.NMFS5ZJMC','USNEFSC.NMFS5ZJMU',
#' 'USNEFSC.NMFS5ZU'}.  Setting this to \code{NULL} will result in a pick list. 
#' @param wingspread  This is the width in feet of a standard tow.  The default 
#' value is \code{41}.  Setting this to \code{NULL} will result in a pick list. 
#' @param towDist  This is the length (NM) of a standard tow.  The default value 
#' is \code{1.75}.  Setting this to \code{NULL} will result in a pick list. 
#' @param strata  These are the strata for which you want results.  The default 
#' value is \code{c(440:495)} (DFO Summer Survey Strata).  Setting this to 
#' \code{NULL} will result in a pick list. 
#' @param spp  This is the species code for the species you want to analyze.  
#' The default value is \code{''}, which will result in a pick list. An example 
#' of a valid, non-empty value is \code{2526}. 
#' @param bySex If the selected species was measured by sex, this parameter
#' allows you to perform calculations by sex.  The default value is \code{''}, 
#' which will result in a pick list. An example of a valid, non-empty value is 
#' \code{TRUE}. 
#' @param ageBySex The APL version of stranal ignored sex differences in some 
#' age results (e.g. 'age by set', 'age mean', 'age total', ' etc.) despite
#' analyses being done by sex.  The default value of this parameter is 
#' \code{FALSE} so that the original results are emulated.  However, setting 
#' this to \code{TRUE} (when \code{bySex=TRUE}), will show age results broken 
#' down by sex.  Setting this to \code{''} will result in a pick list.
#' @param output  The default value is \code{'new'}.  This determines the format 
#' of the output Excel file. Setting this to \code{'classic'} will emulate the
#' original APL STRANAL results, including overriding your parameter for 
#' \code{ageBySex} and forcing it to FALSE.  If no excel output is desired, set
#' this parameter to an empty string \code{''}
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
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


stranal<-function(usepkg = 'rodbc', 
                  agency = 'DFO',
                  type = 1,
                  year = NULL,
                  season = NULL,
                  missions = NULL,
                  strataTable = 'GROUNDFISH.GSSTRATUM',
                  wingspread = 41,
                  towDist = 1.75,
                  strata = c(440:495),
                  spp = NULL,
                  bySex = NULL,
                  ageBySex = FALSE,
                  output = "new"
                  ){
  
  assign("oracle_cxn", Mar.utils::make_oracle_cxn(usepkg), envir = .GlobalEnv )
  
  agency = getUserInput("agency",agency=agency)
  type = getUserInput("type", agency=agency, type=type)
  #NED2016016
  missionsAndStrata = getUserInput("missionsAndStrata", agency=agency,type=type, 
                                   year=year, season=season, missions=missions)
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
  if (!is.null(output)){
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
  }
  saveWorkbook(wb, file = wbName, overwrite=TRUE)
  cat(paste0("\n\nWrote your excel file to ",file.path(getwd(),wbName),""))
  return(res)
  }
