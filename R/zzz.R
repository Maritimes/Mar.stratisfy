.onAttach <- function(libname, pkgname) {
  localVer = utils::packageDescription('Mar.stratisfy')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}
.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  options(scipen=999)
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.datawrangling')
}
