.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Version: ", utils::packageDescription('Mar.stranal')$Version))
}
.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  options(scipen=999)
}
