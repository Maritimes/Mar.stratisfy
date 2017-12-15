# Mar.stranal
### (an R-based version of STRANAL)
#### Dec 15, 2017
---
## Introduction and Approach
STRANAL (or "Stratified Analysis") has been used by Fisheries Biologists at DFO and NMFS to assist with assessments since before 2006.  While the original <a href="https://en.wikipedia.org/wiki/APL_(programming_language)">APL</a> application still works well, the number of people who are able to modify and enhance it is dwindling.  

As a result, we have converted the application to an R package, a format and language known to many within the Population Ecology Division.   By producing a package rather than a script, we are better able to version control the code and ensure consistent results. It should run regardless of OS, working directory, etc...

The hope is that users will be better able to collaborate and enhance Mar.stranal now that the code is readily accessible.  Additionally, since it is now a package rather than an application, its operation is transparent, and PED staff can maintain it without IMTS support.

## <a name="Limitations"></a>  Current Limitations
As of right now, the STRANAL does not yet include:

*  Vessel Net Conversion
*  ALK Modifications (including User-specified age/length table)

## <a name="Running"></a>  Installing and Running
Mar.stranal has a single function - `stranal()` after you install and load the package.

It can be run via command line or via the built-in Graphical User Interface (GUI).  If a correct Oracle username and password are not provided, you will be prompted to provide them.  

If it is run by command line - each input parameter is compared with valid values. If they are not determined to be valid, a select box will present the user with valid options.

You can install Mar.stranal from github with:

```R
# install.packages("devtools")

devtools::install_github("Maritimes/Mar.utils")
devtools::install_github("Maritimes/Mar.stranal")
```

The script can be run like the following:

```R
# very basic - lots of prompts
results <- stranal()

# all details provide - no prompts
results <- stranal(year = 1990, season = "SUMMER", bySex=TRUE, ageBySex = TRUE, missions = c('NED1990140'), spp=14)
```
Depending on what parameters are provided, when the script is run, the user will be prompted for the following:
*  usepkg - "rodbc" or "roracle" - however you connect to oracle 
*  agency - "DFO" or "NMFS" 
*  type = 1
*  year = NULL
*  season = NULL
*  missions = NULL
*  strataTable = "GROUNDFISH.GSSTRATUM""
*  wingspread = 41
*  towDist = 1.75
*  strata = c(440:495)
*  spp = NULL
*  bySex = NULL
*  ageBySex = FALSE
*  output = "new" or "classic". Set as "" to skip excel file generation
  