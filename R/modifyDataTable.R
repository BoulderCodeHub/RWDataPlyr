# takes an existing data file, and edits it
changeDataFile <- function(iFile, varName, newVarName = varName, oldVals, newVals)
{
  zz = utils::read.table(iFile, header = T)
  tmp = as.matrix(subset(zz, select = varName))
  for(i in 1:length(oldVals)){
    tmp[tmp == oldVals[i]] = newVals[i]
  }
  
  ii = match(varName, colnames(zz))
  colnames(zz)[ii] = newVarName
  zz[,ii] = tmp
  utils::write.table(as.matrix(zz), iFile, row.names = F, sep = '\t')
} 

# todo: create general function for adding attributes based on arbitrary functions
dummyFunction <- function()
{
  # tag for drought contingency modeling
  if(tags == 'DCP'){
    scenFull <- strsplit(sPath,'/')[[1]]
    scenFull <- scenFull[length(scenFull)]
    
    dryOrWet15 <- readDryWetFile(scenFull, '15To19')
    dryOrWet20 <-readDryWetFile(scenFull, '20To26')
    vulnT <- readVulnFile(scenFull)
    d15 <- match(allRes$Trace, names(dryOrWet15))
    d15 <- dryOrWet15[d15]
    d20 <- match(allRes$Trace, names(dryOrWet20))
    d20 <- dryOrWet20[d20]
    vv <- match(allRes$Trace, names(vulnT))
    vv <- vulnT[vv]
    
    allRes$DryOrWet2015To2019 <- simplify2array(d15)
    allRes$DryOrWet2020To2026 <- simplify2array(d20)
    allRes$VulnTrace <- simplify2array(vv)
    
    # create a separate column for supply, one for Scenario, and one for hyd-Scenario
    tmp <- as.character(allRes$Scenario)
    allRes$Hyd_Scen <- allRes$Scenario
    onlyScen <- getScenNoSupply(scenFull)
    onlyHyd <- getScenHydrology(scenFull)
    allRes$Scenario <- rep(onlyScen, nrow(allRes))
    allRes$Hydrology <- rep(onlyHyd, nrow(allRes))
  }
}