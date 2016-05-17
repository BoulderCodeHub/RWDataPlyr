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
