
readRwCsv <- function(file, ...) {
  # read in the file
  zz <- data.table::fread(file, sep = ',', header = TRUE, data.table = FALSE, ...)
  
  # check column names
  reqColumns <- c("Run Number", "Trace Number", "Object.Slot", "Timestep", "Slot Value")
  
  opColumns <- c("Object Name", "Object Type", "Slot Name", "Slot Name with Unit", 
                 "Unit", "Timestep Size", "Year", "Month", "Model Name", 
                 "Ruleset File Name", "Input DMI Name", "MRM Config Name", "MRM Descriptors")
  
  # update column names so they contain no spaces
}