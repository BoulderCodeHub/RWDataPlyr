# reqColumns <- c("Run Number", "Trace Number", "Object.Slot", "Timestep", "Slot Value")
# c("Object Name", "Object Type", "Slot Name", "Slot Name with Unit", "Unit", 
#   "Timestep Size", "Year", "Month", "Model Name", "Ruleset File Name", 
#   "Input DMI Name", "MRM Config Name", "MRM Descriptors")

rdfObjectToTbl <- function(rdfObject, timeSteps)
{
  ot <- rdfObject$object_type
  on <- rdfObject$object_name
  sn <- rdfObject$slot_name
  os <- paste(on, sn, sep = ".")
  units <- rdfObject$units
  scale <- as.numeric(rdfObject$scale)
  value <- rdfObject$values * scale
  
  df <- data.frame("Timestep" = timeSteps) %>%
    mutate(
      ObjectName = on,
      SlotName = sn,
      ObjectType = ot,
      ObjectSlot = os,
      Value = value,
      Unit = units
    )

  df
}


# for a single run/trace, take the list and create a data frame from it
rdfTraceToTbl <- function(rdfTrace, traceNum) 
{
  trace <- as.integer(rdfTrace$idx_sequential)
  timeSteps <- rdfTrace$times
  ruleSet <- rdfTrace$rule_set
  slotSet <- rdfTrace$slot_set # the input DMI on the input tab
  
  # ***
  # maybe check if time_step_unit is year, and if so, change the timestep label
  # maybe change to yearmon here?
  # ***
  
  # for all objects, call getObjectData for rdfTrace$objects
  df <- do.call(
    rbind, 
    lapply(rdfTrace$objects, rdfObjectToTbl, timeSteps = timeSteps)
  ) %>%
    mutate(TraceNumber = traceNum, RulesetFileName = ruleSet, InputDMIName = slotSet)
  df
}

rdfToTbl <- function(rdf, scenario = NULL)
{
  # rdf[[1]] contains meta data
  # rdf[[2]] will be the length of rdf[[1]]$number_of_runs
  atts <- rdf[[1]]
  nRun <- as.integer(rdf[[1]]$number_of_runs)
  
  df <- do.call(
    rbind, 
    lapply(1:nRun, function(x) rdfTraceToTbl(rdf[[2]][[x]], traceNum = x))
  )
  
  if(!is.null(scenario))
    df$Scenario <- scenario
  
  attr(df, "MRMName") <- atts$name
  attr(df, "owner") <- atts$owner
  attr(df, "description") <- atts$description
  attr(df, "createDate") <- atts$create_date
  attr(df, "numberOfTraces") <- atts$number_of_runs
  
  df
}
