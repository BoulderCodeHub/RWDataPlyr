# aggregate different variables in different ways

# given mass and flow at the monthly basis, the flow-weighted average annual concentration is returned
# mass and flow should be monthly data and should be for only a single trace, or for one year of one trace
# expect flow to be in acre-ft/month and mass to be in tons
# return value will be in mg/L
flowWeightedAvgAnnConc <- function(mass, flow)
{
	if(length(mass)%%12 != 0 | length(flow)%%12 != 0)
		stop('Data passed to flowWeightedAvgAnnConc is not divisible by 12')
		
	# move into a years x months matrix
	mass <- matrix(mass, ncol = 12, byrow = T)
	flow <- matrix(flow, ncol = 12, byrow = T)
	
	mass.annAvg <- apply(mass, 1, sum)/12
	flow.annAvg <- apply(flow, 1, sum)/12 # now essentially a volume
	
	conc <- mass.annAvg/flow.annAvg*735.466642
	
	return(conc)
}

returnMinAnn <- function(traceVal)
{
	tmp <- matrix(traceVal, ncol = 12, byrow = T)
	tmp <- apply(tmp, 1, min)
	return(tmp)
}

# xx is a matrix such as that returned by rdfSlotToMatrix
getMinAnnValue <- function(xx)
{
	minAnn <- apply(xx, 2, returnMinAnn)
	return(minAnn)
}

returnMaxAnn <- function(traceVal)
{
	tmp <- matrix(traceVal, ncol = 12, byrow = T)
	tmp <- apply(tmp, 1, max)
	return(tmp)
}

# xx is a matrix such as that returned by rdfSlotToMatrix
getMaxAnnValue <- function(xx)
{
	maxAnn <- apply(xx, 2, returnMaxAnn)
	return(maxAnn)
}




