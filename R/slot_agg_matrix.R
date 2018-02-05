# checks: if CY or WY, summary "none" probably doesn't make sense
# if time is raw, then summary must be none

# create ann attribute called "all"; if it is true, then after the get data function
# is called, it will call another function to recreate the slotaggm with all the 
# appropriate values; when the default constructor is called with 
# c("KeySlots.rdf" = "all", "SystemCondtions.rdf" = all), it will only fill in the
# first two columns, and then plac NA's everywhere else; skipping all other 
# relivant checks.
