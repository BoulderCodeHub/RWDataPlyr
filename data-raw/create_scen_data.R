# create the scen_data exported example data
rwa <- read_rwd_agg("inst/extdata/rwd_agg_files/passing_aggs.csv")

scens <- c(
  "2002" = "ISM1988_2014,2007Dems,IG,2002",
  "Most" = "ISM1988_2014,2007Dems,IG,Most"
)

tmp <- rw_scen_aggregate(scens, rwa, scen_dir = 'inst/extdata/Scenario')
scen_data <- tmp
usethis::use_data(scen_data, overwrite = TRUE)
