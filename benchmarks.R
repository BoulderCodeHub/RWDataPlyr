library(dplyr)
rdf_vec <- as.matrix(data.table::fread(
  "inst/extdata/Scenario/ISM1988_2014,2007Dems,IG,Most/KeySlots.rdf", 
  sep = '\t', 
  header = FALSE, 
  data.table = FALSE
))

rdf <- read_rdf("inst/extdata/Scenario/ISM1988_2014,2007Dems,IG,Most/KeySlots.rdf")

std_cols <- c("Timestep", "TraceNumber", "ObjectSlot", "Value")

mb <- microbenchmark::microbenchmark(
  "c++" = RWDataPlyr:::rdf_to_rwtbl_cpp(rdf_vec, keep_cols = std_cols, add_ym = FALSE) %>% tibble::as_tibble(),
  "c++ old" = RWDataPlyr:::rdf_to_rwtbl_cpp_old(rdf_vec) %>% tibble::as_tibble(),
  "R" = rdf_to_rwtbl(rdf, keep_cols = TRUE),
  times = 50
)

mb2<- microbenchmark::microbenchmark(
  "c++" = RWDataPlyr:::rdf_to_rwtbl_cpp(rdf_vec, scenario = "haha", keep_cols = std_cols) %>% tibble::as_tibble(),
  "c++ old" = RWDataPlyr:::rdf_to_rwtbl_cpp_old(rdf_vec) %>% tibble::as_tibble(),
  "R" = rdf_to_rwtbl(rdf, keep_cols = TRUE, scenario = "haha"),
  times = 50
)


t1 <- RWDataPlyr:::rdf_to_rwtbl_cpp(rdf_vec, scenario = "haha", keep_cols = std_cols) %>% tibble::as_tibble()
t2 <- rdf_to_rwtbl(rdf, keep_cols = TRUE, scenario = "haha")

# --------------------------

sal <- slot_agg_list(
  matrix(
    c("KeySlots.rdf", "Mead.Pool Elevation", "EOCY", NA, "meadPe"), 
    nrow = 1
  )
)

rwa <- rwd_agg(data.frame(
  file = "KeySlots.rdf",
  slot = "Mead.Pool Elevation",
  period = "December",
  summary = NA,
  eval = NA,
  t_s = NA,
  variable = "meadPe",
  stringsAsFactors = FALSE
))

scen <- "ISM1988_2014,2007Dems,IG,Most"

spath <- system.file("extdata", "Scenario/", package = "RWDataPlyr")

mb <- microbenchmark::microbenchmark(
  "rdfagg cpp" = rdf_aggregate(rwa, file.path(spath, scen), cpp = TRUE),
  "rdfagg old" = rdf_aggregate(rwa, file.path(spath, scen), cpp = FALSE),
  "scenagg cpp" = rw_scen_aggregate(scen, rwa, spath, scen_names = scen, cpp = TRUE),
  "scenagg old" = rw_scen_aggregate(scen, rwa, spath, scen_names = scen, cpp = FALSE),
  "getallscens" = getDataForAllScens(scen, scen, sal, spath, "tmp.feather"),
  times = 50
)

rwa <- rwd_agg(read.csv(
  system.file(
    "extdata/rwd_agg_files/passing_aggs.csv", 
    package = "RWDataPlyr"
  ), 
  stringsAsFactors = FALSE
))

scens <- c("ISM1988_2014,2007Dems,IG,Most", "ISM1988_2014,2007Dems,IG,2002")

mb <- microbenchmark::microbenchmark(
  "scenagg cpp" = rw_scen_aggregate(scen, rwa, spath, scen_names = scen, cpp = TRUE),
  "scenagg old" = rw_scen_aggregate(scen, rwa, spath, scen_names = scen, cpp = FALSE),
  "getallscens" = getDataForAllScens(scen, scen, sal, spath, "tmp.feather"),
  times = 50
)
