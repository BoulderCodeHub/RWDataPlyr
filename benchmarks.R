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
