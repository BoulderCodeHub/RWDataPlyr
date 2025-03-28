#' @export

# TODO: make arrow only suggested and check here if it's insalled? Then install
# if user confirms? could see benchmark and its use of ggbehive as example

bigrdf_to_rwtbl <- function(file, scenario = NA_character_, keep_cols = FALSE, 
                          add_ym = TRUE)
{
  if (! is.character(file) & length(file) != 1) {
    stop(
      "In `rdf_to_rwtbl2()`, `file` should be a single entry character vector,",
      "  i.e., a file path to one rdf file.",
      call. = FALSE
    )
  }
  
  rdf_vec <- read_rdf(file, rdf = FALSE)
  
  check_rdf_to_rwtbl_args(scenario, keep_cols, add_ym, "rdf_to_rwtbl2")
  
  add_cols <- c("ObjectName", "SlotName", "ObjectType" ,"Unit", 
                "RulesetFileName", "InputDMIName", "Scale")
  
  if (is.logical(keep_cols)) {
    if (keep_cols) {
      keep_cols <- c(req_rwtbl_cols(), add_cols)
    } else {
      keep_cols <- req_rwtbl_cols()
    }
  } else {
    # combine keep_cols with the required columns
    keep_cols <- keep_cols[!(keep_cols %in% req_rwtbl_cols())]
    keep_cols <- c(req_rwtbl_cols(), keep_cols)
  }
  
  # check that keep_cols are all expected column names
  missing_cols <- keep_cols[!(keep_cols %in% c(req_rwtbl_cols(), add_cols))]
  if (length(missing_cols) > 0)
    warning(
      "The following columns specified by 'keep_cols' were not found in the rwtbl:\n",
      "    ", toString(missing_cols)
    )
  
  if (is.null(scenario)) {
    scenario <- NA_character_
  }
  
  if (! is.character(scenario))
    scenario <- as.character(scenario)
  
  row_i <- 0
  trace_num <- 0
  
  res <- list()
  
  pq_path <- normalizePath(tempfile('bigrdf_'), winslash = '/', mustWork = FALSE)
  dir.create(pq_path)
  print(paste("creating\n", pq_path))
  
  while (row_i >= 0) {
    tmp <- RWDataPlyr:::rdf_to_rwtbl_cpp(
      rdf_vec, 
      keep_cols = keep_cols, 
      scenario = scenario, 
      add_ym = add_ym, 
      big = TRUE, 
      row_index = row_i,
      last_trace = trace_num
    )
    
    row_i <- attr(tmp, 'last_i', exact = TRUE)
    attr(tmp, 'last_i') <- NULL
    
    trace_num <- tmp$TraceNumber[1]
    
    tmp %>%
      dplyr::group_by(.data[['TraceNumber']]) %>%
      arrow::write_dataset(path = pq_path, format = "parquet")
  }
  
  arrow::open_dataset(pq_path)
}