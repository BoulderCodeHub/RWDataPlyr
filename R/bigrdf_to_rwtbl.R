#' @export

# TODO: make arrow only suggested and check here if it's insalled? Then install
# if user confirms? could see benchmark and its use of ggbehive as example

bigrdf_to_rwtbl <- function(file, scenario = NA_character_, keep_cols = FALSE, 
                          add_ym = TRUE, status = TRUE, n_trace_per_chunk = 20)
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
  
  pq_path <- normalizePath(tempfile('bigrdf_'), winslash = '/', mustWork = FALSE)
  dir.create(pq_path)
  cat("creating\n", pq_path, '\n')
  
  tmp <- list()
  
  while (row_i >= 0) {
    if (status) {
      cat('Trace: ', trace_num + 1, '\n')  
    }
    
    tmp <- rdf_to_rwtbl_cpp(
      rdf_vec, 
      keep_cols = keep_cols, 
      scenario = scenario, 
      add_ym = add_ym, 
      row_index = row_i,
      last_trace = trace_num,
      n_trace_parse = n_trace_per_chunk
    )
    
    row_i <- attr(tmp, 'last_i', exact = TRUE)
    attr(tmp, 'last_i') <- NULL
    
    trace_num <- tail(tmp$TraceNumber, 1)
    
    tmp %>%
      dplyr::group_by(.data[['TraceNumber']]) %>%
      arrow::write_dataset(path = pq_path, format = "parquet")
      
  }
  
  ds <- arrow::open_dataset(pq_path)
  attr(ds, 'base_dir') <- pq_path
  ds
}

#' @export
# TODO: add option to remove the unique folder, e.g., bigrdf_[alphanumeric]
bigrdf_move <- function(x, path)
{
  ifolder <- attr(x, 'base_dir', exact = TRUE)
  # TODO: add error if this attr doesn't exist
  # TODO: check that path exists
  
  file.copy(ifolder, path, recursive = TRUE)
  
  invisible(x)
}
