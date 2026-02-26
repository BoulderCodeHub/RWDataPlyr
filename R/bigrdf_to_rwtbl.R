#' Convert a 'big' rdf file into a tibble-like object.
#' 
#' @description
#' `bigrdf_to_rwtbl()` converts an rdf into a tibble-like object. 'Big' rdf 
#' files can cause memory issues when processing them, and keeping them stored
#' entirely in memory as tibbles. `bigrdf_to_rwtbl()` processes the rdf files
#' in chunks, specified as the number of traces per chunk (`n_trace_per_chunk`),
#' and saves the data as a temporary [arrow::FileSystemDataset], where each
#' trace is saved as a single parquet file. This ensures there are no memory 
#' issues when processing the rdf file. 
#' 
#' Users can then save the `FileSystemDataSet` using `bigrdf_save()`, and/or 
#' work with it from its temporary location in a tidyverse pipeline. Using 
#' [dplyr::collect()] will return a tibble in memory. 
#' 
#' @inheritParams rdf_to_rwtbl2
#' 
#' @param status If `TRUE`, print out information on status of processing the 
#' rdf. 
#' 
#' @param n_trace_per_chunk How many traces to process at a time. It is faster
#' to process more traces at a time, but you are more likely to run into 
#' memory issues. It will be dependent on the overall structure of the rdf in 
#' terms of number of variables and timesteps and will be up to the user to 
#' determine an appropriate value in terms of speed vs. memory. 
#' 
#' @export
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
  
  pq_path <- normalizePath(
    tempfile(paste0(tools::file_path_sans_ext(basename(file)),'_')), 
    winslash = '/', 
    mustWork = FALSE)
  
  dir.create(pq_path)
  cat("creating\n", pq_path, '\n')
  
  tmp <- list()
  part_count <- 0
  
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
    
    # TODO: consider changing this to be grouped by variable, but would have to
    # write custom write out code so that part-0, part-1... work for the 
    # different groups of traces, e.g., folders would be Vaariable=x.y, 
    # Variable = x.y2 ... and then there woulwd be part-0.parquet for first 
    # parsing of traces, part-1.parquet for next, etc. This cannot be done
    # by defaults of write_dataset()
    tmp %>%
      dplyr::group_by(.data[['TraceNumber']]) %>%
      arrow::write_dataset(
        path = pq_path, 
        format = "parquet"
      )
    
    part_count <- part_count + 1
  }
  
  ds <- arrow::open_dataset(pq_path)
  attr(ds, 'base_dir') <- pq_path
  ds
}
#' @rdname bigrdf_to_rwtbl
#' 
#' @param x Object returned by `bigrdf_to_rwtbl()`
#' 
#' @param path The file path where the user wants the bigrdf saved.
#' 
#' @param remove_uid In the temporary storage, a unique ID is included in the
#' FileSystemDataSet folder name. If this is `TRUE`, that UID is removed when
#' saving the data to `path`.
#' 
#' @export
bigrdf_save <- function(x, path, remove_uid = FALSE)
{
  ifolder <- attr(x, 'base_dir', exact = TRUE)
  
  if (is.null(ifolder)) {
    stop(
      "x does not have required attribute that is set by `bigrdf_to_rwtbl()`"
    )
  }
  
  if (!dir.exists(path)) {
    stop(path, " does not already exist.")
  }
  
  file.copy(ifolder, path, recursive = TRUE)
  
  folder_orig <- basename(ifolder)
  new_path <- file.path(path, folder_orig)
  
  if (remove_uid) {
    folder_new <- sub("_[a-zA-Z0-9]+$", "", folder_orig)
    
    file.rename(new_path, file.path(path, folder_new))
    new_path <- file.path(path, folder_new)
  }
  
  invisible(arrow::open_dataset(new_path))
}
