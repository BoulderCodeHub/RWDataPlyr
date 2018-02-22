
# slot_agg_matrix, 
# scen_dir = ".",
# scenario = NULL,
# keep_cols = FALSE,
# nans_are = "0"
#' @export
rw_scen_aggregate <- function(scenarios, 
                              agg, 
                              scen_dir = ".", 
                              nans_are = "0",
                              keep_cols = FALSE,
                              file = NULL, 
                              scen_names = NULL)
{
  if (!is.rwd_agg(agg)) {
    stop("In `rw_scen_aggregate()`, `agg` must be a `rwd_agg` object.")
  }
  
  if (length(scen_dir) != 1) {
    stop("`scen_dir` should only have a length of 1.", call. = FALSE)
  }
  scen_dir <- normalizePath(scen_dir, mustWork = TRUE)
  
  nans_are <- match.arg(nans_are, choices = c("0", "error"))
  
  check_scen_rdf_paths(scenarios, scen_dir, agg)
  
  # check that file is correct type if it's specified
  if (!missing(file)) {
    check_rw_agg_file(file)
  }

  scenarios <- get_scen_names(scenarios, scen_names)
}

get_scen_names <- function(scenarios, scen_names)
{
  # if scenarios have names, then scen_names should not be specified
  if (!is.null(names(scenarios)) && !missing(scen_names)) {
    stop(
      "In `rw_scen_aggregate()`, `scenarios` have `names()`, so `scen_names` should not be specified.",
      call. = FALSE
    )
  }
  
  if (is.null(names(scenarios)) && !missing(scen_names)) {
    names(scenarios) <- scen_names
  }
  
  if (!is.null(names(scenarios))) {
    if (any(names(scenarios) %in% "")) {
      repI <- which(names(scenarios) %in% "")
      names(scenarios)[repI] <- scenarios[repI]
    }
  } else if (is.null(names(scenarios)) && missing(scen_names)) {
    names(scenarios) <- scenarios
  }
  
  scenarios
}

check_scen_rdf_paths <- function(scenarios, scen_dir, agg)
{
  # check all scenario paths
  scen_paths <- file.path(scen_dir, scenarios)
  if (! all(dir.exists(scen_paths))) {
    stop(
      "The following scenario directories do not exist:\n",
      paste(scen_paths[!dir.exists(scen_paths)], collapse = "\n"),
      call. = FALSE
    )
  }
  
  # check that all rdfs exist
  rdfs <- expand.grid(scen_paths, unique(agg$file))
  rdfs <- apply(rdfs, 1, paste, collapse = "/")
  rdfs_exist <- file.exists(rdfs)
  if (!all(rdfs_exist)) {
    stop(
      "The following rdf files do not exist:\n",
      paste(rdfs[!rdfs_exist], collapse = "\n"),
      call. = FALSE
    )
  }
}

check_rw_agg_file <- function(file)
{
  if (length(file) != 1) {
    stop("In `rw_scen_aggregate()`, `file` should have a length of 1.")
  }
  
  if (!(tools::file_ext(file) %in% c("csv", "feather", "txt"))) {
    stop(
      "In `rw_scen_aggregate()`, `file` should have a .csv, .feather, or .txt extension.", 
      call. = FALSE
    )
  }
  
  if (!dir.exists(dirname(file))) {
    stop("In `rw_scen_aggregate()`, `file` should point to a valid location.")
  }
  
  invisible(file)
}
