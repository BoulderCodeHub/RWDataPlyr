#' Create a `rwd_agg` template
#' 
#' `rwd_agg_template` creates a template csv file to use to create a RiverWare 
#' data aggregator ([rwd_agg]). 
#' 
#' @param file The file name to use for the template
#' @param path The path to create the template at
#' @param examples Boolean; When `FALSE` (default), the template includes only 
#'   headers. When `TRUE`, the template will include several examples of 
#'   specifying how each slot should be summarized. 
#'   
#' @examples 
#' 
#' rwd_agg_template()
#' rwd_agg_template(examples = TRUE)
#' rwd_agg_template(file = "rwa_keyslots.csv")
#' 
#' @seealso [read_rwd_agg()]

rwd_agg_template <- function(file = "rwa.csv", path = ".", examples = FALSE)
{
  if (length(file) != 1 || length(path) != 1 || length(examples) != 1)
    stop(
      "`rdf_agg_template()` expects all parameters to have length == 1", 
      call. = FALSE
    )
  
  if (!is.character(file) || !is.character(path))
    stop(
      "`rdf_agg_template()` expects `file` and `path` to be characers", 
      call. = FALSE
    )
  
  if (!is.logical(examples) || (is.logical(examples) && is.na(examples)))
    stop(
      "`rdf_agg_template()` expects `examples` to be TRUE or FALSE",
      call. = FALSE
    )
  
  if (tools::file_ext(file) != "csv")
    stop("`rdf_agg_template()` expects `file` to be a .csv file", call. = FALSE)
  
  if (examples) {
    x <- setNames(data.frame(matrix( 
        c("KeySlots.rdf", "Mead.Pool Elevation", "asis", NA, NA, NA, "powellPe", 
        "KeySlots.rdf", "Powell.Outflow", "wy", "sum", "<", 8230000, "pwylt823",
        "KeySlots.rdf", "Powell.Outflow", "July", NA, NA, 0.001, "pjulrel",
        "KeySlots.rdf", "Powell.Outflow", "wy", "min", NA, NA, "pminrel",
        "KeySlots.rdf", "Powell.Outflow", "djf", "max", ">=", 600000, "pwinmax"
        ),
        nrow = 5, 
        byrow = TRUE
      )), 
      c("file", "slot", "period", "summary" , "eval", "t_s", "variable")
    )
    
  } else {
    x <- setNames(
      data.frame(
        matrix(ncol = 7, nrow = 0),
        stringsAsFactors = FALSE
      ), 
      c("file", "slot", "period", "summary" , "eval", "t_s", "variable")
    )
  }
    
  write.csv(x, file.path(path, file), row.names = FALSE)
}

