#' Coerce lists, matrices, and data.frames to RiverWare data aggregators 
#' ([rwd_agg])
#' 
#' S3 generic for coercing from lists, matrices, and data.frames to [rwd_agg] 
#' objects.
#' 
#' @export
as_rwd_agg <- function(x, ...) 
{
  UseMethod("as_rwd_agg")
}

#' @export
#' @rdname as_rwd_agg
as_rwd_agg.data.frame <- function(x, ...)
{
  validate_rwd_agg(new_rwd_agg(x))
}

#' @export
#' @rdname as_rwd_agg
as_rwd_agg.list <- function(x, ...)
{
  if (length(unique(lengths(x))) != 1) {
    stop("When attempting to coerce a `list` to a `rwd_agg`, all entries must have the same length.")
  } else {
    as_rwd_agg(as.data.frame(x, stringsAsFactors = FALSE, ...))
  }
}

#' @export
#' @rdname as_rwd_agg
as_rwd_agg.matrix <- function(x, ...)
{
  as_rwd_agg(as.data.frame(x, stringsAsFactors = FALSE, ...))
}

#' @export
#' @rdname as_rwd_agg
as_rwd_agg.default <- function(x, ...)
{
  as_rwd_agg(as.data.frame(x, stringsAsFactors = FALSE, ...))
}

#' @export
#' @rdname as_rwd_agg
#' @usage NULL
as.rwd_agg <- function(x, ...) {
  UseMethod("as_rwd_agg")
}
