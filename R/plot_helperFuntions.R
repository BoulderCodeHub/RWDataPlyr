# -----------------------------------------------------------------------------
#   							myBoxStatsMinMax
# -----------------------------------------------------------------------------
# used for custome statistics in boxplot using ggplot2
myBoxStatsMinMax <- function(x) 
{
  r <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# -----------------------------------------------------------------------------
# 								myBoxStats1090
# -----------------------------------------------------------------------------
# used for custome statistics in boxplot using ggplot2
myBoxStats1090 <- function(x) 
{
  r <- quantile(x, probs = c(.1, 0.25, 0.5, 0.75, .9))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}