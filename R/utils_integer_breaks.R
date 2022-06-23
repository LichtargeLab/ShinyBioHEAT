#' integer_breaks
#'
#' @description Produce integer breaks for ggplot2
#'
#' @return Numeric vector that can be used as breaks in ggplot2
#'
#' @noRd
integer_breaks <- function(x) {
  unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))
}
