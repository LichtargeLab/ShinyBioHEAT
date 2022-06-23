#' R2
#'
#' @description Calculate R square for a linear model
#'
#' @return The R square value
#'
#' @noRd
R2 <- function(m) {
  gl <- length(fitted(m)) - 1
  ssq_total <- var((fitted(m) + resid(m))) * gl
  ssq_resid <- sum(resid(m)^2)
  r2 <- (ssq_total - ssq_resid)/ssq_total
  r2 <- round(r2,4)
  return(r2)
}
