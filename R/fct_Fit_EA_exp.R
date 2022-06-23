#' Fit_EA_exp
#'
#' @description Bin EA scores into 10 bins, then perform exponential fit (linear and none
#' linear)
#' @param EA A numeric vector of EA scores.
#' @return A tibble with model parameters.
#'
#' @noRd
Fit_EA_exp <- function(EA) {
  fit_data <- dplyr::tibble(EA) %>%
    dplyr::mutate(bin = (EA - 0.000001) %/% 10 * 10 + 5) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::ungroup()
  EA_lm <- lm(log(count)~bin, data = fit_data)
  a_start <- exp(EA_lm$coefficients[1])
  b_start <- -EA_lm$coefficients[2]
  EA_nlm <- purrr::possibly(nls, otherwise = NA)(count ~ a * exp(b * -bin), start = list(a = a_start, b = b_start),
                                           data = fit_data, control = nls.control(maxiter = 6000))
  if(purrr::is_list(EA_nlm) == TRUE) {
    output <- dplyr::tibble(model = c("linear", "nonlinear"),
                            A = c(a_start, coef(EA_nlm)[[1]]),
                            lambda = c(b_start, coef(EA_nlm)[[2]]),
                            R2 = c(R2(EA_lm), R2(EA_nlm)))
  } else {
    output <- dplyr::tibble(model = c("linear", "nonlinear"),
                            A = c(a_start, NA),
                            lambda = c(b_start, NA),
                            R2 = c(R2(EA_lm), NA))
  }
  return(output)
}
