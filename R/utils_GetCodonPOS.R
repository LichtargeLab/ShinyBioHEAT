#' GetCodonPOS
#'
#' @description Get codon mutated position
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
GetCodonPOS <- function(POS, start, end, strand) {
  if (is.na(strand)) output <- NA
  else if (strand == "+") {
    output <- (POS - start + 1) %% 3
    if (output == 0) output = 3
  }
  else if (strand == "-") {
    output <- (end - POS + 1) %% 3
    if (output == 0) output = 3
  }
  else output <- NA
  return(output)
}
