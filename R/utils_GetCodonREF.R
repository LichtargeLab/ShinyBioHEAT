#' GetCodonREF
#'
#' @description Get the reference of the mutated codon
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
GetCodonREF <- function(AA.POS, strand, DNA) {
  if (is.na(strand)) output <- NA
  output <- stringr::str_sub(DNA, AA.POS*3-2, AA.POS*3)
  return(output)
}
