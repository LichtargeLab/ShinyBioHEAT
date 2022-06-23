#' GetAAPOS
#'
#' Get AA position for nucleotide position in a genome
#' @description Calculate AA position based on the nucleotide position (from VCF), start position, end position and strand.
#' @param POS nucleotide position referencing to the genome
#' @param start start nucleotide position of the query protein
#' @param end end nucleotide position of the query protein
#' @param strand + or -
#'
#' @return AA position
#'
#' @noRd
GetAAPOS <- function(POS, start, end, strand) {
  if (is.na(strand)) output <- NA
  else if (strand == "+") output <- (POS - start) %/% 3 + 1
  else if (strand == "-") output <- (end - POS) %/% 3 + 1
  else output <- NA
  return(output)
}
