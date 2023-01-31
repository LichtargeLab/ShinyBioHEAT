#' Get AA position for nucleotide position in a genome
#' @description Calculate AA position based on the nucleotide position (from VCF),
#' start position, end position and strand. The function is vectorized
#' @author Chen Wang
#' @param POS A vector of nucleotide position referencing to the genome
#' @param start A vector of start nucleotide position of the query protein
#' @param end A vector of end nucleotide position of the query protein
#' @param strand + or -. Vector.
#' @return AA position
#'
#' @noRd
GetAAPOS <- function(POS, start, end, strand) {
  if (sum(!strand %in% c("+", "-")) > 0) {
    stop("only use '+' or '-' in strand")
  }
  strand <- strand == "+"
  output <- ((POS - start)*strand + (end - POS)*(1-strand)) %/% 3 + 1
  return(output)
}
