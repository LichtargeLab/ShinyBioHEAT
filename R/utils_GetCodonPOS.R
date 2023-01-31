#' Get the codon substitution position
#' @description Get codon mutated position
#' @param POS A vector of nucleotide position referencing to the genome
#' @param start A vector of start nucleotide position of the query protein
#' @param end A vector of end nucleotide position of the query protein
#' @param strand + or -. Vector.
#' @return Codon positions
#'
#' @noRd
GetCodonPOS <- function(POS, start, end, strand) {
  if (sum(!strand %in% c("+", "-")) > 0) {
    stop("only use '+' or '-' in strand")
  }
  strand <- strand == "+"
  output <- ((POS - start + 1)*strand + (end - POS + 1)*(1-strand)) %% 3
  output[output == 0] <- 3
  return(output)
}
