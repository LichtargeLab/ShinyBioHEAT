#' Get transcript position for givien mutations
#' @description Calculate transcript position based on the nucleotide position (from VCF),
#' start position, end position and strand. The function is vectorized
#' @author Chen Wang
#' @param POS A vector of nucleotide position referencing to the genome
#' @param start A vector of start nucleotide position of the query protein
#' @param end A vector of end nucleotide position of the query protein
#' @param strand + or -. Vector.
#' @param DNA_pad DNA padding for ribosomal slippage
#' @return transcript position
#' @noRd
GetTranscriptPOS <- function(POS, start, end, strand, DNA_pad) {
  if (sum(!strand %in% c("+", "-")) > 0) {
    stop("only use '+' or '-' in strand")
  }
  strand <- strand == "+"
  output <- ((POS - start)*strand + (end - POS)*(1-strand)) + DNA_pad + 1
  return(output)
}
