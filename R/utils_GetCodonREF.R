#' GetCodonREF
#'
#' @description Get the reference of the mutated codon
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
GetCodonREF <- function(AA.POS, strand, DNA) {
  # Genes in the - strand are already reversed complemented in the
  # gbk tibble
  output <- stringr::str_sub(DNA, AA.POS*3-2, AA.POS*3)
  output[is.na(strand)] <- NA
  return(output)
}
