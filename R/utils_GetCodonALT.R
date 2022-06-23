#' GetCodonALT
#'
#' @description Combine mutations to get the mutated version of the codon
#'
#' @return A character that stores the mutated version of the codon
#'
#' @noRd
GetCodonALT <- function(codon.ref.v, codon.pos.v, ALT.v) {
  codon.alt <- unique(codon.ref.v)
  for (i in 1:length(codon.pos.v)){
    stringr::str_sub(codon.alt, start = codon.pos.v[i],
                     end = codon.pos.v[i]) <- ALT.v[i]
  }
  return(codon.alt)
}
