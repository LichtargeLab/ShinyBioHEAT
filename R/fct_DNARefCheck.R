#' DNARefCheck
#'
#' @description A function to check if REFs in the VCF file match the reference MG1655
#' genome.
#'
#' @param POS Vectorized positions in the VCF file
#' @param REF Vectorized REF in the VCF file
#' @param ref_seq Vectorized reference genome
#'
#' @return A logic vector.
#'
#' @noRd
DNARefCheck <- function(POS, REF, ref_seq) {
  gbk.REF <- ref_seq[POS]
  REF.check <- REF == gbk.REF
  return(REF.check)
}
