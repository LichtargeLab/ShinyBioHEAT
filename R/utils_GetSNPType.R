#' GetSNPType
#'
#' @description Classify SNP type for given AA_ref and AA_alt combinations
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
GetSNPType <- function(AA_ref, AA_alt) {
  output <- rep("nonsynonymous", length(AA_ref))
  output[is.na(AA_alt)] <- "intergenic"
  output[AA_ref == AA_alt] <- "synonymous"
  output[(AA_alt %in% c("*", "X")) & (!AA_ref %in% c("*", "X"))] <- "nonsense"
  return(output)
}
