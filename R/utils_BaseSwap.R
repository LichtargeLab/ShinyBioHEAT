#' BaseSwap
#'
#' @description Randomly swap a nucleotide.
#'
#' @return A mutated nucleotides.
#'
#' @noRd
BaseSwap <- function(REF, titv = c("both", "ti", "tv")) {
  titv <- match.arg(titv)
  bases <- c("A", "T", "G", "C")
  # Transition substitution for all 4 nucleotides
  ti_ref <- c("A", "T", "G", "C")
  names(ti_ref) <- c("G", "C", "A", "T")
  if (titv == "both") {
    possi.alt <- bases[-which(bases == REF)]
    ALT <- sample(possi.alt, size = 1)
  } else if (titv == "ti") {
    ALT <- ti_ref[REF] %>%
      as.vector()
  } else {
    possi.alt <- bases[-which(bases == REF)]
    possi.alt <- possi.alt[-which(possi.alt == ti_ref[REF])]
    ALT <- sample(possi.alt, size = 1)
  }
  return(ALT)
}
