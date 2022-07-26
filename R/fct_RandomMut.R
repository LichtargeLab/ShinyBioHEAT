#' RandomMut
#'
#' @description Generate random mutations on the reference genome and output vcf.
#' tibble file. The accession ID from Genbank file. Is used to generate #CHROM column in the
#' vcf file.
#'
#' @param ref_table The reference table for the reference genome. Should contain these columns:
#' start, end. Each row represents one gene/intergene.
#' @param DNA_seq The DNA seq of the reference genome. Stored as vector, so the nucleotide
#' at position i can be indexed as DNA_seq[i].
#' @param n Number of simulated mutations
#' @param ti Number of transition mutations in the simulation. A integer between 0 and n.
#' @param cds_only If is TRUE, only mutations in the cds regions will be simulated. Default is TRUE.
#' @param replace True or False. Whether a same nucleotide position can be mutated multiple times. Set to FALSE by default.
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
RandomMut <- function(ref_table, DNA_seq, ID = "Genome", n, ti = n%/%2, cds_only = TRUE, replace = FALSE) {
  # Pick positions to be mutated
  if (cds_only == TRUE) {
    cds.ref <- ref_table %>%
      dplyr::select(start, end) %>%
      dplyr::mutate(range = purrr::map2(start, end, seq))
    # Generate a vector that stores all the nucleotide positions in the cds
    cds.pos <- cds.ref[["range"]] %>%
      unlist() %>%
      unique()
    POS <- sample(x = 1:length(cds.pos), size = n, replace = replace) %>%
      cds.pos[.] %>%
      sort()
    rm(cds.pos)
  } else {
    POS <- sample(x = 1:length(DNA_seq), size = n, replace = replace) %>%
      sort()
  }
  POS.ti <- sample(x = POS, size = ti, replace = FALSE)
  output <- dplyr::tibble(`#CHROM` = ID,
                   POS = POS) %>%
    dplyr::mutate(ID = "") %>%
    dplyr::mutate(titv = ifelse(POS%in%POS.ti, "ti", "tv")) %>%
    # Get REF nucleotide
    dplyr::mutate(REF = DNA_seq[POS]) %>%
    dplyr::mutate(ALT = purrr::map2_chr(REF, titv, ~BaseSwap(REF = .x, titv = .y)),
           QUAL = "",
           FILTER = "") %>%
    dplyr::select(`#CHROM`, POS, REF, ALT, QUAL, FILTER, INFO = titv)
  return(output)
}

