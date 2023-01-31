#' AnnotateMutations
#'
#' @description Annotate mutations from VCF file
#'
#' @return A tibble with annotated mutations
#'
#' @noRd
AnnotateMutations <- function(vcf, ref_df = MG1655_ref, genome_map = MG1655_genome_map) {
  mut.list <- vcf %>%
    dplyr::select(POS, REF, ALT, INFO) %>%
    dplyr::filter(nchar(REF) == 1 & nchar(ALT) == 1) %>%
    dplyr::mutate(id = genome_map[POS]) %>%
    tidyr::unnest(cols = c(id)) %>%
    dplyr::left_join(ref_df, by = "id") %>%
    dplyr::arrange(POS, locus_tag) %>%
    dplyr::mutate(SNP_type = "")

  if (nrow(mut.list) > 0) {
    cds.output <- dplyr::filter(mut.list, CDS == TRUE) %>%
      dplyr::mutate(AA_pos = GetAAPOS(POS, start, end, strand)) %>%
      dplyr::mutate(codon_pos = GetCodonPOS(POS, start, end, strand)) %>%
      dplyr::mutate(codon_ref = GetCodonREF(AA_pos, strand, ref_df$DNA[id])) %>%
      dplyr::mutate(AA_ref = codon_table[codon_ref]) %>%
      dplyr::mutate(ALT.1 = ifelse(strand == "+",
                                   ALT,
                                   DNA_pair[ALT])) %>%
      dplyr::group_by(locus_tag, AA_pos) %>%
      dplyr::mutate(codon_alt = GetCodonALT(codon_ref, codon_pos, ALT.1)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(AA_alt = codon_table[codon_alt]) %>%
      dplyr::mutate(SUB = paste0(AA_ref, AA_pos, AA_alt)) %>%
      dplyr::mutate(SNP_type = GetSNPType(AA_ref, AA_alt))
    output <- dplyr::filter(mut.list, CDS == FALSE) %>%
      dplyr::mutate(SNP_type = ifelse(stringr::str_detect(gene, "intergenic"),
                                      "intergenic",
                                      "non coding genes")) %>%
      dplyr::mutate(SNP_type = as.character(SNP_type)) %>%
      dplyr::bind_rows(., cds.output) %>%
      dplyr::select(POS, REF, ALT, gene, locus_tag, CDS, SNP_type, SUB, AA_pos, AA_ref, AA_alt, codon_pos, codon_ref, codon_alt, INFO)
  } else {
    output <- dplyr::tibble(POS = double(), REF = character(), ALT = character(),
                            gene = character(), locus_tag = character(), CDS = logical(),
                            SNP_type = character(), SUB = character(), AA_pos = double(),
                            AA_ref = character(), AA_alt = character(), codon_pos = double(),
                            codon_ref = character(), codon_alt = character(), INFO = character())
  }
  return(output)
}
